from typing import Dict, Iterator, List

import torch
import torch.nn as nn
from torch.autograd import Variable

from allennlp.modules.text_field_embedders import TextFieldEmbedder
from allennlp.modules.matrix_attention.cosine_matrix_attention import CosineMatrixAttention
import math


class TK(nn.Module):
    '''
    Paper: S. Hofstätter, M. Zlabinger, and A. Hanbury 2020. Interpretable & Time-Budget-Constrained Contextualization for Re-Ranking. In Proc. of ECAI 

    Based on:
        The paper
        https://github.com/sebastian-hofstaetter/matchmaker/blob/master/matchmaker/models/published/ecai20_tk.py
    The code has been adapted and comments have been added to demonstrate understanding. The function get_positional_features() 
    is used as it is written in the ecai20_tk.py file. he role of the function get_positional_features() is to add the positional 
    information of the words to the encoder layer. Additionally, it was observed that the final feedforward neural network (NN) 
    for the implementation of the weighted average was not included in the previous implementation. Therefore, this feedforward NN 
    has been added in the forward() function.
    '''

    def __init__(self,
                 word_embeddings: TextFieldEmbedder,
                 n_kernels: int,
                 n_layers:int,
                 n_tf_dim:int,
                 n_tf_heads:int):

        super(TK, self).__init__()

        self.word_embeddings = word_embeddings

        self.register_buffer("positional_features_q", self.get_positional_features(n_tf_dim,2000))
        self.register_buffer("positional_features_d", self.positional_features_q)

        # static - kernel size & magnitude variables
        if torch.cuda.is_available():
            mu = Variable(torch.cuda.FloatTensor(self.kernel_mus(n_kernels)), requires_grad=False) \
                .view(1, 1, 1, n_kernels)
            sigma = Variable(torch.cuda.FloatTensor(self.kernel_sigmas(n_kernels)), requires_grad=False) \
                .view(1, 1, 1, n_kernels)
        else:
            mu = Variable(torch.FloatTensor(self.kernel_mus(n_kernels)), requires_grad=False) \
                .view(1, 1, 1, n_kernels)
            sigma = Variable(torch.FloatTensor(self.kernel_sigmas(n_kernels)), requires_grad=False) \
                .view(1, 1, 1, n_kernels)
            
        self.register_buffer('mu', mu)
        self.register_buffer('sigma', sigma)

        #todo

        # This is the learning rate alpha in order to have the hybrid-contextualized representation 
        self.mixer = nn.Parameter(torch.full([1], 0.5, dtype=torch.float32, requires_grad=True))

        # This is for the W_log and W_len in order to produce a scalar, for both the log-normalized and length normalized kernels
        # The W_log and W_len are appeared in the paper in the equations (9)
        self.nn_scaler = nn.Parameter(torch.full([1], 0.01, dtype=torch.float32, requires_grad=True))

        # This is the Contextualized encoding, using the transformers in order to have the Contextualized Term Representation
        encoder_layer = nn.TransformerEncoderLayer(d_model=n_tf_dim, nhead=n_tf_heads, dim_feedforward=n_tf_dim, dropout=0)
        self.contextualizer = nn.TransformerEncoder(encoder_layer, n_layers, norm=None)


        # This is for the cosine similarity calculation between the query and document in order to create the single match-matrix M
        self.cosine_module = CosineMatrixAttention() 

        # The below NNs are for the final calculation of the score in order to find the appropriate weights
        # Bias is set to False because we do not want the bias term (according to the paper)
        # This is the last part of the TK model in order to calculate the final score
        # 3 Feed Forward NNs
        self.log_norm = nn.Linear(n_kernels, 1, bias=False)
        self.len_norm = nn.Linear(n_kernels, 1, bias=False)
        self.weighted_sum = nn.Linear(2, 1, bias=False)

        # Initialize the weights of the linear layer using a uniform distribution
        # This is a key factor in order to have good results when the TK model is trained
        torch.nn.init.uniform_(self.log_norm.weight, -0.014, 0.014) 
        torch.nn.init.uniform_(self.len_norm.weight, -0.014, 0.014) 


    def forward(self, query: Dict[str, torch.Tensor], document: Dict[str, torch.Tensor]) -> torch.Tensor:
        # pylint: disable=arguments-differ

        #
        # prepare embedding tensors & paddings masks
        # -------------------------------------------------------

        # shape: (batch, query_max)
        query_pad_oov_mask = (query["tokens"]["tokens"] > 0).float() # > 1 to also mask oov terms
        # shape: (batch, doc_max)
        document_pad_oov_mask = (document["tokens"]["tokens"] > 0).float()

        # shape: (batch, query_max,emb_dim)
        query_embeddings = self.word_embeddings(query)
        # shape: (batch, document_max,emb_dim)
        document_embeddings = self.word_embeddings(document)

        # Creation of the contextualized embeddings using:
        # 1. The learning rate (here is called mixer and in the paper is the alpha parameter)
        # 2. The non-contextualized embeddings with their maskings (query and document)
        # 3. The positional encodings
        query_embeddings = self.forward_representation(query_embeddings, query_pad_oov_mask,self.positional_features_q[:,:query_embeddings.shape[1],:])
        document_embeddings = self.forward_representation(document_embeddings, document_pad_oov_mask,self.positional_features_d[:,:document_embeddings.shape[1],:])

        query_by_doc_mask = torch.bmm(query_pad_oov_mask.unsqueeze(-1), document_pad_oov_mask.unsqueeze(-1).transpose(-1, -2))
        query_by_doc_mask_view = query_by_doc_mask.unsqueeze(-1)

        # Calculation of the cosine matrix M
        # This appears in the equation (4) from the paper
        cosine_matrix = self.cosine_module.forward(query_embeddings, document_embeddings)
        cosine_matrix_masked = cosine_matrix * query_by_doc_mask
        cosine_matrix_extradim = cosine_matrix_masked.unsqueeze(-1)

        # RBF kernels 
        # This function represents the equation (5) from the paper
        raw_kernel_results = torch.exp(- torch.pow(cosine_matrix_extradim - self.mu, 2) / (2 * torch.pow(self.sigma, 2)))
        kernel_results_masked = raw_kernel_results * query_by_doc_mask_view

        doc_lengths = torch.sum(document_pad_oov_mask, 1)

        # These 4 commands represent the equations (7) and (9) from the paper regarding the log normalization
        per_kernel_query = torch.sum(kernel_results_masked, 2)
        log_per_kernel_query = torch.log2(torch.clamp(per_kernel_query, min=1e-10)) * self.nn_scaler # here is the equation (9)
        log_per_kernel_query_masked = log_per_kernel_query * query_pad_oov_mask.unsqueeze(-1) # make sure we mask out padding values
        per_kernel = torch.sum(log_per_kernel_query_masked, 1) 

        # These 4 commands represent the equations (8) and (9) from the paper regarding the length normalization
        per_kernel_query_mean = per_kernel_query / (doc_lengths.view(-1,1,1) + 1) 
        log_per_kernel_query_mean = per_kernel_query_mean * self.nn_scaler # here is the equation (9)
        log_per_kernel_query_masked_mean = log_per_kernel_query_mean * query_pad_oov_mask.unsqueeze(-1) # make sure we mask out padding values
        per_kernel_mean = torch.sum(log_per_kernel_query_masked_mean, 1) 


        ##
        ## "Learning to rank" layer - connects kernels with learned weights
        ## -------------------------------------------------------

        # Here is the final calculation of the scores and constitute the final part of the TK model
        # 3 Feed Forward NNs are used 
        # The first and the second FF NNs are for the log and length normalized kernels
        # and the final FF NN is for the weighted sum of the two kernels
        dense_log = self.log_norm(per_kernel)
        dense_len_out = self.len_norm(per_kernel_mean)
        # Equation (10)
        dense_comb_out = self.weighted_sum(torch.cat([dense_log,dense_len_out],dim=1))
        score = torch.squeeze(dense_comb_out,1)   

        # Return the final score
        return score
    

    def forward_representation(self, sequence_embeddings: torch.Tensor, sequence_mask: torch.Tensor,positional_features=None) -> torch.Tensor:

        if positional_features is None:
            positional_features = self.positional_features_d[:,:sequence_embeddings.shape[1],:]

        # Here the transformer is used in order to create the embeddings using the word embeddings and the positional encodings
        sequence_embeddings_context = self.contextualizer((sequence_embeddings + positional_features).transpose(1,0),src_key_padding_mask=~sequence_mask.bool()).transpose(1,0)
        # Here the hybrid-contextualized represention is returned (in the paper is the t hat in the equation (1))
        return (self.mixer * sequence_embeddings + (1 - self.mixer) * sequence_embeddings_context)

    def get_positional_features(self,dimensions,
                                max_length,
                                min_timescale: float = 1.0,
                                max_timescale: float = 1.0e4):
        # pylint: disable=line-too-long
        """
        Implements the frequency-based positional encoding described
        in `Attention is all you Need
        <https://www.semanticscholar.org/paper/Attention-Is-All-You-Need-Vaswani-Shazeer/0737da0767d77606169cbf4187b83e1ab62f6077>`_ .
        Adds sinusoids of different frequencies to a ``Tensor``. A sinusoid of a
        different frequency and phase is added to each dimension of the input ``Tensor``.
        This allows the attention heads to use absolute and relative positions.
        The number of timescales is equal to hidden_dim / 2 within the range
        (min_timescale, max_timescale). For each timescale, the two sinusoidal
        signals sin(timestep / timescale) and cos(timestep / timescale) are
        generated and concatenated along the hidden_dim dimension.
        Parameters
        ----------
        tensor : ``torch.Tensor``
            a Tensor with shape (batch_size, timesteps, hidden_dim).
        min_timescale : ``float``, optional (default = 1.0)
            The smallest timescale to use.
        max_timescale : ``float``, optional (default = 1.0e4)
            The largest timescale to use.
        Returns
        -------
        The input tensor augmented with the sinusoidal frequencies.
        """
        timesteps=max_length
        hidden_dim = dimensions

        timestep_range = self.get_range_vector(timesteps, 0).data.float()
        # We're generating both cos and sin frequencies,
        # so half for each.
        num_timescales = hidden_dim // 2
        timescale_range = self.get_range_vector(num_timescales, 0).data.float()

        log_timescale_increments = math.log(float(max_timescale) / float(min_timescale)) / float(num_timescales - 1)
        inverse_timescales = min_timescale * torch.exp(timescale_range * -log_timescale_increments)

        # Broadcasted multiplication - shape (timesteps, num_timescales)
        scaled_time = timestep_range.unsqueeze(1) * inverse_timescales.unsqueeze(0)
        # shape (timesteps, 2 * num_timescales)
        sinusoids = torch.cat([torch.sin(scaled_time), torch.cos(scaled_time)], 1)
        if hidden_dim % 2 != 0:
            # if the number of dimensions is odd, the cos and sin
            # timescales had size (hidden_dim - 1) / 2, so we need
            # to add a row of zeros to make up the difference.
            sinusoids = torch.cat([sinusoids, sinusoids.new_zeros(timesteps, 1)], 1)
        return sinusoids.unsqueeze(0)
    
    def get_range_vector(self, size: int, device: int) -> torch.Tensor:
        """
        Returns a range vector with the desired size, starting at 0. The CUDA implementation
        is meant to avoid copy data from CPU to GPU.
        """
        # if device > -1:
        #     return torch.cuda.LongTensor(size, device=device).fill_(1).cumsum(0) - 1
        # else:
        return torch.arange(0, size, dtype=torch.long)  

    def kernel_mus(self, n_kernels: int):
        """
        get the mu for each guassian kernel. Mu is the middle of each bin
        :param n_kernels: number of kernels (including exact match). first one is exact match
        :return: l_mu, a list of mu.
        """
        l_mu = [1.0]
        if n_kernels == 1:
            return l_mu

        bin_size = 2.0 / (n_kernels - 1)  # score range from [-1, 1]
        l_mu.append(1 - bin_size / 2)  # mu: middle of the bin
        for i in range(1, n_kernels - 1):
            l_mu.append(l_mu[i] - bin_size)
        return l_mu

    def kernel_sigmas(self, n_kernels: int):
        """
        get sigmas for each guassian kernel.
        :param n_kernels: number of kernels (including exactmath.)
        :param lamb:
        :param use_exact:
        :return: l_sigma, a list of simga
        """
        bin_size = 2.0 / (n_kernels - 1)
        l_sigma = [0.0001]  # for exact match. small variance -> exact match
        if n_kernels == 1:
            return l_sigma

        l_sigma += [0.5 * bin_size] * (n_kernels - 1)
        return l_sigma
