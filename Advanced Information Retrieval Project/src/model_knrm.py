from typing import Dict
import torch
import torch.nn as nn
from allennlp.modules.text_field_embedders import TextFieldEmbedder
from allennlp.modules.matrix_attention.cosine_matrix_attention import CosineMatrixAttention

class KNRM(nn.Module):
    '''
    Paper: End-to-End Neural Ad-hoc Ranking with Kernel Pooling, Xiong et al., SIGIR'17
    '''

    def __init__(self,
                 word_embeddings: TextFieldEmbedder,
                 n_kernels: int):

        super(KNRM, self).__init__()

        self.word_embeddings = word_embeddings

        # static - kernel size & magnitude variables
        mu = torch.FloatTensor(self.kernel_mus(n_kernels)).view(1, 1, 1, n_kernels)
        sigma = torch.FloatTensor(self.kernel_sigmas(n_kernels)).view(1, 1, 1, n_kernels)

        self.register_buffer('mu', mu)
        self.register_buffer('sigma', sigma)

        self.cosine_module = CosineMatrixAttention()

        # Initialize the linear layer for combining the kernel outputs
        self.dense = nn.Linear(n_kernels, 1)

        nn.init.uniform_(self.dense.weight, -0.014, 0.014)

    def forward(self, query: Dict[str, torch.Tensor], document: Dict[str, torch.Tensor]) -> torch.Tensor:
        # prepare embedding tensors & paddings masks
        query_pad_oov_mask = (query["tokens"]["tokens"] > 0).float()
        document_pad_oov_mask = (document["tokens"]["tokens"] > 0).float()

        query_embeddings = self.word_embeddings(query)
        document_embeddings = self.word_embeddings(document)

        # compute the kernelized cross-match features
        kernel_scores = self.calculate_kernel_scores(query_embeddings, document_embeddings, query_pad_oov_mask, document_pad_oov_mask)

        per_kernel_query = torch.sum(kernel_scores, dim=2)

        # Take the logarithm of the soft-TF scores and apply a mask to exclude padding values
        log_per_kernel_query = torch.log(torch.clamp(per_kernel_query, min=1e-10)) * 0.01
        log_per_kernel_query_masked = log_per_kernel_query * query_pad_oov_mask.unsqueeze(-1)

        # Sum the soft-TF scores for each kernel
        per_kernel = torch.sum(log_per_kernel_query_masked, dim=1)

        # Combine the kernel outputs using the linear layer
        output = self.dense(per_kernel).squeeze(1)

        return output
    
    def calculate_kernel_scores(self, query_embeddings, document_embeddings, query_pad_oov_mask, document_pad_oov_mask):
        # Create query-by-document mask
        query_by_doc_mask = torch.einsum("bi,bj->bij", query_pad_oov_mask, document_pad_oov_mask)

        # Compute cosine similarity matrix
        cosine_matrix = self.cosine_module(query_embeddings, document_embeddings)

        # Apply the query-by-document mask to the cosine similarity matrix
        cosine_matrix_masked = cosine_matrix * query_by_doc_mask

        # Compute the kernelized cross-match features
        similarity_scores = torch.exp(-torch.pow(cosine_matrix_masked.unsqueeze(-1) - self.mu, 2) / (2 * torch.pow(self.sigma, 2)))

        # Apply the query-by-document mask to the similarity scores
        kernel_scores = similarity_scores * query_by_doc_mask.unsqueeze(-1)

        return kernel_scores

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
