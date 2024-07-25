# Group-40 Team Information

12143812 Lovasz Enrico

11921651 S Tóth Marcell

12223236 Vakalopoulos Konstantinos

# Report

## Part 1 - Test Collection Preparation
In the first part, we explored the ways of aggregating the raw judgments into labels. The raw judgments of the data contain anonymized information about user judgments for query-passage pairs. These judgments are used to evaluate the relevance or quality of a passage in response to a given query. Judgments contain some additional information such as user-id, the time it took to annotate, the time of annotation, the judgment value, and the optional selection of text. We aim to find a more sophisticated method of aggregating raw judgment to usable and robust labels in comparison to the given baseline majority voting. The baseline method assumes that all annotators are equally reliable and that all errors are completely random, which is usually not the case.

When annotating query-document pairs many different options for aggregating labels from multiple users exist. No single strategy is universally suitable for all cases; the best approach depends on the specific use case and domain. Methods for aggregation include majority voting, weighted voting based on annotator expertise, or advanced techniques like Bayesian models to handle annotator reliability. Each method has its strengths and weaknesses, and the choice should consider factors such as the variability of annotations, the criticality of accuracy, and the characteristics of the domain in question.

At the beginning we considered the median (instead of mean) for the aggregation of the labels as it is a robust estimator. Here, we observed for quite some cases that the aggregated label outcome is questionable (e.g., if you have 3 votes: 0_NOT_RELEVANT, 1_TOPIC_RELEVANT_DOES_NOT_ANSWER, 2_GOOD_ANSWER). The aggregation is not really conclusive and questionable. Hence, after an extensive discussion within the group we aligned on some guiding principles in our study. Firstly, we do not want to overengineer the aggregation logic by introducing highly sophisticated strategies. Mainly because we do not have too much domain knowledge (in the field of information retrieval and specifically for the provided data) for sophistication, but also to keep it transparent and comprehensible. Secondly, we want to use reliable data as much as possible in the training process, i.e., we prefer to have non-contradictory labels of at least two users within our aggregation. Thirdly, we want to use as many data as possible in the training process.

The main hypothesis that we pursued is that not all annotators are equally reliable in their labelling, i.e., some annotators are more precise (in reading / understanding the context) than others and therefore deliver (on average) more correct labels. As these annotators deliver more correct labels they are more reliable and therefore should receive higher voting power in the aggregation strategy. In order to operationalize this we finally used the information provided in column relevanceCharacterRanges of table judgements-anonymized. We argue that annotators who not only provide a label but additionally provide a range of relevant characters (at least for labels 2_GOOD_ANSWER and 3_PERFECT_ANSWER) are more reliable.

After conducting descriptive statistics about the different data sources we focused on identifying reliable annotators. We had two approaches to define reliable annotators. For both cases we first identified query-document pairs with contradictory labelling. For this purpose we defined a binary indicator with 0 when relevanceLevel is in '0_NOT_RELEVANT' or '1_TOPIC_RELEVANT_DOES_NOT_ANSWER' and 1 otherwise. A query-document pair labelling is contradictory if one annotator's indicator is 0 (or 1) and at least for one other annotator this indicator is 1 (or 0). 

To judge if any given annotator is reliable we reviewed the ratio per user of having a contradictory vote compared with total user's vote. Users with higher ratio are more reliable than users with low ratio. Alternatively, we reviewed the ratio of provided relevanceCharacterRanges divided by the total number of provided labels per user. Annotators with a high ratio (>2/3) of provided relevance character ranges received higher weights (2x), user with low ratio (<1/3) received lower weights (0.5x) and the rest get a standard weight (1x). We decided us for the latter approach as we in a first step excluded all query-document pairs with contradictory voting and then used an additional dimension of information, i.e., relevanceCharacterRanges, to judge the reliability of the users. In this way we utilized two dimensions (contradictory of voting and provided relevanceCharacterRanges) to derive the aggregated label. We believe that this yields a higher probability of correct label.

The details concerning our process and implementation are described in the notebook "User_weighted.ipynb". The final aggregation strategy is implemented in judgement_aggregation.py. The analysis of the aggregated outcome and meta-judgement of the raw-judgements is provided in "Final_aggregation.ipynb".



## Part 2 - Neural Re-Ranking
In this part, we were first required to implement two re-ranking models, KNRM and TK. The whole implementation of the two re-ranking models is based on the two papers provided by the instructions of the project. 

The Kernel-Based Neural Ranking Model (KNRM) is a simple model that does not use any transformer layers and therefore has relatively few learning parameters. The process begins with encoding, where the queries and documents are converted into embeddings. Next, a match matrix is calculated using the cosine similarity between the query and document embeddings. This match matrix is then transformed using the RBF kernel function. The output of the RBF kernel is summed and fed into a feed-forward neural network to calculate the final score between the query and the document. The score, which is the output of the feed-forward neural network, is the weighted sum of its input, which is the output of the RBF kernel.

The TK-model (Transformer-Kernel ranking model) is a more complex alternative to the KNRM model. It is an extension of the KNRM model and it is considered an effective model that integrates transformers with kernel-pooling. This model operates as follows: it begins with an encoding layer that uses transformers to independently perform the encoding, including the positional information of the words. In the matching phase, similar to KNRM, it employs cosine similarity for term-by-term interactions, creating a single match matrix. Finally, again similar to KNRM, kernel pooling is applied to count the match matrix interactions, which are then summed up and weighted in the final scoring layer.

The two models can be found in the ./src folder of this project where each of the respective files contains more detailed comments of the difficult parts of the model. In order to train and get results from these models, the ‘re-ranking.ipynb’ has to be utilized and run. Before starting to explain the training and evaluation process, it is worth mentioning that the models explained above have been trained using NVIDIA GeForce RTX 2060 6 GB.

### Training - Validation
As mentioned earlier, all the training, validation, and testing for both models were performed in re_ranking.ipynb, by just modifying the "config" dictionary. This training was conducted using the triples.train.tsv, which is approximately 1.8 GB in size. To avoid excessively long training times for our models, a stopping criterion was used. Specifically, every 200 iterations, the currently trained model is evaluated using msmarco_tuples.validation.tsv. If the MRR@10 does not change for five consecutive evaluations, the process breaks from the current epoch and moves on to the next one.

At this point, it is worth noting that the number of epochs used for training the models was 3. Initially, the template of the re_ranking.ipynb file included only 2 epochs. However, this was changed to 10, with the rationale that this would allow for more training of our models. Nonetheless, it was observed in both models that after 3 epochs, all metrics during evaluation exhibited a downward trend. For this reason, only 3 epochs were used for training our models.


### Evaluation
Below is presented all the results from the KNRM and the TK model. As shown, the models were first tested on the MS-MARCO dataset, then on the FIRA 2022 dataset with the baseline labels, and finally again on the FIRA 2022 dataset with the labels from part 1 of the project. 

As for the MS-MARCO dataset, where it says 'Best result on validation set,' this means that these were the best results achieved by our models during training/validation. As can be seen from the results below, the TK model is significantly better and more effective than the KNRM model, achieving an MRR@10 of 0.242, compared to only 0.131 for KNRM (almost double the difference). Of course, it should be noted that the TK model is also more complex than the KNRM model, resulting in the training of the TK model taking much longer—approximately 3 times longer—than the KNRM model.

Regarding the FIRA dataset with baseline and part 1 implementation labels, the differences between the models are relatively small. Two things should be given special attention. First, the results from the labels in Part 1 are particularly satisfactory, as they approach about 75% in each metric (TK model). It is logical, of course, that we do not reach the baseline values, because in the data provided by the exercise (**fira-2022.tuples.tsv** and **fira-2022.baseline-qrels.tsv**), we have not only the same number of (query id document id) pairs but also exactly the same combination of (query id document id) pairs. This does not happen with our labels from Part 1, as our labels include (query id document id) pairs that do not exist in the **fira-2022.tuples.tsv** dataset. As a result, the performance of our models using the labels from Part 1 is lower compared to the baseline labels.


#### KNRM Results
**MS-MARCO**
```
Best result on validation set:
	MRR@10: 0.146, MRR@20: 0.154, nDCG@10: 0.183, nDCG@20: 0.214
Test set:
	MRR@10: 0.131, MRR@20: 0.141, nDCG@10: 0.169, nDCG@20: 0.205
```
**FIRA + Baseline Labels**
```
FiRA-2022 baseline labels:
	MRR@10: 0.95, MRR@20: 0.95, nDCG@10: 0.894, nDCG@20: 0.908
```
**FIRA + Labels from Part 1**
```
FiRA-2022 labels from part 1:
	MRR@10: 0.727, MRR@20: 0.727, nDCG@10: 0.746, nDCG@20: 0.767
```

#### TK Results
**MS-MARCO**
```
Best result on validation set:
	MRR@10: 0.251, MRR@20: 0.256, nDCG@10: 0.301, nDCG@20: 0.319
Test set:
	MRR@10: 0.242, MRR@20: 0.248, nDCG@10: 0.291, nDCG@20: 0.314
```
**FIRA + Baseline Labels**
```
FiRA-2022 baseline labels:
	MRR@10: 0.958, MRR@20: 0.958, nDCG@10: 0.902, nDCG@20: 0.917
```
**FIRA + Labels from Part 1**
```
FiRA-2022 labels from part 1:
	MRR@10: 0.75, MRR@20: 0.75, nDCG@10: 0.766, nDCG@20: 0.786
```
## Part 3 - Extractive QA
In this part we deal with extractive question-answer algorithm. The goal is given a query and a passage/document, the algorithm should select the words in the passage that answer the query. For this we used a pre-trained extractive QA model from the HuggingFace model hub and prepared our extractive QA pipeline. From the large amount of available algorithms we have selected tinyroberta-squad2 (https://huggingface.co/deepset/tinyroberta-squad2). This is the distilled version of the deepset/roberta-base-squad2 model. It should have a comparable prediction quality and runs at twice the speed of the base model. Due to the speed of the model and appropriate memory consumption we were able to run the whole analysis on our local machines. The exact procedure and code is stored in the file "extractive_qa.ipynb".

As our TK model produced better results in part 2, it was selected for our re-ranking phase in this part of the exercise. We took a snapshot of the best model obtained from the last section's evaluation phase and implemented some snippets to transform the MSMARCO test set into an input file compatible with the fira.qrels.qa-tuples.tsv data set format. During the mapping of document to reference answer, we observed that our re-ranking procedure may not be optimal, since for quite a few documents no reference answers could be obtained. As described in the assignment we only evaluate the overlap of pairs that are in the result and the qrels.

The following table presents the average F1 and exact scores as well as their standard deviations for both QA models. There are several important remarks to be made:

- the sizes of the testsets differ significantly due to the recommended procedure to compile the testset
- taking into consideration the standard deviations of the F1 score and the exact matches (both being quite high) the outcome in both data sets is comparable to each other

| Model | Test-Set | Testset Size | F1 | Exact | No Reference |
|-------------:|----:|----:|------:|------:|----:|
| distilled QA         | FIRA qa-tuples                    | 52606 | 0.371 (+- 0.306)   | 0.093 (+- 0.291)  | -     |
| TK + distilled QA    | MSMARCO testset + FIRA answers    | 2000  | 0.418 (+- 0.309)   | 0.113 (+- 0.316)  | 1111  | 

In the official eval script on HuggingFace the model achieved an F1 score of 81.2. Our performance value here is certainly lower, but we also did not fully fine-tune our setup (due to resource contrains). Therefore in general, it can be observed that the distilled QA model works quite well out-of-the-box (although we do not have any baseline here). 

## Appendix - Allocation of work
The work load was split as follows:

Enrico Lovasz: descriptive and explorative analysis of raw judgements and final aggregation, extractive QA analysis, writing of respective parts in the report

Marcell S Tóth: KNRM model implementation and execution, training and validation code in the re_ranking.ipynb file, early stopping criterion, test set evaluation on the MS-MARCO data set

Konstantinos Vakalopoulos: descriptive and explorative analysis of raw judgements, TK model implementation and execution, test set evaluation on MSMARCO and FiRA-2022 data set using the baseline and from Part 1 labels, writing of respective parts in the report
