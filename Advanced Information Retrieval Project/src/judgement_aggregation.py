# implement part 1 here

"""
The flow of our approach is as follows:
- ignore document-query pairs with only one relevance label (def filter())
- identify contradictory document-query relevance labels and exclude these (def is_contradictory())
- calculate the ratio per user of provided relevanceCharacterRanges / total number of provided relevance labels
- assign weights based on above ratio and aggregate final relevance label by document-query pairs
"""

import pandas as pd

df = pd.read_csv("Part-1\\fira-22.judgements-anonymized.tsv", sep="\t", encoding='latin-1')

def is_contradictory(judgements):
    mapping_relevance = {
    '0_NOT_RELEVANT': 0,
    '1_TOPIC_RELEVANT_DOES_NOT_ANSWER': 0,
    '2_GOOD_ANSWER': 1,
    '3_PERFECT_ANSWER': 1
    }
    judgements_binary = list(judgements['relevanceLevel'].map(mapping_relevance))
    return int(0 in judgements_binary and 1 in judgements_binary)

flag_is_contradictory = df.groupby(by = ['queryId', 'documentId']).apply(lambda x: is_contradictory(x)).reset_index()
flag_is_contradictory.columns = ['queryId', 'documentId', 'contradictory']

def filter(ds, flag):
    ds = ds.groupby(['documentId', 'queryId']).filter(lambda x: len(x) > 1)
    ds = ds.merge(flag_is_contradictory, on=['queryId', 'documentId']).query('contradictory == 0')

    return ds

flt_df = filter(df, flag_is_contradictory)

def label_aggregation(ds):
    # Define the mapping for relevanceLevel
    mapping = {
        '0_NOT_RELEVANT': 0,
        '1_TOPIC_RELEVANT_DOES_NOT_ANSWER': 1,
        '2_GOOD_ANSWER': 2,
        '3_PERFECT_ANSWER': 3
    }

    # Replace relevanceLevel with mapped values
    ds['relevanceLevel'] = ds['relevanceLevel'].map(mapping)

    # Group by userId and Range Category and count occurrences
    all_no_ranges = ds.groupby('queryId')['relevanceCharacterRanges'].apply(
        lambda x: (x == '<no ranges selected>').all())
    filtered_query_ids = all_no_ranges[~all_no_ranges].index
    filtered_df = ds[ds['queryId'].isin(filtered_query_ids)]
    filtered_df = filtered_df[filtered_df['relevanceLevel'].isin([2, 3])]
    filtered_df['Range Category'] = filtered_df['relevanceCharacterRanges'].apply(
        lambda x: 'No Ranges Selected' if x == '<no ranges selected>' else 'Ranges Selected')
    grouped = filtered_df.groupby(['userId', 'Range Category']).size().unstack(fill_value=0)
    grouped['Ratio'] = grouped['Ranges Selected'] / (grouped['Ranges Selected'] + grouped['No Ranges Selected'])

    # Merge the ratio back to the judgements DataFrame
    ds = ds.merge(grouped[['Ratio']], on='userId')

    # Calculate the weight based on the Ratio
    ds['weight'] = ds['Ratio'].apply(lambda x: 2 if x >= 2 / 3 else 0.5 if x < 1 / 3 else 1)

    # Function to calculate the weighted average
    def weighted_avg(group):
        return round((group['relevanceLevel'] * group['weight']).sum() / group['weight'].sum())

    # Group by 'documentId' and 'queryId' to calculate the weighted average relevance level
    weighted_judgements = ds.groupby(['documentId', 'queryId']).apply(weighted_avg).reset_index(
        name='weightedRelevanceLevel')

    return weighted_judgements
