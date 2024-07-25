import pandas as pd
import numpy as np
from scipy import sparse as sp
from scipy.sparse.linalg import norm
import sklearn.preprocessing as pp


class Recommender_CF_UU:
    UU = {} ## user-user similarities; constructed lazily

    def create_Ratings_Matrix(self):

        self.item_ids = self.ratings['item'].unique()
        self.item_ids.sort()
        self.user_ids = self.ratings['user'].unique()
        self.user_ids.sort()
        self.m = self.user_ids.size

        ## user_id and item_id are external ids; u_id and i_id are internal ids
        self.item_id_to_i_id = dict(zip(self.item_ids, range(0, self.item_ids.size)))
        self.i_id_to_item_id = dict(zip(range(0, self.item_ids.size), self.item_ids))

        self.user_id_to_u_id = dict(zip(self.user_ids, range(0, self.user_ids.size )))
        self.u_id_to_user_id = dict(zip(range(0, self.user_ids.size), self.user_ids))

        self.R = sp.csr_matrix((self.ratings['rating'], (self.ratings['user'].map(self.user_id_to_u_id), self.ratings['item'].map(self.item_id_to_i_id))))
        self.R_dok = self.R.todok()


    def compute_user_avgs(self):
        user_sums = self.R.sum(axis=1).A1 ## matrix converted to 1-D array via .A1
        self.user_cnts = (self.R != 0).sum(axis=1).A1
        self.user_avgs = user_sums / self.user_cnts

    def compute_pairwise_user_similarity(self, u_id, v_id):

        u = self.R[u_id,:].copy()
        v = self.R[v_id,:].copy()

        ### BEGIN SOLUTION
        
        u.data = u.data - np.mean(u.data)
        v.data = v.data - np.mean(v.data)
    
        numerator = u.dot(v.T)
        numerator = numerator.A.item()

        denominator = norm(u)*norm(v)

        if denominator == 0:
            similarity = 0.;
        else:
            similarity = numerator/denominator
        

        ### END SOLUTION
        return similarity

    def compute_user_similarities(self, u_id):
        if u_id in self.UU.keys(): ## persist
            return self.UU[u_id]

        uU = np.empty((self.m,))

        
        
        ########## BEGIN HERE ##########
        
        for user_id in range(self.R.shape[0]):
            similarity = self.compute_pairwise_user_similarity(u_id, user_id)
            uU[user_id] = similarity
        ##########  END HERE  ##########
        
        '''
        #Add the version without the for loop here if you have done it
        # """
        ########## BEGIN BONUS ##########
        
        
        
        #########  END BONUS  ##########
        '''
        self.UU[u_id] = uU

        return self.UU[u_id]


    def create_user_neighborhood(self, u_id, i_id):
        nh = {} ## the neighborhood dict with (user id: similarity) entries
        ## nh should not contain u_id and only include users that have rated i_id; there should be at most k neighbors
        self.compute_user_similarities(u_id)
        uU = self.UU[u_id].copy()

        uU_copy = uU.copy() ## so that we can modify it, but also keep the original

        
        ########## BEGIN HERE ##########
        '''
        v_id = []
        for user_id in range(self.R.shape[0]):
            if ((user_id, i_id) in self.R_dok) and (user_id!=u_id):
                v_id.append(user_id)
        v_id = np.array(v_id)
        sim = uU_copy[v_id]

        if self.with_abs_sim == True:
            sim = np.absolute(sim)

        sort_id = np.argsort(sim)[::-1]

        nh = dict(zip(v_id[sort_id[0:self.k]],sim[sort_id[0:self.k]]))
        
'''
        if self.with_abs_sim:
            uU_copy = np.abs(uU_copy)
        index = np.argsort(uU_copy)[::-1]
        index = index[index != u_id]

        j = 0
        for i in index:
            if j >= self.k:
                break
            if (i, i_id) in self.R_dok:
                nh[i] = uU[i]
                j = j+1
        ##########  END HERE  ##########
        
        return nh


    def predict_rating(self, u_id, i_id):

#         if (u_id, i_id) in self.R_dok:
#             print("user", u_id, "has rated item", i_id, "with", self.R[u_id, i_id])
#         else:
#             print("user", u_id, "has not rated item", i_id)
#         print("k:", self.k, "with_deviations:", self.with_deviations, "with_abs_sim:", self.with_abs_sim)


        nh = self.create_user_neighborhood(u_id, i_id)

        neighborhood_weighted_avg = 0.

        ########## BEGIN HERE ##########
        '''
        keys = list(nh.keys())
        values = list(nh.values())

        if self.with_deviations:
            sum_scores = sum(values*(self.R[keys, i_id].data-self.user_avgs[keys]))
            sum_weights = sum(np.absolute(values))
        else:
            sum_scores = sum(values*self.R[keys, i_id].data)
            sum_weights = sum(np.absolute(values))
         '''
        index = list(nh.keys())
        v = self.R[index, i_id].copy()

        if self.with_deviations:
            v.data = v.data - self.user_avgs[index]

        # vals are the similarity scores
        vals = np.fromiter(nh.values(), dtype='float')

        sum_scores = sum(v.data * vals)
        sum_weights = sum(np.abs(vals))
        ##########  END HERE  ##########


        if sum_weights != 0: ## avoid division by zero
            neighborhood_weighted_avg = sum_scores/sum_weights


        if self.with_deviations:
            prediction = self.user_avgs[u_id] + neighborhood_weighted_avg
#             print(f'prediction {prediction:.4f} (user_avg {user_avgs[u_id]:.4f} offset {neighborhood_weighted_avg:.4f})')
        else:
            prediction = neighborhood_weighted_avg
#             print(f'prediction {prediction:.4f} (user_avg {user_avgs[u_id]:.4f})')

        return prediction


    def __init__(self, with_abs_sim = True, with_deviations = True, k = 50):
        self.with_abs_sim = with_abs_sim
        self.with_deviations= with_deviations
        self.k = k


    def build_model(self, ratings, items_meta = None):
        self.ratings = ratings

        self.create_Ratings_Matrix()
        self.compute_user_avgs()


    def recommend(self, user_id, from_item_ids=None, topN=20):

        item_ids_rated_by_user_id = self.ratings[self.ratings['user'] == user_id]['item'].tolist()

        u_id = self.user_id_to_u_id[user_id]

        recommendations = []

        if from_item_ids is None:
            from_item_ids = self.item_ids

        for item_id in from_item_ids:
            if item_id in item_ids_rated_by_user_id:
                continue
            i_id = self.item_id_to_i_id[item_id]
            rating = self.predict_rating(u_id, i_id)
            recommendations.append((item_id, rating))

        recommendations = sorted(recommendations, key=lambda x: -x[1])[:topN]

        return [item_id for item_id, rating in recommendations]
