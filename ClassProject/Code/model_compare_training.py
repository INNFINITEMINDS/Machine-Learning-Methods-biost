import os
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LassoCV
from sklearn.linear_model import Lasso
from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV
from sklearn.model_selection import cross_val_predict
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn.metrics import explained_variance_score
from sklearn.cross_decomposition import PLSRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.svm import SVR
import pandas as pd
import numpy as np
import random
import pickle

# a find gene list function that accept a Dnase position, a df_RNA file, a df_gene_info data frame, and a distance number, and then
# return the list of gene names for prediction and the matrix of gene expression
def find_gene_list(dnase_position, df_gene_info, distance):
    dnase_ls = dnase_position.split("-")
    chr_id = dnase_ls[0]
    chr_start = int(dnase_ls[1])
    chr_end = int(dnase_ls[2])
    
    # define the interval for gene selection
    gene_chr = chr_id.replace("chr", "")
    gene_start = chr_start - distance
    gene_end = chr_end + distance
    #print(gene_chr, gene_start, gene_end)
    # find the gene list for the gene interval
    df_gene_filter = df_gene_info[df_gene_info["chr"] == gene_chr]
    df_gene_filter = df_gene_filter[df_gene_filter["start"] >= gene_start]
    df_gene_filter = df_gene_filter[df_gene_filter["start"] <= gene_end]
    gene_ls = df_gene_filter.index
    return(gene_ls)


# a combined data set function, that accept a df_RNA data set and a df_dnase data set and then combine the two data set,
# and return the columns for the RNA genes and Dnase set
def combine_RNA_dnase(df_rna, df_dnase):
    df_rna["tissue"] = df_rna.index
    df_dnase["tissue"] = df_dnase.index
    df_combine = pd.merge(df_rna, df_dnase, on="tissue")
    df_combine.index = df_rna.index
    gene_list = df_rna.loc[:, df_rna.columns != "tissue"].columns
    dnase_list = df_dnase.loc[:, df_dnase.columns != "tissue"].columns
    return (df_combine, gene_list, dnase_list)

# Here I define a function that accept a Dnase id, df_Dnase and df_RNA combined data set
# , df_gene_info, a distance for computing nearest genes and return the X matrix and Y vector.

def data_matrix(Dnase_id, df_combine, df_gene_info, distance):
    
    # find the nearest gene list
    nearest_genes = list(find_gene_list(Dnase_id,df_gene_info, distance))
    
    # extract and return the df_dnase vector and df_rna matrix
    pred_y = df_combine[Dnase_id]
    pred_x = df_combine[nearest_genes]
    return((pred_x, pred_y))

# define a function that return cross validated MSE using Lasso
def mse_lasso(pred_x, pred_y):
    lasso  = LassoCV(alphas = np.logspace(-5, 5, 1000), cv=10) 
    lasso.fit(np.array(pred_x), np.array(pred_y))
    best_alpha=lasso.alpha_
    lasso = Lasso(alpha=best_alpha)
    pred = cross_val_predict(lasso, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
    #print("score: ", np.mean(cross_val_score(lasso, np.array(pred_x), np.array(pred_y), cv = 10, scoring = "mean_squared_error")))
    return(mean_squared_error(pred_y, pred))

# define a function that return cross validated MSE using Ridge
def mse_ridge(pred_x, pred_y):
    ridge  = RidgeCV(alphas = np.logspace(-5, 5, 100), cv=10) 
    ridge.fit(np.array(pred_x), np.array(pred_y))
    best_alpha=ridge.alpha_
    ridge = Ridge(alpha=best_alpha)
    pred = cross_val_predict(ridge, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
    return(mean_squared_error(pred_y, pred))

# define a function that return cross validated MSE using PLS
def mse_pls(pred_x, pred_y):
    max_com = min([pred_x.shape[1] + 1, 6])
    comps = list(range(1, max_com))
    mse_ls = []
    for n in comps:
        pls = PLSRegression(n_components = n)
        pred = cross_val_predict(pls, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
        mse = mean_squared_error(pred_y, pred)
        mse_ls.append(mse)
    
    # return the lowest mse
    return(np.min(mse_ls))

# define a function that return mse based on random forest
def mse_random_forest(pred_x, pred_y):
    rf = RandomForestRegressor(n_estimators = 500, max_features = 0.3, n_jobs=-1)
    pred = cross_val_predict(rf, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
    return(mean_squared_error(pred_y, pred))

# define a function that return mse based on boosting
def mse_boosting(pred_x, pred_y):
    tree_n = list(range(1, 20))
    mse_ls = []
    for n in tree_n:
        bs = GradientBoostingRegressor(n_estimators=n, 
                                       learning_rate=0.1, max_depth=1, random_state=0, loss='ls')
        pred = cross_val_predict(bs, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
        mse_ls.append(mean_squared_error(pred_y, pred))
    return(np.min(mse_ls))

# define a function that return mse based on support vector machine
def mse_svr(pred_x, pred_y):
    C_s = list(range(1, 1000, 100))
    mse_ls = []
    for c in C_s:
        
        '''
        svr = SVR(kernel='rbf', C=c, gamma=0.1)
        pred = cross_val_predict(svr, np.array(pred_x), np.array(pred_y), cv = 10)
        mse_ls.append(mean_squared_error(pred_y, pred))
        
        svr =  SVR(kernel='linear', C=c)
        pred = cross_val_predict(svr, np.array(pred_x), np.array(pred_y), cv = 10)
        mse_ls.append(mean_squared_error(pred_y, pred))
        
        svr = SVR(kernel='poly', C=c, degree=2)
        pred = cross_val_predict(svr, np.array(pred_x), np.array(pred_y), cv = 10)
        mse_ls.append(mean_squared_error(pred_y, pred))
        '''
        svr = SVR(kernel='rbf', C=c, gamma=0.1)
        pred = cross_val_predict(svr, np.array(pred_x), np.array(pred_y), cv = 10, n_jobs=-1)
        mse_ls.append(mean_squared_error(pred_y, pred))
    
    return(np.min(mse_ls))

# this function return a mse based on average prediction
def mse_aver(pred_x, pred_y):
    return(np.mean((pred_y -  np.mean(pred_y))**2))

# The function accept a list of Dnase site, and then compute the mse score for different 
# prediction models, and return a data frame for the score for different models
def mse_models(dnase_list, df_combine, df_gene_info, distance, output_folder):
    
    # define the mse list for different models
    lasso = []
    ridge = []
    pls = []
    rf = []
    bs = []
    svr = []
    ave = []
    # for each dnase site, record the mse of the model
    for dnase_id in dnase_list:
        print("Start calculating Dnase site: ", dnase_id)
        pred_x, pred_y = data_matrix(dnase_id, df_combine, df_gene_info, distance)
        
        # there are nearby genes
        if pred_x.shape[1] != 0:
            print("Calculate Lasso mse...")
            lasso.append(mse_lasso(pred_x, pred_y))
            print("Calculate Ridge mse...")
            ridge.append(mse_ridge(pred_x, pred_y))
            print("Calculate PLS mse...")
            pls.append(mse_pls(pred_x, pred_y))
            print("Calculate random forest mse...")
            rf.append(mse_random_forest(pred_x, pred_y))
            print("Calculate boosting mse...")
            bs.append(mse_boosting(pred_x, pred_y))
            print("Calculate svr mse...")
            svr.append(mse_svr(pred_x, pred_y))
            print("Calculate the average mse...")
            ave.append(mse_aver(pred_x, pred_y))
        
        # if there are no nearby genes, then use the average score for prediction
        else:
            mse_ave = np.mean((pred_y -  np.mean(pred_y))**2)
            lasso.append(mse_ave)
            ridge.append(mse_ave)
            pls.append(mse_ave)
            rf.append(mse_ave)
            bs.append(mse_ave)
            svr.append(mse_ave)
            ave.append(mse_ave)
            
    result = pd.DataFrame({"lasso" : lasso, 
                          "ridge" : ridge,
                          "pls" : pls,
                          "rf" : rf,
                          "bs" : bs,
                          "svr" : svr,
                          "ave" : ave})
    
    folder = output_folder
    if not os.path.exists(folder):
        os.makedirs(folder)
        
    result.to_csv(folder + "/compare_mse_models.csv")
    return(0)

# here I am going to sample 1000 dnase site and then compute the mse with different scores

# load the data
df_rna = pd.read_hdf("./processed_data/rna_scaled.hdf")
df_dnase = pd.read_hdf("./processed_data/dnase_scaled.hdf")
df_gene_info = pd.read_hdf("./processed_data/df_gene_info.hdf")
output_folder = "./result/result_training"

df_combine, gene_list, dnase_list = combine_RNA_dnase(df_rna, df_dnase)
sample_number = 100
distance = 500000

dnase_list_sample = random.sample(list(dnase_list), sample_number)

train_list = random.sample(list(df_combine.index), 16)

test_list = list(set(list(df_combine.index)).difference(set(train_list)))

dnase_list_file = output_folder + "/dnase_list.pickle"
train_list_file = output_folder + "/train_list.pickle"
test_list_file = output_folder + "/test_list.pickle"
data_file = output_folder + "/combined_data.pickle"

with open(dnase_list_file, "wb") as fp:
    pickle.dump(dnase_list_sample, fp)

with open(train_list_file, "wb") as fp:
    pickle.dump(train_list, fp)
    
with open(test_list_file, "wb") as fp:
    pickle.dump(test_list, fp)

with open(data_file, "wb") as fp:
    pickle.dump(df_combine, fp)
print("Tissue used for training: ", train_list)
print("Tissue used for testing: ", test_list)
mse_models(dnase_list_sample, df_combine.loc[train_list,:], df_gene_info, distance, output_folder)