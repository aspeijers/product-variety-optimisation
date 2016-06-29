##################################################################################################
######## Description: Evaluate clusters based on how well they predict the product market share (by sales)
########              a customized cross-validation function
######## input: clustering_2.RData, master_mktshare.RData 
######## output: cluster_eval_mse.RData
##################################################################################################


library(data.table)
library(plyr)


################################################################################
################################################################################
# calculate the MSE of the mkt_share_product_sales that is calculated by averaging over all the stores in the same cluster
# carry out for each of the clusters

# This function calculates the MSE using a specific test and train set.

mkt_MSE = function(train_table, test_table, cluster_table) {
    
    # add the clustering to the train and test data 
    train_table = merge(train_table, cluster_table, by = "storeID", all.x = TRUE)
    test_table = merge(test_table, cluster_table, by = "storeID", all.x = TRUE)
    
    # average the product market share by sell-in over each of the clusters in train
    train_product = train_table[, .(mkt_product_store_sales_predicted= mean(mkt_product_store_sales)), by =.(cluster,productID) ]
    
    # merge to test 
    test_table = merge(test_table, train_product, by=c("cluster","productID"), all.x = TRUE)    
    
    # calculate MSE
    mse_vector = sum((test_table$mkt_product_store_sales- test_table$mkt_product_store_sales_predicted)^2)/nrow(test_table)
    
    return(mse_vector) 
}




################################################################################
################################################################################
# the CV_represent function 
# This function splits the master table into a representative test and train set
# then calculates MSE using 100-fold CV. It calls the function mkt_MSE. 

# Function returns: name of best clustering, average_MSE for all clusterings, 
# and number of NA's that were included in the MSE average for each clustering. 

CV_represent = function(master_table, number_of_CV, clusters_df){
    ## initialize empty list to keep the mse's
    mse_list = list()
    
    # create new column in master table that concatenates storeID and productID to merge on
    master_table <- master_table[,storeprodID:=paste0(storeID, productID)]
    
    ## Loop through the different train and test data  
    for (i in 1:number_of_CV) {
        
        cat("CV", i)
        
        # make sure that all test storeIDs already exist in train data
        none_new_stores = FALSE
        while (none_new_stores == FALSE) {
            
            # split into test and train by (storeID, productID), using representative sampling method (for stores)
            test <- ddply(master_table, .(chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 15)), ]})
            test <- as.data.table(test)
            
            train_rows  <- setdiff( master_table[,storeprodID], test[,storeprodID] )
            train       <- master_table[storeprodID %in% train_rows]
            rm(train_rows)
            
            # remove extra column and master table
            test[,storeprodID:=NULL]
            train[,storeprodID:=NULL]
            
            # check if all stores in test are present in train
            none_new_stores = sum( unique(test$storeID) %in% unique(train$storeID) ) == length(unique(test$storeID))
        }
        
        
        ## In the particular train and test, loop through the different clusters 
        # store the mse cluster values in a matrix - initialize it 
        mse = c()
        
        # vector containing the row names of the above matrix  
        rnames = c()
        
        for(j in 2:ncol(clusters_df)) {
            
            cat("clusters column ", j, "\n")
            
            #isolate cluster
            current_cluster = clusters_df[,c(1,j)]
            names(current_cluster)[2] = "cluster"
            
            #calculate mse using the above function
            new_mse = mkt_MSE(train_table = train, test_table = test, cluster_table = current_cluster) 
            
            #append to the mse matrix
            mse = rbind(mse, new_mse)
            rnames = c(rnames, names(clusters_df)[j])
        }
        
        # name the rows of mse indicating which is which cluster
        rownames(mse) = rnames 
        
        #save it in the list
        mse_list[[i]] = as.data.frame(mse)
        
#         if (sum(is.na(mse_list[[i]])) != 0 ){
#             print('NAs occurring')
#             return( list(train=train, test=test, mse_list = mse_list[[i]]) )
#             break
#         }
    }

    ### Average the results over each of the data splits (ie over each fold)
    combined_MSE = mse_list[[1]]
    for (l in 2:length(mse_list)) {
        combined_MSE = cbind(combined_MSE, mse_list[[l]])
    }
    
    average_MSE = rowMeans(combined_MSE, na.rm = TRUE)
    
    # number of NA's for each clustering
    no_NAs <- apply( combined_MSE, 1, function(x) sum(is.na(x)) )
    
    # discover which cluster has lowest mse
    cl = which.min(average_MSE)
    
    return(list( best_clustering = names(clusters_df)[cl+1], 
                 average_MSE = average_MSE, 
                 no_NAs = no_NAs) )
}



################################################################################
####################### Evaluate the clustering ################################

## load the tables for evaluation
#clusters = readRDS("cluster.RData")
clusters <- readRDS("clusterings_2.RData")
master = readRDS("master_mktshare.RData")

set.seed(1234)
results = CV_represent(master = master, number_of_CV = 100, clusters_df = clusters)

# save results
saveRDS(results, "cluster_eval_mse.RData")


# troubleshooting - problem is that we can still get NA's if the train/test split 
# works such that not all products exist in each cluster in train.
# can try to avoid this by making the test set smaller. 
# another option is to just ignore the NA's in the averageing. However, it is important 
# to note the no. that are used in the averaging. (eg 90 NA's in a 100-fold CV is a lot)

