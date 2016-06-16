# Evaluate clusters based on how well they predict the product market share (by sales)


setwd("~/BGSE/semester3/kernel/data")

library(data.table)
library(plyr)


################################################################################
################################################################################
# calculate the MSE of the mkt_share_product_sales that is calculated by averaging over all the stores in the same cluster
# carry out for each of the clusters

mkt_MSE = function(train_table, test_table, cluster_table) {
    
    # add the clustering to the train and test data 
    cluster_table$storeID = as.integer(as.character(cluster_table$storeID))
    train_table = merge(train_table, cluster_table, by = "storeID", all.x = TRUE)
    test_table = merge(test_table, cluster_table, by = "storeID", all.x = TRUE)
    
    # w.r.t. product 
    print("product")
    train_product = train_table[, .(mkt_product_store_sales_predicted= mean(mkt_product_store_sales)), by =.(cluster,productID) ]
    
    # merge to test 
    test_table = merge(test_table, train_product, by=c("cluster","productID"), all.x = TRUE)    
    
    # calculate MSE
    mse_vector = sum((test_table$mkt_product_store_sales- test_table$mkt_product_store_sales_predicted)^2)/nrow(test_table)
    
    return(mse_vector) 
}




################################################################################
################################################################################
#the CV_represent function 

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
            test <- ddply(master_table, .(chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 20)), ]})
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
            
            cat("cluster",j)
            
            #isolate cluster
            current_cluster = clusters_df[,c(1,j)]
            names(current_cluster)[2] = "cluster"
            
            #calculate mse using the above function
            new_mse = mkt_MSE(train_table = train, test_table = test, cluster_table = current_cluster) 
            
            #append to the mse matrix
            mse = rbind(mse, new_mse)
            rnames = c(rnames, paste0("cluster",j))
        }
        
        # name the rows of mse indicating which is which cluster
        rownames(mse) = rnames 
        
        #save it in the list
        mse_list[[i]] = as.data.frame(mse)
        
        if (sum(is.na(mse_list[[i]])) !=0 ){
            print('NAs occurring')
            return(train, test)
            break
        }
    }
    
    ### Average the results over each of the data splits (ie over each fold)
    average_MSE = mse_list[[1]]
    for (l in 2:length(mse_list)) {
        average_MSE = average_MSE + mse_list[[l]]
    }
    average_MSE = average_MSE/number_of_CV
    
    # discover which cluster has lowest mse
    cl = which.min(average_MSE[,1])
    
    return(list( names(clusters_df)[cl+1], average_MSE) )
}



################################################################################
####################### Evaluate the clustering ################################

## load the tables for evaluation
clusters = readRDS("cluster.RData")
master = readRDS("master_mktshare.RData")

set.seed(1234)
results = CV_represent(master = master, number_of_CV = 100, clusters_df = clusters)

# save results
saveRDS(results, "cluster_eval_mse.RData")
