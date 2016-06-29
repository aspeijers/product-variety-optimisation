##### Evaluate clusters
setwd("~/BGSE/semester3/kernel/data")

library(data.table)
library(plyr)



###################################################################################################################################
################### MSE FUNCTIONS #################################################################################################


mkt_MSE = function(train,test, cluster,avg_flavor,avg_groups){
  ### add the clustering to the train and test data 
  cluster$storeID = as.integer(as.character(cluster$storeID))
  train = merge(train, cluster, by = "storeID",all.x = TRUE)
  test = merge(test,cluster, by = "storeID", all.x = TRUE)
  rm(cluster)
  
  ### w.r.t. product 
  print("product")
  avg_variables = c("mkt_product_store","mkt_product_store_sales")
  train_product = train[,lapply(.SD,mean), by =.(cluster,productID),.SDcols = avg_variables ]
  names(train_product)[3:ncol(train_product)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_product,by=c("cluster","productID"), all.x = TRUE)
  rm(train_product)
  
  
  #### w.r.t. subFam
  print("subFam")
  avg_variables = c("mkt_subFam_store","mkt_subFam_store_sales")
  train_subFam = train[,lapply(.SD,mean), by =.(cluster,subFam),.SDcols = avg_variables ]
  names(train_subFam)[3:ncol(train_subFam)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_subFam,by=c("cluster","subFam"), all.x = TRUE)
  rm(train_subFam)
  
  ### w.r.t. fam
  print("fam")
  avg_variables = c("mkt_Fam_store","mkt_Fam_store_sales")
  train_fam = train[,lapply(.SD,mean), by =.(cluster,fam),.SDcols = avg_variables ]
  names(train_fam)[3:ncol(train_fam)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_fam,by=c("cluster","fam"), all.x = TRUE)
  rm(train_fam)
  
  
  ### w.r.t. grup 
  print("grup")
  
  #subset for the ones which are not in avering part
  temp = train[!(grup%in%avg_groups)]
  train_grup_not_average = temp[,.(mkt_Grup_store_predict=unique(mkt_Grup_store),
                                    mkt_Grup_store_sales_predict=mean(mkt_Grup_store_sales)), 
                                 by = .(storeID,grup)]
  rm(temp)
  

  #merge to test 
  test = merge(test,train_grup_not_average,by=c("storeID","grup"), all.x = TRUE)
  rm(train_grup_not_average)
  ############################################################################
  # Getting rid of the NAs in the test data by averiging w.r.t. clusters
  # add store_grup variable to train, test
  train[,cluster_grup:=paste0(cluster,grup)]
  test[,cluster_grup:=paste0(cluster,grup)]
  
  # subset training data for grup that needs to be averaged over all stores in a particular cluster
  cluster_grup_na = unique(test[is.na(mkt_Grup_store_predict),]$cluster_grup)
  temp = train[cluster_grup %in% cluster_grup_na]
  train_grup_average = unique(temp[,.(cluster_grup,mkt_Grup_store_predict=mean(mkt_Grup_store),
                               mkt_Grup_store_sales_predict=mean(mkt_Grup_store_sales))
                            , by = .(cluster,grup)])
  #add storeIDs
  store_cluster_grup = unique(test[is.na(mkt_Grup_store_predict),.(cluster_grup,storeID)])
  #merge the calculated mkt based on cluster and grup
  store_cluster_grup = merge(store_cluster_grup,
                             train_grup_average[,.(cluster_grup,mkt_Grup_store_sales_predict,mkt_Grup_store_predict)], 
                             by ="cluster_grup",all.x = TRUE)
  #merge the averaging estimation for every store if mkt share is NA
  test[,mkt_Grup_store_predict:=ifelse(is.na(mkt_Grup_store_predict),
                                       store_cluster_grup[cluster_grup==cluster_grup,mkt_Grup_store_predict],
                                       mkt_Grup_store_predict)]
  
  test[,mkt_Grup_store_sales_predict:=ifelse(is.na(mkt_Grup_store_sales_predict),
                                       store_cluster_grup[cluster_grup==cluster_grup,mkt_Grup_store_sales_predict],
                                       mkt_Grup_store_sales_predict)]
  
  #remove auxiliary cluster
  rm(train_grup_average,store_cluster_grup,temp)

  #Null the auxiliary columns 
  train[,cluster_grup:=NULL]
  test[,cluster_grup:=NULL]
  
  
  
  ### w.r.t. flavor 
  print("flavor")

  #subset for the ones which are not in avering part
  train_flavor_not_average = train[,.(mkt_Flavor_store_predict=unique(mkt_Flavor_store),
                                   mkt_Flavor_store_sales_predict=mean(mkt_Flavor_store_sales)), 
                                by = .(storeID,flavor)]

  
  #merge to test 
  test = merge(test,train_flavor_not_average,by=c("storeID","flavor"), all.x = TRUE)
  rm(train_flavor_not_average)
  ############################################################################
  # Getting rid of the NAs in the test data by averiging w.r.t. clusters
  # add store_flavor variable to train, test
  train[,cluster_flavor:=paste0(cluster,flavor)]
  test[,cluster_flavor:=paste0(cluster,flavor)]
  
  # subset training data for flavor that needs to be averaged over all stores in a particular cluster
  cluster_flavor_na = unique(test[is.na(mkt_Flavor_store_predict),]$cluster_flavor)
  temp = train[cluster_flavor %in% cluster_flavor_na]
  train_flavor_average = unique(temp[,.(cluster_flavor,mkt_Flavor_store_predict=mean(mkt_Flavor_store),
                                      mkt_Flavor_store_sales_predict=mean(mkt_Flavor_store_sales))
                                   , by = .(cluster,flavor)])
  #add storeIDs
  store_cluster_flavor = unique(test[is.na(mkt_Flavor_store_predict),.(cluster_flavor,storeID)])
  #merge the calculated mkt based on cluster and flavor
  store_cluster_flavor = merge(store_cluster_flavor,
                             train_flavor_average[,.(cluster_flavor,mkt_Flavor_store_sales_predict,mkt_Flavor_store_predict)], 
                             by ="cluster_flavor",all.x = TRUE)
  #merge the averaging estimation for every store if mkt share is NA
  test[,mkt_Flavor_store_predict:=ifelse(is.na(mkt_Flavor_store_predict),
                                       store_cluster_flavor[cluster_flavor==cluster_flavor,mkt_Flavor_store_predict],
                                       mkt_Flavor_store_predict)]
  
  test[,mkt_Flavor_store_sales_predict:=ifelse(is.na(mkt_Flavor_store_sales_predict),
                                             store_cluster_flavor[cluster_flavor==cluster_flavor,mkt_Flavor_store_sales_predict],
                                             mkt_Flavor_store_sales_predict)]
  
  #remove auxiliary cluster
  rm(train_flavor_average,store_cluster_flavor,temp)
  
  #Null the auxiliary columns 
  train[,cluster_flavor:=NULL]
  test[,cluster_flavor:=NULL]
  
  ### w.r.t. units 
  print("units")

  #subset for the ones which are not in avering part
  train_units_not_average = train[,.(mkt_Units_store_predict=unique(mkt_Units_store),
                                   mkt_Units_store_sales_predict=mean(mkt_Units_store_sales)), 
                                by = .(storeID,units)]
  
  
  #merge to test 
  test = merge(test,train_units_not_average,by=c("storeID","units"), all.x = TRUE)
  rm(train_units_not_average)
  ############################################################################
  # Getting rid of the NAs in the test data by averiging w.r.t. clusters
  # add store_units variable to train, test
  train[,cluster_units:=paste0(cluster,units)]
  test[,cluster_units:=paste0(cluster,units)]
  
  # subset training data for units that needs to be averaged over all stores in a particular cluster
  cluster_units_na = unique(test[is.na(mkt_Units_store_predict),]$cluster_units)
  temp = train[cluster_units %in% cluster_units_na]
  train_units_average = unique(temp[,.(cluster_units,mkt_Units_store_predict=mean(mkt_Units_store),
                                      mkt_Units_store_sales_predict=mean(mkt_Units_store_sales))
                                   , by = .(cluster,units)])
  #add storeIDs
  store_cluster_units = unique(test[is.na(mkt_Units_store_predict),.(cluster_units,storeID)])
  #merge the calculated mkt based on cluster and units
  store_cluster_units = merge(store_cluster_units,
                             train_units_average[,.(cluster_units,mkt_Units_store_sales_predict,mkt_Units_store_predict)], 
                             by ="cluster_units",all.x = TRUE)
  #merge the averaging estimation for every store if mkt share is NA
  test[,mkt_Units_store_predict:=ifelse(is.na(mkt_Units_store_predict),
                                       store_cluster_units[cluster_units==cluster_units,mkt_Units_store_predict],
                                       mkt_Units_store_predict)]
  
  test[,mkt_Units_store_sales_predict:=ifelse(is.na(mkt_Units_store_sales_predict),
                                             store_cluster_units[cluster_units==cluster_units,mkt_Units_store_sales_predict],
                                             mkt_Units_store_sales_predict)]
  
  #remove auxiliary cluster
  rm(train_units_average,store_cluster_units,temp)
  
  #Null the auxiliary columns 
  train[,cluster_units:=NULL]
  test[,cluster_units:=NULL]
  
  # w.r.t. Type
  ### w.r.t. type 
  print("type")
  
  #subset for the ones which are not in avering part
  train_type_not_average = train[,.(mkt_Type_store_predict=unique(mkt_Type_store),
                                     mkt_Type_store_sales_predict=mean(mkt_Type_store_sales)), 
                                  by = .(storeID,type)]
  
  
  #merge to test 
  test = merge(test,train_type_not_average,by=c("storeID","type"), all.x = TRUE)
  rm(train_type_not_average)
  ############################################################################
  # Getting rid of the NAs in the test data by averiging w.r.t. clusters
  # add store_type variable to train, test
  train[,cluster_type:=paste0(cluster,type)]
  test[,cluster_type:=paste0(cluster,type)]
  
  # subset training data for type that needs to be averaged over all stores in a particular cluster
  cluster_type_na = unique(test[is.na(mkt_Type_store_predict),]$cluster_type)
  temp = train[cluster_type %in% cluster_type_na]
  train_type_average = unique(temp[,.(cluster_type,mkt_Type_store_predict=mean(mkt_Type_store),
                                       mkt_Type_store_sales_predict=mean(mkt_Type_store_sales))
                                    , by = .(cluster,type)])
  #add storeIDs
  store_cluster_type = unique(test[is.na(mkt_Type_store_predict),.(cluster_type,storeID)])
  #merge the calculated mkt based on cluster and type
  store_cluster_type = merge(store_cluster_type,
                              train_type_average[,.(cluster_type,mkt_Type_store_sales_predict,mkt_Type_store_predict)], 
                              by ="cluster_type",all.x = TRUE)
  #merge the averaging estimation for every store if mkt share is NA
  test[,mkt_Type_store_predict:=ifelse(is.na(mkt_Type_store_predict),
                                        store_cluster_type[cluster_type==cluster_type,mkt_Type_store_predict],
                                        mkt_Type_store_predict)]
  
  test[,mkt_Type_store_sales_predict:=ifelse(is.na(mkt_Type_store_sales_predict),
                                              store_cluster_type[cluster_type==cluster_type,mkt_Type_store_sales_predict],
                                              mkt_Type_store_sales_predict)]
  
  #remove auxiliary cluster
  rm(train_type_average,store_cluster_type,temp)
  
  #Null the auxiliary columns 
  train[,cluster_type:=NULL]
  test[,cluster_type:=NULL]
  
  
  
  
  ### Calculating MSE
  mse_vector = rep(NA,14)
  names(mse_vector) = names(test)[27:40]
  
  mse_vector[1] = sum((test$mkt_product_store - test$mkt_product_store_predict)^2)/nrow(test)
  mse_vector[2] = sum((test$mkt_subFam_store - test$mkt_subFam_store_predict)^2)/nrow(test)
  mse_vector[3] = sum((test$mkt_Fam_store - test$mkt_Fam_store_predict)^2)/nrow(test)
  mse_vector[4] = sum((test$mkt_Grup_store - test$mkt_Grup_store_predict)^2)/nrow(test)
  mse_vector[5] = sum((test$mkt_Flavor_store - test$mkt_Flavor_store_predict)^2)/nrow(test)
  mse_vector[6] = sum((test$mkt_Type_store - test$mkt_Type_store_predict)^2)/nrow(test)
  mse_vector[7] = sum((test$mkt_Units_store - test$mkt_Units_store_predict)^2)/nrow(test)
  mse_vector[8] = sum((test$mkt_product_store_sales- test$mkt_product_store_sales_predict)^2)/nrow(test)
  mse_vector[9] = sum((test$mkt_subFam_store_sales - test$mkt_subFam_store_sales_predict)^2)/nrow(test)
  mse_vector[10] = sum((test$mkt_Fam_store_sales - test$mkt_Fam_store_sales_predict)^2)/nrow(test)
  mse_vector[11] = sum((test$mkt_Grup_store_sales - test$mkt_Grup_store_sales_predict)^2)/nrow(test)
  mse_vector[12] = sum((test$mkt_Flavor_store_sales - test$mkt_Flavor_store_sales_predict)^2)/nrow(test)
  mse_vector[13] = sum((test$mkt_Type_store_sales - test$mkt_Type_store_sales_predict)^2)/nrow(test)
  mse_vector[14] = sum((test$mkt_Units_store_sales - test$mkt_Units_store_sales_predict)^2)/nrow(test)

 return(mse_vector) 
}

##############################################################################################################################
##############################################################################################################################

#the CV_represent function 

CV_represent = function(master, number_of_CV,clusters_df){
## initialize empty list to keep the mse's
mse_list = list()
## Loop through the different train and test data  
 for (i in 1:number_of_CV){
   cat("CV",i)
   master <- master[,storeprodID:=paste0(storeID, productID)]
   
   #make sure that all test storeIDs already exist in train data
   none_new_stores = FALSE
   while (none_new_stores == FALSE){
     
     # split into test and train by (storeID, productID), using representative sampling method (for stores)
     test    <- ddply(master, .(chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 20)), ]})
     test <- as.data.table(test)
     
     train_rows   <- setdiff(master[,storeprodID], test[,storeprodID])
     train <- master[storeprodID %in% train_rows]
     rm(train_rows)
     
     # remove extra column and master table
     test[,storeprodID:=NULL]
     train[,storeprodID:=NULL]
     #check if all stores in test are present in train
    none_new_stores = sum( unique(test$storeID) %in% unique(train$storeID) ) == length(unique(test$storeID))
   }
  
   
   # in the particular train and test, loop through the different clusters
   #we store the mse cluster values in a matrix - initialize it 
   mse = c()
   #vector containing the row names of the above matrix  
   rnames = c()
   for(j in 2:length(clusters_df)){
     cat("cluster",j)
     #isolate cluster
     cluster = clusters_df[,c(1,j)]
     names(cluster)[2] = "cluster"
     #calculate mse using the above function
     new_mse = mkt_MSE(train, test, cluster, avg_flavor, avg_groups)
     #append to the mse matrix
     mse = rbind(mse,new_mse)
     rnames = c(rnames, paste0("cluster",j))
   }
   #name the columns appropriately, indicating which mse we are taking 
   colnames(mse) = names(new_mse)
   rownames(mse) = rnames 
   #save it in the list
   mse_list[[i]] = as.data.frame(mse)
    
 }

### RESULTS 
average_MSE = mse_list[[1]]
for (l in 2:length(mse_list)){
  average_MSE = average_MSE + mse_list[[l]]
}

average_MSE = average_MSE/number_of_CV

#average_average_MSE = rowSums(average_MSE)/ncol(average_MSE)
cl = which.min(average_MSE$mkt_product_store_sales)
  
return(list( names(clusters_df)[cl], average_MSE))
}

######################################################################################################################################
####################### Evaluate the clustering ######################################################################################

#identify which grups has only 1 or 2 or 3 products 
products = readRDS("products.RData")
products = products[availability == "alltimer"]
group_statistics  = products[,.(num_products = length(productID)), by = grup]
avg_groups = c()
for(i in group_statistics$grup){
  if (group_statistics[grup==i,]$num_products == 1 |
      group_statistics[grup==i,]$num_products == 2 |
      group_statistics[grup==i,]$num_products == 3){
    avg_groups = c(avg_groups,i)
  }
}


#identify which flavor has only 1 or 2 or 3 products 
flavor_statistics  = products[,.(num_products = length(productID)), by = flavor]
avg_flavor= c()
for(i in flavor_statistics$flavor){
  if (flavor_statistics[flavor==i,]$num_products == 1 |
      flavor_statistics[flavor==i,]$num_products == 2 |
      flavor_statistics[flavor==i,]$num_products == 3){
    avg_flavor = c(avg_flavor,i)
  }
}

rm(products,flavor_statistics,group_statistics)



## load the tables for evaluation
clusters = readRDS("cluster.RData")
master = readRDS("master_mktshare.RData")

results = CV_represent(master = master,number_of_CV = 100, clusters_df = clusters)









