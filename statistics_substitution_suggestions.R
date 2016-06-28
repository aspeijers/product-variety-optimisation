library(data.table)
#read in data 
suggestions = readRDS("suggestions.RData")
master = readRDS("master_mktshare.RData")
new_insp = readRDS("new_prod_subs_insp.RData")
new_tr = readRDS("new_prod_subs_treat.RData")

# find the old variety 
store_old_variety = master[,.(storeID,productID)]
rm(master)


#find if there are identical pairs but with the same order
new_tr = cbind(new_tr,both_prod = paste0(new_tr$exist_prod,new_tr$intro_prod))
new_insp = cbind(new_insp,both_prod = paste0(new_insp$intro_prod,new_insp$exist_prod))

tr_subset = new_tr[,c(1,4)]
insp_subset = new_insp[,c(1,4)]
both = rbind(tr_subset,insp_subset)
nrow(unique(both)) #It is different 

#combine them into one regardless of which is inspected and which is treated 
new_prod_subs = rbind(new_tr,new_insp)
new_prod_subs = new_prod_subs[duplicated(both)==FALSE,]

rm(tr_subset,insp_subset,both)
new_prod_subs = new_prod_subs[,1:3]

rm(new_insp,new_tr)

######### Substitution Analysis

#create new vectors
old_store_prod = paste0(store_old_variety$storeID,store_old_variety$productID) 
suggestions_store_prod = paste0(suggestions$storeID,suggestions$productID)

new_store_prod = setdiff(old_store_prod,suggestions_store_prod)

rm(old_store_prod,suggestions_store_prod)
#these are the store suggested products which are new
store_new_variety = as.data.table(new_store_prod)
store_new_variety = store_new_variety[,storeID:=substr(new_store_prod,1,4)]
store_new_variety = store_new_variety[,productID:=substr(new_store_prod,5,17)]

rm(new_store_prod)

#add other new columns for convenience
new_prod_subs = cbind(new_prod_subs,store_exist = paste0(new_prod_subs$storeID,new_prod_subs$exist_prod))
new_prod_subs = cbind(new_prod_subs,store_intro = paste0(new_prod_subs$storeID,new_prod_subs$intro_prod))

## suggested products - substitutes of themselves 

existing_exit = sum(new_prod_subs$store_exist %in% store_new_variety$new_store_prod)
existing_intro = sum(new_prod_subs$store_intro %in% store_new_variety$new_store_prod)

#Result: both of them are zero 

## substitutes between new and old products  

#add a column for convenience
store_old_variety = store_old_variety[,old_store_prod := paste0(storeID,productID)]

num_subs_new_old = c()

for (st in unique(store_new_variety$storeID)){
  print(which(unique(store_new_variety$storeID)==st))
  # the new product is existing and the old one is introduced
  existing_exit_new = sum(new_prod_subs[new_prod_subs$storeID == st,]$exist_prod %in% store_new_variety[store_new_variety$storeID == st,]$productID)
  existing_intro_old = sum(new_prod_subs[new_prod_subs$storeID == st,]$intro_prod %in% store_old_variety[store_old_variety$storeID == st,]$productID)
  subs_tuple1 = min(existing_exit_new,existing_intro_old)
  
  # the new product is existing and the old one is introduced
  existing_exit_old = sum(new_prod_subs[new_prod_subs$storeID == st,]$exist_prod %in% store_old_variety[store_old_variety$storeID == st,]$productID)
  existing_intro_new = sum(new_prod_subs[new_prod_subs$storeID == st,]$intro_prod %in% store_new_variety[store_new_variety$storeID == st,]$productID)
  subs_tuple2 = min(existing_exit_old,existing_intro_new)
  
  num_subs_new_old = c(num_subs_new_old,sum(subs_tuple1,subs_tuple2))
}

## substitutes between old products   
num_subs_old = c()

for (st in unique(store_new_variety$storeID)){
  print(which(unique(store_new_variety$storeID)==st))
  # the old product is existing and the old one is introduced
  existing_exit = sum(new_prod_subs[new_prod_subs$storeID == st,]$exist_prod %in% store_old_variety[store_old_variety$storeID == st,]$productID)
  existing_intro = sum(new_prod_subs[new_prod_subs$storeID == st,]$intro_prod %in% store_old_variety[store_old_variety$storeID == st,]$productID)
  value = min(existing_exit,existing_intro)
  num_subs_old = c(num_subs_old,value)
}


hist(num_subs_old,main = "Number of Substitute Tuples\n in the product variety which\n already exists in the store", xlab = "number of tuples")





