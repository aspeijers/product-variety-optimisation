#this file takes into acctount both the suggestions and the substitution effects
#read in the necessary files
library(data.table)
product_differences = readRDS("product_differences.RData")
suggestions = readRDS("suggestions.RData")
predicted_mkt = readRDS("predicted_mktshare.RData")
subs = readRDS("substitutes.RData")
master = readRDS("master_mktshare.RData")

#first point is look how many of the suggested are substitutes 
#number of stores
num_stores = unique(product_differences$storeID)

suggestion_substitutes = as.data.frame(matrix(NA,1,3))
names(suggestion_substitutes)=c("storeID","product1","product2")
i = 1

# New products as Inspected products
for (st in num_stores) {
  print(i)
  #subset for the store
  store_info = suggestions[storeID==st]
  
  # find the new products for this store
  new_prods = setdiff(store_info$productID, master[storeID == st,]$productID )
  
    for (pr in new_prods) {
        #check if each new product is in the substitutes list (inspected products)
        if (sum(subs$inspected_productID==pr)!=0) {
            treat = subs[subs$inspected_productID==pr,]$treatment_productID
      
            #check if the correseponding treated products exist in the recommendation for this store
            if (sum(treat %in% store_info$productID)!=0) {
                existing = treat %in% store_info$productID
                treat_existing = as.vector(treat[existing == TRUE])
        
                #if it is present, populate the substitution_siggestion table
                for (tr in 1:length(treat_existing)) {
                    store_insp_treat_pair = c(st, pr, treat_existing[tr])
                    suggestion_substitutes = rbind(suggestion_substitutes, store_insp_treat_pair)
                }
            }
        }
    }
  i = i+1
}

new_prod_subs_insp <- suggestion_substitutes[-1,]
names(new_prod_subs_insp)[2:3] <- c("intro_prod", "exist_prod")
saveRDS(new_prod_subs_insp, "new_prod_subs_insp.RData")

#new_prod_subs_insp = readRDS("new_prod_subs_insp.RData")

################################################################################
suggestion_substitutes1 = as.data.frame(matrix(NA,1,3))
names(suggestion_substitutes1)=c("storeID","intro_prod","exist_prod")
i = 1

# New products as Treatment products
for (st in num_stores) {  
    print(i)
    #subset for the store
    store_info = suggestions[storeID==st]
    
    # find the new products for this store
    new_prods = setdiff(store_info$productID, master[storeID == st,]$productID )
    
    for (pr in new_prods) {
        #check if each new product is in the substitutes list (treated products)
        if (sum(subs$treatment_productID==pr)!=0) {
            inspect = subs[subs$treatment_productID==pr,]$inspected_productID
            
            #check if the correseponding treated products exist in the recommendation for this store
            if (sum(inspect %in% store_info$productID)!=0) {
                existing = inspect %in% store_info$productID
                inspect_existing = as.vector(inspect[existing == TRUE])
                
                #if it is present, populate the substitution_siggestion1 table
                for (tr in 1:length(inspect_existing)) {
                    store_insp_treat_pair = c(st, pr, inspect_existing[tr])
                    suggestion_substitutes1 = rbind(suggestion_substitutes1, store_insp_treat_pair)
                }
            }
        }
    }
    i = i+1
}

new_prod_subs_treat <- suggestion_substitutes1[-1,]
saveRDS(new_prod_subs_treat, "new_prod_subs_treat.RData")

new_prod_subs_treat = readRDS("new_prod_subs_treat.RData")
################################################################################
#separate only the first store 
st1 = suggestion_substitutes[suggestion_substitutes$storeID==num_stores[1],]
#check how many product1 are in product2 and vice versa 
sum(st1$product1 %in% st1$product2)
sum(st1$product2 %in% st1$product1)

#check if they are just the same tuple 

pr1pr2 = paste0(st1$product1,st1$product2)
pr2pr1 = paste0(st1$product2,st1$product1)

sum(pr1pr2 %in% pr2pr1)

#no they are not 
# only the first is the mutual one 











