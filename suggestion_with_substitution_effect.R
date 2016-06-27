#this file takes into acctount both the suggestions and the substitution effects
#read in the necessary files
library(data.table)
product_differences = readRDS("product_differences.RData")
suggestions = readRDS("suggestions.RData")
predicted_mkt = readRDS("predicted_mktshare.RData")
subs = readRDS("substitutes.RData")

#first point is look how many of the suggested are substitutes 
#number of stores
num_stores = unique(product_differences$storeID)

suggestion_substitutes = as.data.frame(matrix(NA,1,3))
names(suggestion_substitutes)=c("storeID","product1","product2")


for(st in a){
  print(st)
  #subset for the store
  store_info = suggestions[storeID==st]
  for(pr in store_info$productID){
    #check if initially there is inspected product 
    if (sum(subs$inspected_productID==pr)!=0){
      treat = subs[subs$inspected_productID==pr,]$treatment_productID
      #check if there is present the treated product in the store
      if(sum(treat %in% store_info$productID)!=0){
        existing = treat %in% store_info$productID
        treat_existing = as.vector(treat[existing == TRUE])
        #if it is present, populate the substitution_siggestion table
        for(tr in 1:length(treat_existing)){
          store_insp_treat_pair = c(st,pr,treat_existing[tr])
          suggestion_substitutes = rbind(suggestion_substitutes,store_insp_treat_pair)
        }
      }
    }
}
}

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











