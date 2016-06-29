##################################################################################################
######## Description: Checking if any point of sales changes ownership
######## input: store.RData
######## output: 
##################################################################################################

library(data.table)
#### 

stores<-readRDS("stores.RData")
head(stores)
id=unique(stores$storeID)
changing_stores_chain=c()
changing_stores_sub_chain=c()
changing_stores_promo=c()
for (i in id){
   a = stores[storeID==i,length(unique(chain))]
   if (a!= 1){changing_stores_chain=c(changing_stores_chain,i)}
   
   b = stores[storeID==i,length(unique(sub_chain))]
   if (b!= 1){changing_stores_sub_chain=c(changing_stores_sub_chain,i)}
   
   c = stores[storeID==i,length(unique(promo_group))]
   if (c!= 1){changing_stores_promo=c(changing_stores_promo,i)}
}

# Result: 
# the promotional group changes and nothing else does



