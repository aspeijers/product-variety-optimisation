# check to see if each store belongs to more than 1 promo group

a <- readRDS("stores.RData")

names(a)
a <- a[,.(storeID, promo_group)]
a <- unique(a)

for (store in a$storeID) {
    b <- length(a[storeID==store]$promo_group)
    if (b>1){
        print(store)
    }
    
}

# answer: each store belongs to only 1 promo group