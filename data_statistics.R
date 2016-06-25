### some data statistics
stores_bla =readRDS("stores.RData")
no_na = stores[complete.cases(stores),]

products = readRDS("products.RData")

fam_statistics  = a_llproducts[,.(num_products = length(productID)), by = fam]
avg_fams = c()
for(i in fam_statistics$fam){
  if (fam_statistics[fam==i,]$num_products == 1 |
      fam_statistics[fam==i,]$num_products == 2 |
      fam_statistics[fam==i,]$num_products == 3){
    avg_fams = c(avg_fams,i)
  }
}

length(avg_fams)/length(unique(products$fam))

subFam_statistics  = all_products[,.(num_products = length(productID)), by = subFam]
avg_subFams = c()
for(i in subFam_statistics$subFam){
  if (subFam_statistics[subFam==i,]$num_products == 1 |
      subFam_statistics[subFam==i,]$num_products == 2 |
      subFam_statistics[subFam==i,]$num_products == 3){
    avg_subFams = c(avg_subFams,i)
  }
}

length(avg_subFams)/length(unique(products$subFam))

hist(products$expirationDay,breaks = length(table(products$expirationDays)), xlab = "Number of Expiration Days",xlim = c(0,200),main="")


variety  = sales[,.(num_products = length(unique(productID))), by = storeID]

hist(variety$num_products,xlab = "Number of Products per Store", main="", xlim = c(0,250), ylim = c(0,800))
