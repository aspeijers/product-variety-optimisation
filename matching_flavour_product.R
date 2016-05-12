### Libraries
library(data.table)
library(stringr)

setwd("~/BGSE/semseter3/kernel/data")

### TAbles
flavor_data<-readRDS("skuSabor.RData")
products = readRDS("products.RData") #I have renamed the new skuClean2.RData file

# Create new columns based on the last 3 digits of family, subfamily, etc.
products = products[, c("productID_3","subFam_3","fam_3") := 
                      list(str_sub(productID,-3,-1),str_sub(subFam,-3,-1),str_sub(fam,-3,-1))]

products[1,.(productID_3,subFam_3,fam_3,grup)]

#Do the same for the flavour_data
flavor_data = flavor_data[, c("subfam_3","fam_3") := 
                      list(str_sub(subfam,-5,-1),str_sub(fam,-4,-1))]

flavor_data[1,.(sku,subfam_3,fam_3,grup)]





