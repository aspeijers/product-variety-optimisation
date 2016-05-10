library(data.table)
library(stringr)
setwd("~/BGSE/semseter3/kernel/data")

# How many products per subFamily
master <- readRDS("master_table.RData")

subFams <- unique(master$subFam) #134 

# let's see for the alltimers
products = readRDS("products.RData")
productID_alltimer = products[availability == "alltimer",productID]

master = master[productID %in% productID_alltimer]

subFam_product <-as.data.frame(matrix(NA,length(subFams),2))
names(subFam_product) <- c("subFam","no_of_products")
for ( i in 1:length(subFams) ) {
    a <- master[subFam==subFams[i]]
    subFam_product[i,1] <- subFams[i]
    subFam_product[i,2] <- length(unique(a$productID))
    
}

#table(subFam_product[,2])
bigger_than_1 = subFam_product[subFam_product$no_of_products>1,]

############# check what the difference are between products where...
####### 2 products 
prod_2 = bigger_than_1[bigger_than_1$no_of_products ==2,1]
master_2_prod = master[subFam %in% prod_2,.(units,subFam)]
#different units?

for (i in 1:length(prod_2)) {
  print( prod_2[i])
  print(length(unique(master_2_prod[subFam == prod_2[i],units])))
}  

#No

prod_2_table = as.data.frame(matrix(NA,length(prod_2),2))

for (i in 1:length(prod_2)) {
  print( prod_2[i])
  print(length(unique(master_2_prod[subFam == prod_2[i],units])))
} 




##### 6 products per subFam
subFams_6 <- subFam_product[subFam_product$no_of_products==6,1]

master_6 <- master[subFam %in% subFams_6,.(units,subFam)]

table(master_6[subFam == subFams_6[1],units])
table(master_6[subFam == subFams_6[2],units])
table(master_6[subFam == subFams_6[3],units])
# units is changing. 
# What is flavour doing???????

subFam1 <- unique(master[subFam==subFams_6[1],productID])
changing_package_numers = str_sub(subFam1,-3,-1)




##### 5 products per subFam
subFams_5 <- subFam_product[subFam_product$no_of_products==5,1]

master_5 <- master[subFam %in% subFams_5,.(units,subFam)]

table(master_5[subFam == subFams_5[1],units])
table(master_5[subFam == subFams_5[2],units])
table(master_5[subFam == subFams_5[3],units])
table(master_5[subFam == subFams_5[4],units])
# all subfamilies only have 1 type of units sold.

subFam2 <- unique(master[subFam==subFams_5[1],productID])
changing_package_numers = str_sub(subFam2,-3,-1)

##### 2 products per subFam
subFams_2 <- subFam_product[subFam_product$no_of_products==2,1]

master_2 <- master[subFam %in% subFams_2,.(units,subFam)]

for (i in 1:length(subFams_2)) {
    print( table(master_2[subFam == subFams_2[i],units]) )
}



# let's do it for all products 
product_change = master[,.(productID = str_sub(productID,-3,-1),units,subFam)]
# take the ones which are bigger than 1 
product_change = product_change[subFam %in% bigger_than_1$subFam]

table(product_change)
# Probably the subFam corresponds to flavour because the number of units is not changing 

# let's chack that for family and subfamily
subFam_change = master[,.(subFam = str_sub(subFam,-3,-1),units,fam)]

fam_subFam <-as.data.frame(matrix(NA,length(unique(master$fam)),2))
names(fam_subFam) <- c("fam","no_of_subFams")
fams = unique(master$fam)
for ( i in 1:length(unique(master$fam)) ) {
  a <- master[fam==fams[i]]
  fam_subFam[i,1] <- fams[i]
  fam_subFam[i,2] <- length(unique(a$subFam))
  
}

bigger_than_1_subFam = fam_subFam[fam_subFam$no_of_subFams>1,]
# take the ones which are bigger than 1 

subFam_change = subFam_change[fam %in% bigger_than_1_subFam$fam]

table(subFam_change)

#Maybe but it is not conclusive 




