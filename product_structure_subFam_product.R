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




