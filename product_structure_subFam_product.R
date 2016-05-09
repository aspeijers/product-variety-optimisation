# How many products per subFamily
master <- readRDS("master_table.RData")

subFams <- unique(master$subFam)

subFam_product <-as.data.frame(matrix(NA,length(subFams),2))
names(subFam_product) <- c("subFam","no_of_products")
for ( i in 1:length(subFams) ) {
    a <- master[subFam==subFams[i]]
    subFam_product[i,1] <- subFams[i]
    subFam_product[i,2] <- length(unique(a$productID))
    
}

table(subFam_product[,2])

############# check what the difference are between products where...
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

