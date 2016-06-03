product2 = common_products[2]
# subset for the first product
sales_product2 = sales_promo1[productID==product2]
#sales_product2 = sales_product2[,simple_ration :=NA_real_]
# order by store and date
sales_product2 = setorder(sales_product2,storeID,date)

sales_product2 = sales_product2[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
ggplot(sales_product2, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 

product3 = common_products[3]
# subset for the first product
sales_product3 = sales_promo1[productID==product3]
#sales_product2 = sales_product2[,simple_ration :=NA_real_]
# order by store and date
 setorder(sales_product3,storeID,date)

 sales_product3 = sales_product3[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
ggplot(sales_product3, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 



see_product_promo1 = function(i){
  product3 = common_products[i]
  # subset for the first product
  sales_product3 = sales_promo1[productID==product3]
  #sales_product2 = sales_product2[,simple_ration :=NA_real_]
  # order by store and date
  setorder(sales_product3,storeID,date)
  
  sales_product3 = sales_product3[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
  print(ggplot(sales_product3, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw() )
}

see_product_promo1(4)
for (i in 1:10){
  see_product_promo1(i)
}

#################################################################################################
### town 
sales_promo1_pr1 = merge(sales_promo1_pr1,stores[,.(storeID,town)], by = "storeID", all.x = TRUE)
sales_promo1_pr1 = merge(sales_promo1_pr1,stores[,.(storeID,sub_chain)], by = "storeID", all.x = TRUE)
sales_promo1_pr1 = merge(sales_promo1_pr1,stores[,.(storeID,province)], by = "storeID", all.x = TRUE)


for(t in (unique(sales_promo1_pr1$province))){
  a = sales_promo1_pr1[province==t]
  print(ggplot(a, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
          geom_line() +
          theme_bw() )
}

sales_promo1 = merge(sales_promo1,products[,.(productID,fam)], by ="productID", all.x = TRUE)
sales_by_fam = sales_promo1[,.(storeID, date, quantity_sold_kg = sum(quantity_sold_kg)), by = fam]

## subset for one family 
common_fam = c()
for(i in unique(sales_promo1$fam)){
  if(length(unique(sales_promo1[fam==i]$storeID))==33){
    common_fam = c(common_fam,i)
  }
}

fam1 = common_fam[1]
sales_promo1_fm1 = sales_by_fam[fam == fam1]
sales_promo1_fm1 = sales_promo1_pr1[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]

ggplot(sales_promo1_fm1, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 


see_fam_promo1 = function(i){
  product3 = common_fam[i]
  # subset for the first product
  sales_product3 = sales_promo1[fam==product3]
  #sales_product2 = sales_product2[,simple_ration :=NA_real_]
  # order by store and date
  setorder(sales_product3,storeID,date)
  
  sales_product3 = sales_product3[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
  print(ggplot(sales_product3, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
          geom_line() +
          theme_bw() )
}



for (i in 1:10){
  see_fam_promo1(i)
}
