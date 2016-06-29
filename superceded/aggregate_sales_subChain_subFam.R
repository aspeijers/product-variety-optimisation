library(data.table)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

master_table <- readRDS("master_table.RData")

## Average by sub_chain and subFam
master_aggby_subChain_subFam <- master_table[,.(avg_sales = mean(avg_sales_per_day)), by =.(sub_chain,subFam)]
saveRDS(master_aggby_subChain_subFam, file = "master_aggby_subChain_subFam.RData")

