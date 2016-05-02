library(data.table)
library(sampling)

# read in master table
master <- readRDS("master_table.RData")
stores <- unique(master[,.(size, storeID)])

# What do we do with the NA's?????????? Omit them for now.
stores <- na.omit(stores)

# sort
stores <- stores[order(stores$size),]
table(stores$size)

# .75*29
# .75*130
# .75*79
# .75*611
# .75*997
# .75*658
# .75*36
qtys <- c(22, 97, 59, 485, 748, 493, 27)

training_stores <- strata(stores, "size", qtys, description=TRUE)
