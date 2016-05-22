library(data.table)
install.packages("dummies")
library(dummies)

#setwd("~/BGSE/semester3/kernel/data")
setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")


################################ Training ######################################
# Read in the data
master_train = readRDS("master_train.RData")

### Train dataset
# remove columns that will not be used in models
master_train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", "total_shelf_space", 
                "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                "chain", "town", "size", "idComunidad", "idProvince", "units", 
                "flavor", "type")

# update necessary columns to factors
master_train <- master_train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

# add id column
master_train[,id:=seq(1:nrow(master_train))]

# using Dummy pkg create dummy vars for each factor
for (i in factor_vars) {
    print(i)
    dummy_matrix <- dummy(i, master_train)
    dummy_matrix <- as.data.table(dummy_matrix)
    master_train <- cbind(master_train, dummy_matrix)
}

rm(dummy_matrix)

saveRDS(master_train, "master_train_dummies.RData")

################################ Testing ######################################
# Read in the data
master_test = readRDS("master_test.RData")

### test dataset
# remove columns that will not be used in models
master_test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", "total_shelf_space", 
                "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "idComunidad", "idProvince", "units", 
                 "flavor", "type")

# update necessary columns to factors
master_test <- master_test[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

# add id column
master_test[,id:=seq(1:nrow(master_test))]

# using Dummy pkg create dummy vars for each factor
for (i in factor_vars) {
    print(i)
    dummy_matrix <- dummy(i, master_test)
    dummy_matrix <- as.data.table(dummy_matrix)
    master_test <- cbind(master_test, dummy_matrix)
}

rm(dummy_matrix)

saveRDS(master_test, "master_test_dummies.RData")
