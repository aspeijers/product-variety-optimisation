library(data.table)
setwd("~/BGSE/semester3/kernel/data")
# Read in the data
master_test = readRDS("master_test.RData")
master_train = readRDS("master_train.RData")

### Starting to think about how to evaluate models 
### this is a test script


# first create all of the the factor variables as dummy ones 
# redo catagorical variables in the test and the train 


### Train dataset

##remove unnecessary columns
input = master_train[,c("total_quantity","days_in_assort", "availability"):=list(NULL,NULL,NULL)]
factor_var = names(input)[-1*c(3,15,16,17)]
a = input[,(factor_var):=lapply(.SD, as.factor),.SDcols=factor_var]

