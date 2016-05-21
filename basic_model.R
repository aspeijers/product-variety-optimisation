setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
library(data.table)

master_test <- readRDS("master_test.RData")
master_train <- readRDS("master_train.RData")

# The predict.basic function uses sales from training where they are available
# and average sales by product where it was not sold before.
# It uses data.table inputs, but the output is data.frame.
predict.basic <- function(master_test, master_train){
    pred <- master_test[,.(productID, storeID)]
    setkeyv(pred, c("productID", "storeID"))
    setkeyv(master_train, c("productID", "storeID"))
    pred <- merge(pred, master_train[, .(productID, storeID, avg_sales_per_day)], all.x=TRUE)
    not_sold_in_training <- data.table(productID = pred[is.na(avg_sales_per_day), productID])
    setkey(not_sold_in_training, productID)
    master_train_avg_product <- master_train[,.(avg_sales_per_day = mean(avg_sales_per_day)), by =.(productID)]
    not_sold_in_training <- not_sold_in_training[master_train_avg_product, nomatch=0]
   
    pred <- data.frame(pred)
    pred[is.na(pred$avg_sales_per_day), "present_in_train"] <- FALSE
    pred[is.na(pred$present_in_train), "present_in_train"] <- TRUE
    pred[is.na(pred$avg_sales_per_day), "avg_sales_per_day"] <- not_sold_in_training[,avg_sales_per_day]

    return(pred)
}

#This function uses averages by product for prediction.
## the prediction for a product is the average of the product across all stores 
predict.avg <- function(master_test, master_train){
    pred <- master_test[,.(productID, storeID)]
    master_train_avg_product <- master_train[,.(avg_sales_per_day = mean(avg_sales_per_day)), by =.(productID)]
    pred <- pred[master_train_avg_product, nomatch=0]
    pred <- data.frame(pred)

    return(pred)
}

#Mean Square Error
mse <- function(master_test, prediction){
    master_test <- data.frame(master_test)
    sum((master_test$avg_sales_per_day - prediction$avg_sales_per_day)**2)/nrow(prediction)
}

#Mean Absolute Error 
mae <- function(master_test, prediction){
    master_test <- data.frame(master_test)
    sum(abs(master_test$avg_sales_per_day - prediction$avg_sales_per_day))/nrow(prediction)
}

#Mean Error 
me <- function(master_test, prediction){
    master_test <- data.frame(master_test)
    sum(master_test$avg_sales_per_day - prediction$avg_sales_per_day)/nrow(prediction)
}

prediction.basic <- predict.basic(master_test, master_train)
prediction.avg <- predict.avg(master_test, master_train)

mse(master_test, prediction.basic)
mse(master_test, prediction.avg)
mae(master_test, prediction.basic)
mae(master_test, prediction.avg)
me(master_test, prediction.basic)
me(master_test, prediction.avg)
mse(master_test[prediction.basic$present_in_train,], prediction.basic[prediction.basic$present_in_train,])
mse(master_test[!prediction.basic$present_in_train,], prediction.basic[!prediction.basic$present_in_train,])
mae(master_test[prediction.basic$present_in_train,], prediction.basic[prediction.basic$present_in_train,])
mae(master_test[!prediction.basic$present_in_train,], prediction.basic[!prediction.basic$present_in_train,])

library(ggplot2)

#this plotting function asks for an xvalue to split by
#add xvalue as a string!
plot.residuals <- function(master_test, prediction, xvalue, violin = FALSE, varwidth = FALSE){
    master_test <- data.frame(master_test)
    master_test$res <- master_test$avg_sales_per_day - prediction$avg_sales_per_day
    limits <- sapply(split(master_test$res, master_test[,xvalue]), 
                     function(x) boxplot.stats(x)$stats[c(1, 5)])
    ylim1 = c(min(limits), max(limits))
    p <- ggplot(master_test, aes_string(xvalue, "res")) +
        coord_cartesian(ylim = ylim1*1.025) +
        theme_bw() +
        geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
    if(!violin & varwidth){
        p <- p + geom_boxplot(varwidth = TRUE, outlier.size = 0)
    }
    if(!violin & !varwidth){
        p <- p + geom_boxplot(varwidth = FALSE, outlier.size = 0)
    }
    if(violin & varwidth){
        p <- p + geom_violin(scale = 'count')
    }
    if(violin & !varwidth){
        p <- p + geom_violin()
    }
    p
}

#Look, how gorgeous! o.o
plot.residuals(master_test, prediction.avg, "idComunidad")
plot.residuals(master_test, prediction.avg, "idComunidad", varwidth = TRUE)
plot.residuals(master_test, prediction.avg, "idComunidad", violin = TRUE)
plot.residuals(master_test, prediction.avg, "idComunidad", violin = TRUE, varwidth = TRUE)

#we should put this somewhere else
master_test$chain <- as.factor(master_test$chain)
master_test$town <- as.factor(master_test$town)