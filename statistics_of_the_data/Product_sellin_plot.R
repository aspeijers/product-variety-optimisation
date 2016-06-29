##################################################################################################
######## Description: this file plots the sell-in for a typical product ina typical store  
######## input: sales.RData
######## output: 
##################################################################################################

library(ggplot2)
library(plyr)
library(data.table)
library(zoo)


sales <- readRDS("sales.RData")
id_pairs <- data.table(productID = "G13F03S01S04", storeID = 2784)

datatosmooth <- merge(id_pairs, sales, by = c("storeID", "productID"))

datatosmooth[,days_since_last := date - shift(date)]
datatosmooth$days_since_last[1] <- 0
datatosmooth$days_since_last <- as.numeric(datatosmooth$days_since_last)

# create zero values for missing days
df1 <- data.frame(date=seq(min(datatosmooth$date), max(datatosmooth$date), by="day"))
df2 <- merge(df1, datatosmooth[,3:4, with=FALSE], by="date", all.x=TRUE)
df2$quantity_sold_kg <- ifelse(is.na(df2$quantity_sold_kg), 0, df2$quantity_sold_kg)

# ggplot(datatosmooth, aes(x=date, y=quantity_sold_kg)) +
#     geom_bar(stat="identity", fill="orangered", colour="orangered3") +
#     #scale_fill_continuous(name="Days since\nlast truck") +
#     ylab("Sell-in quantity") +
#     theme_bw() +
#     theme(axis.title.x = element_blank(), 
#           axis.title.y = element_text(size = rel(1.2)),
#           axis.text.x = element_text(size=rel(1.2)),
#           axis.text.y = element_text(size=rel(1.2)))

ggplot(df2, aes(x=date, y=quantity_sold_kg)) +
    geom_line(colour="coral2") +
    ylab("Sell-in quantity (kg)") +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_text(size = rel(1.2)),
          axis.text.x = element_text(size=rel(1.2)),
          axis.text.y = element_text(size=rel(1.2)))
    
    