library(data.table)
library(lubridate)

sales<-readRDS("/home/didi/BGSE/semester3/kernel/data/sales.RData")

## Checking if we have two entries for the same product on the same date and for the same store in the sales data 
a = sales[,.(daily_records=length(date)),by=.(storeID,productID)]
b = sales[,.(uni_daily_records=length(unique(date))),by=.(storeID,productID)]
sum(a$daily_records!=b$uni_daily_records)

# Result - NO


## Aggregate on chain, time
agg_by_time = sales[,.(average_sales=mean(quantity_sold_kg))]
