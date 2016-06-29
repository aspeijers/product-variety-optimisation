##################################################################################################
######## Description: this file produces the sales by month by community plot 
######## input: sales_by_month.RData;stores.RData 
######## output:
##################################################################################################

library(ggplot2)
library(plyr)
library(data.table)


sales_by_month <- readRDS("sales_by_month.RData")
stores <- readRDS("stores.RData")
stores1 <- unique(stores[,.(storeID, community)])

# include community in sales by month data
datatoplot <- merge(sales_by_month, stores1, by = "storeID", all.x = TRUE)
datatoplot1 <- datatoplot[,.(summed_qty = sum(quantity_sold_kg)), by = .(community, month)]

# get rid of data before 1/15/2015
datatoplot2 <- datatoplot1[month >= as.Date("2015-01-01", "%Y-%m-%d")]

# rename communities
datatoplot2[,community:=ifelse( nchar(community)>2, substr(community,5, nchar(community)), community )]
#datatoplot2[,summed_qty:=summed_qty/1000]

# plot
ggplot(datatoplot2, aes(x=month, y=log(summed_qty), group=community, colour=community)) +
    geom_line(size=0.8) +
    scale_color_discrete(breaks=as.character(1:13), name="Autonomous\nCommunity") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_text(size = rel(1.2)),
          axis.text.x  = element_text(size= rel(1.1)),
          axis.text.y = element_text(size= rel(1.1)) ) +
    ylab("log of sell-in (kg)") 