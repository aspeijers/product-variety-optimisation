setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
# this should be put somewhere else
# product_store_timeline <- readRDS("product_store_timeline.RData")
# product_store_timeline[, c("20150201") := get("20150131")]


#CREATING THE EVENTS TABLE
#it shows when were products introduced or taken out from specific store
#uses the assortment data
assortment_change <- readRDS("assortment_change.RData")
products <- readRDS("products.RData")
product_store_timeline <- readRDS("product_store_timeline.RData")
product_store_timeline <- as.data.frame(product_store_timeline)
events <- data.frame()

for(i in 4:440){
    cat(i)
    current <- assortment_change[assortment_change[,i] != 0, c(1:2, i)]
    if(nrow(current)>0) {
        current$date <- names(assortment_change)[i]
        names(current)[3] <- "begin"
        current$begin[current$begin==-1] <- FALSE
        current$begin <- as.logical(current$begin)
        current[,products$productID] <- FALSE
        for(j in 1:nrow(current)){
            others <- product_store_timeline[product_store_timeline$storeID == current$storeID[j], c(2, i)]
            others <- others[others[,2]==1,]
            for(k in others$productID){
                current[j, which(names(current) == k)] <- TRUE
            }
        }
        events <- rbind(events, current)   
    }
}
saveRDS(events, "events.RData")

#SMOOTH THE SALES
library(data.table)
sales <- readRDS("sales.RData")
id_pairs <- data.table(productID = "G13F03S01S04", storeID = 2784)

#inputs and outputs of this function are data.table

    datatosmooth <- merge(id_pairs, sales, by = c("storeID", "productID"))
    sales_daily <- data.table(date = seq(min(datatosmooth$date), max(datatosmooth$date)-1, by="days"),
                              quantity_sold_kg = as.numeric(NA))
    
    for(i in 1:(nrow(datatosmooth)-1)){
        sale_current <- datatosmooth[i,]
        sale_next <- datatosmooth[i+1,]
        period_length <- sale_next[,date] - sale_current[,date]
        rows <- which(sales_daily[,date] >= sale_current[,date] & sales_daily[,date] < sale_next[,date])
        for(r in seq_along(rows)){
            set(sales_daily,
                i=rows[r],
                j=2L,
                value=sale_current[,quantity_sold_kg]/as.numeric(period_length))
        }
    }

library(ggplot2)
ggplot(datatosmooth, aes(x=date, y=quantity_sold_kg)) +
    geom_line() +
    theme_bw()
ggplot(sales_daily, aes(x=date, y=quantity_sold_kg)) +
    geom_line() +
    theme_bw()

#SELECT RELEVANT SALES
library(data.table)
sales <- readRDS("sales.RData")
events <- readRDS("events.RData")
events$date <- as.Date(events$date, "%Y%m%d")
setkeyv(sales, c("productID", "storeID", "date"))

diffindiff <- function(treatment_product, inspected_product, days_window, introduction,
                       events, sales){
    
    relevant_events <- events[events$productID==treatment_product & events$begin==introduction &
                                  events[, inspected_product], 1:4]
    cat("relevant events found\n")
    relevant_events$enoughdata <- NA
    #Was the inspected product available long enough before and after the event?
    for(i in 1:nrow(relevant_events)){
        cat(i)
        datebefore <- relevant_events$date[i]-days_window
        dateafter <- relevant_events$date[i]+days_window-1
        
        #check if the window is in the time period when we have data
        if(datebefore >= as.Date("2015-01-01") & dateafter < as.Date("2016-03-15")){
            relevant_events$enoughdata[i] <- nrow(events[events$productID==treatment_product &
                                                    events$begin!=introduction &
                                                    events$storeID==relevant_events$storeID[i] &
                                                    events$date > datebefore &
                                                    events$date <= dateafter, 1:4]) < 1
        }
        else{
            relevant_events$enoughdata[i] <- FALSE
        }
    }
    cat("\nRelevant events with enough sales data found.\n")
    relevant_events <- relevant_events[relevant_events$enoughdata,]
    cat("Number of events:")
    cat(nrow(relevant_events))
    cat("\n")
    relevant_events$salesafter <- NA
    relevant_events$salesbefore <- NA
    relevant_events$salesratio <- NA
    for(i in 1:nrow(relevant_events)){
        cat(i)
        #keys
        relevant_events$salesbefore[i] <- sum(sales[productID == inspected_product &
                                                 storeID == relevant_events$storeID[i] &
                                                 date < relevant_events$date[i] &
                                                 date >= relevant_events$date[i]-days_window, quantity_sold_kg])
        relevant_events$salesafter[i] <- sum(sales[productID == inspected_product &
                                                     storeID == relevant_events$storeID[i] &
                                                     date >= relevant_events$date[i] &
                                                     date < relevant_events$date[i]+days_window, quantity_sold_kg])
    }
    
    relevant_events$salesratio <- relevant_events$salesafter/relevant_events$salesbefore
    relevant_events <- relevant_events[!is.na(relevant_events$salesratio),]
    return(relevant_events)
}

#trying
did1a <-diffindiff("G01F04S12S01", "G01F01S01S01", 16, introduction = TRUE,
           events = events, sales = sales)
did1b <-diffindiff("G01F04S12S01", "G01F01S01S01", 16, introduction = FALSE,
                  events = events, sales = sales)
did2a <-diffindiff("G19F03S02S01", "G01F01S01S01", 16, introduction = TRUE,
                  events = events, sales = sales)
did2b <-diffindiff("G19F03S02S01", "G01F01S01S01", 16, introduction = FALSE,
                  events = events, sales = sales)
did3a <-diffindiff("G19F03S02S02", "G01F01S01S01", 16, introduction = TRUE,
                  events = events, sales = sales)
did3b <-diffindiff("G19F03S02S02", "G01F01S01S01", 16, introduction = FALSE,
                  events = events, sales = sales)
plot(density(did1a$salesratio, na.rm=TRUE))
plot(density(did1b$salesratio, na.rm=TRUE))
plot(density(did2a$salesratio, na.rm=TRUE))
plot(density(did2b$salesratio, na.rm=TRUE))
plot(density(did3a$salesratio, na.rm=TRUE))
plot(density(did3b$salesratio, na.rm=TRUE))
median(did1a$salesratio, na.rm=TRUE)
median(did1b$salesratio, na.rm=TRUE)
median(did2a$salesratio, na.rm=TRUE)
median(did2b$salesratio, na.rm=TRUE)
median(did3a$salesratio, na.rm=TRUE)
median(did3b$salesratio, na.rm=TRUE)
saveRDS(did1a, "did1a.RData")
saveRDS(did1b, "did1b.RData")
saveRDS(did2a, "did2a.RData")
saveRDS(did2b, "did2b.RData")
saveRDS(did3a, "did3a.RData")
saveRDS(did3b, "did3b.RData")
did1a <- readRDS("did1a.RData")
did1b <- readRDS("did1b.RData")
did2a <- readRDS("did2a.RData")
did2b <- readRDS("did2b.RData")
did3a <- readRDS("did3a.RData")
did3b <- readRDS("did3b.RData")
