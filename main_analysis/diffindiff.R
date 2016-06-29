##################################################################################################
######## Description: Identifying the events for each store (intoduction/removal of a product)
######## input: assortment_changes.RData, products.RData, product_store_timeline.RData, sales.RData
########        promotion_dates.RData
######## output: events.RData, event_table_all.RData
##################################################################################################


# accounting for missing file in the assortment data
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
#This piece of code distributes sellIn-s among days until the next sellIn.
#It does not seem to smooth the sales very effectively, that's why it was not developed into a function.
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

#DIFFERENCE IN DIFFERENCES
library(data.table)
sales <- readRDS("sales.RData")
events <- readRDS("events.RData")
promo_prod_date <- readRDS("promotion_dates.RData")
events$date <- as.Date(events$date, "%Y%m%d")
setkey(sales, productID)

#This function asks for a treatment product - which was introduced to or taken out from assortment -
#and and inspected product -which was available before and after the event.
#Its output is a data frame (relevant_events_forcalc), which contains these events and for every event
#a salesbefore, salesafter and salesratio variable.
#These variable explain how the sales of the inspected product hanged in the given store after the event.
#They are supposed to give us a proxy about the substitution effect between the two products.
diffindiff <- function(treatment_product, inspected_product, days_window, introduction,
                       events, sales, promo_prod_date){
    
    #relevant_events_forcalc contains the events from which we potentially want to calculate a salesratio.
    #relevant_events_treatment and relevant_events_inspected contain the events
    # which are needed to check if the products were available for enough time.
    relevant_events_treatment <- events[events$productID==treatment_product &
                                            events[, inspected_product], 1:4]
    relevant_events_inspected <- events[events$productID==inspected_product, 1:4]
    relevant_events_forcalc <- relevant_events_treatment[relevant_events_treatment$begin==introduction,]
    cat("relevant events selected\n")
    relevant_promotions <- promo_prod_date[productID == inspected_product,]
    relevant_events_forcalc$enoughdata <- NA
    #Was the inspected product available long enough before and after the event?
    relevant_events_forcalc$enoughdata <- apply(relevant_events_forcalc, 1, function(x) enoughdata(x['date'], x['storeID'], relevant_events_treatment, relevant_events_inspected, days_window, relevant_promotions))
    relevant_events_forcalc <- relevant_events_forcalc[relevant_events_forcalc$enoughdata,]
    
    cat("\nRelevant events with enough sales data found.\n")
    cat("Number of events:")
    cat(nrow(relevant_events_forcalc))
    cat("\n")
    relevant_events_forcalc$salesafter <- NA
    relevant_events_forcalc$salesbefore <- NA
    relevant_events_forcalc$salesratio <- NA
    relevant_sales <- sales[productID == inspected_product,]
    setkeyv(relevant_sales, c("storeID", "date"))
    for(i in 1:nrow(relevant_events_forcalc)){
        cat(i)
        relevant_sales_for_specific_store <- relevant_sales[storeID == relevant_events_forcalc$storeID[i],]
        # shift function -> add zero rows to sales function (look at promotion_identification_smoothing.R)
        relevant_events_forcalc$salesbefore[i] <- sum(relevant_sales_for_specific_store[date < relevant_events_forcalc$date[i] &
                                                                                        date >= relevant_events_forcalc$date[i]-days_window, quantity_sold_kg])
        relevant_events_forcalc$salesafter[i] <- sum(relevant_sales_for_specific_store[date >= relevant_events_forcalc$date[i] &
                                                                                        date < relevant_events_forcalc$date[i]+days_window, quantity_sold_kg])
    }
    cat("\n")
    relevant_events_forcalc$salesratio <- relevant_events_forcalc$salesafter/relevant_events_forcalc$salesbefore
    relevant_events_forcalc <- relevant_events_forcalc[!is.na(relevant_events_forcalc$salesratio),]
    return(relevant_events_forcalc)
}

# Notice that the enoughdata function is called from within the diffindiff function
# This function selects asks for an event (defined by date and storeID)
# and decides if this event has enough data around it to calculate a before-after sales ratio.
enoughdata <- function(date, storeID, relevant_events_treatment, relevant_events_inspected, days_window, relevant_promotions){
    date <- as.Date(date)
    datebefore <- date-days_window
    dateafter <- date+days_window
    enoughdata <- TRUE
    
    #Is the time window in the time period when we have data?
    if(datebefore < as.Date("2015-01-01") || dateafter > as.Date("2016-03-15")){
        enoughdata <- FALSE
    }
    
    #Is there only one event for the treatment product in the given store within the time window?
    else if(nrow(relevant_events_treatment[relevant_events_treatment$storeID==storeID &&
                                           relevant_events_treatment$date >= datebefore &&
                                           relevant_events_treatment$date < dateafter, ]) > 1){
        enoughdata <- FALSE
    }
    
    #Is there any event for the inspected product in the given store within the time window?
    else if(nrow(relevant_events_treatment[relevant_events_inspected$storeID==storeID &&
                                           relevant_events_inspected$date >= datebefore &&
                                           relevant_events_inspected$date < dateafter, ]) > 0){
        enoughdata <- FALSE
    }
    
    #Has the inspected product a promotion in the given period?
    else if(nrow(relevant_promotions[relevant_promotions$storeID==storeID &&
                                     relevant_promotions$promo_start_date >= datebefore-14 &&
                                     relevant_promotions$promo_start_date < dateafter, ]) > 0){
        enoughdata <- FALSE
    }
    
    return(enoughdata)
}

#THIS PART OF THE CODE IS FOR TESTING THE DIFFINDIFF FUNCTION
#test the time it takes
treatment_product="G01F04S12S01"
inspected_product="G01F01S01S01"
days_window=30
introduction = TRUE
t0 <- system.time(diffindiff("G01F04S12S01", "G01F01S01S01", 30, introduction = TRUE,
                   events = events, sales = sales, promo_prod_date))
t1 <- system.time(diffindiff("G01F04S12S01", "G01F01S01S01", 30, introduction = TRUE,
                             events = events, sales = sales, promo_prod_date))
did1anew <- diffindiffnew("G01F04S12S01", "G01F01S01S01", 30, introduction = TRUE,
              events = events, sales = sales)
#trying
did1a <-diffindiff("G01F04S12S01", "G01F01S01S01", 30, introduction = TRUE,
           events = events, sales = sales, promo_prod_date)
did1b <-diffindiff("G01F04S12S01", "G01F01S01S01", 30, introduction = FALSE,
                  events = events, sales = sales)
did2a <-diffindiff("G19F03S02S01", "G01F01S01S01", 30, introduction = TRUE,
                  events = events, sales = sales)
did2b <-diffindiff("G19F03S02S01", "G01F01S01S01", 30, introduction = FALSE,
                  events = events, sales = sales)
did3a <-diffindiff("G19F03S02S02", "G01F01S01S01", 30, introduction = TRUE,
                  events = events, sales = sales)
did3b <-diffindiff("G19F03S02S02", "G01F01S01S01", 30, introduction = FALSE,
                  events = events, sales = sales)
plot(density(did1a$salesratio, na.rm=TRUE))
plot(density(did1b$salesratio, na.rm=TRUE))
plot(density(did2a$salesratio, na.rm=TRUE))
plot(density(did2b$salesratio, na.rm=TRUE))
plot(density(did3a$salesratio, na.rm=TRUE))
plot(density(did3b$salesratio, na.rm=TRUE))
hist(did1a$date, "weeks")
hist(did1b$date, "weeks")
hist(did2a$date, "weeks")
hist(did2b$date, "weeks")
hist(did3a$date, "weeks")
hist(did3b$date, "weeks")
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

#NUMBER OF EVENTS TABLE BY PRODUCT PAIRS

#Here we calculate the event_table_all data frame,
#which tells us how many events are available for each product pair.
library(reshape2)
even <- events[,-c(1, 3, 4)]
event_table_all <- aggregate(even[,-1], by=data.frame(even$productID), sum)
event_table_all <- melt(event_table_all, id=1)
names(event_table_all) <- c("treatment_productID", "inspected_productID", "number_of_events")
even <- events[events$begin,-c(1, 3, 4)]
event_table_intro <- aggregate(even[,-1], by=data.frame(even$productID), sum)
event_table_intro <- melt(event_table_intro, id=1)
names(event_table_intro) <- c("treatment_productID", "inspected_productID", "number_of_intros")
even <- events[!events$begin,-c(1, 3, 4)]
event_table_out <- aggregate(even[,-1], by=data.frame(even$productID), sum)
event_table_out <- melt(event_table_out, id=1)
names(event_table_out) <- c("treatment_productID", "inspected_productID", "number_of_outs")
event_table_all <- merge(event_table_all, event_table_intro)
event_table_all <- merge(event_table_all, event_table_out)
event_table_all <- event_table_all[as.character(event_table_all$treatment_productID)!=as.character(event_table_all$inspected_productID),]
event_table_all <- event_table_all[event_table_all$number_of_events>10,]
saveRDS(event_table_all, "event_table_all.RData")

#The numberofevents function counts for a given product pair the relevant number of events,
#given constraints of a selected days_window.
#Notice, that it uses the enoughdata function,
#and the whole function is very similar to the first part of diffindiff.
numberofevents <- function(treatment_product, inspected_product, days_window, introduction, events, promo_prod_date){
    relevant_events_treatment <- events[events$productID==treatment_product &
                                            events[, inspected_product], 1:4]
    relevant_events_inspected <- events[events$productID==inspected_product, 1:4]
    relevant_events_forcalc <- relevant_events_treatment[relevant_events_treatment$begin==introduction,]
    cat(".")
    relevant_promotions <- promo_prod_date[promo_prod_date$productID == inspected_product,]
    #Was the inspected product available long enough before and after the event?
    sum(apply(relevant_events_forcalc, 1, function(x) 
        enoughdata(x['date'], x['storeID'], relevant_events_treatment, relevant_events_inspected, days_window, relevant_promotions)))
    
}

#The next part of the code takes a lot of time to run,
#it is meant to be run on a server or a powerful machine.
library(foreach)
library(iterators)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

event_table_test
foreach(x=iter(event_table_test[event_table_test$number_of_intros>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 30, introduction = TRUE, events, promo_prod_date)

event_table_all <- readRDS("event_table_all.RData")
event_table_all <- event_table_all[event_table_all$number_of_events>10,]
# event_table_test <- event_table_all[1000:2000,]
event_table_all$number_of_intros30[event_table_all$number_of_intros>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_intros>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 30, introduction = TRUE, events, promo_prod_date)
event_table_all$number_of_outs30[event_table_all$number_of_outs>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_outs>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 30, introduction = FALSE, events, promo_prod_date)
event_table_all$number_of_events30 <- event_table_all$number_of_intros30 + event_table_all$number_of_outs30
event_table_all <- event_table_all[event_table_all$number_of_events30>10,]
saveRDS(event_table_all, "event_table_all30.RData")

event_table_all$number_of_intros60[event_table_all$number_of_intros30>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_intros30>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 60, introduction = TRUE, events, promo_prod_date)
event_table_all$number_of_outs60[event_table_all$number_of_outs30>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_outs30>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 60, introduction = FALSE, events, promo_prod_date)
event_table_all$number_of_events60 <- event_table_all$number_of_intros60 + event_table_all$number_of_outs60
event_table_all <- event_table_all[event_table_all$number_of_events60>10,]
saveRDS(event_table_all, "event_table_all60.RData")

event_table_all$number_of_intros90[event_table_all$number_of_intros60>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_intros60>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 90, introduction = TRUE, events, promo_prod_date)
event_table_all$number_of_outs90[event_table_all$number_of_outs60>0] <- 
    foreach(x=iter(event_table_all[event_table_all$number_of_outs60>0,1:2], by='row'), .combine='c') %dopar% numberofevents(as.character(x$treatment_productID), as.character(x$inspected_productID), 90, introduction = FALSE, events, promo_prod_date)
event_table_all$number_of_events90 <- event_table_all$number_of_intros90 + event_table_all$number_of_outs90
event_table_all <- event_table_all[event_table_all$number_of_events90>10,]
saveRDS(event_table_all, "event_table_all90.RData")