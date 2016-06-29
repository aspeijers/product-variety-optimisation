#try to spread the sales over certain period 
# daily contdata

a = data.table(date=seq.Date(min(sales_promo1_pr1$date), max(sales_promo1_pr1$date), by="day"))
storeID = rep(stores_promo1,each = nrow(a))
a_all_stores = cbind(storeID,a)
rm(storeID)
b = merge(sales_promo1_pr1, a_all_stores, by=c("date","storeID"), all.y = TRUE)
#change all NAs to zeros 
b$quantity_sold_kg = ifelse(is.na(b$quantity_sold_kg),0,b$quantity_sold_kg)
#smoothing part 

smoothing = function(x,time_period){
    daily_sells = 0 
    for (i in 1:time_period){
        new_enty = shift(x,n=i,type = "lag")/time_period
        daily_sells = daily_sells + new_enty
    }
    return(daily_sells)
}

time_period = 14
b = b[,quantity_b:=NA_real_]
b = b[,.(date,quantity_sold_kg,quantity_b = smoothing(quantity_sold_kg,time_period)), by = storeID]

ggplot(b, aes(x=date, y=quantity_b, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw()  

b = b[,.(date,quantity_sold_kg,quantity_b,simple_ratio = simple_ratio_function(quantity_b)), by = storeID]

ggplot(b, aes(x=date, y=simple_ratio, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw() 


### median_ratio

median_ratio_function = function(x,median_period){
    values = as.data.frame(matrix(NA,length(x),median_period))
    for (i in 1:median_period){
        new_enty = shift(x,n=i,type = "lag")
        values[,i] = new_enty
    }
    med = apply(values,1,median)
    med = ifelse(is.na(med),x,med)
    med = ifelse(is.na(med),0,med)
    median_ratio = x/med
    median_ratio = ifelse(is.nan(median_ratio),1,median_ratio)
    median_ratio = ifelse(is.infinite(median_ratio),0,median_ratio)
    median_ratio = ifelse(is.na(median_ratio),0,median_ratio)
    return(median_ratio)
}

median_period = 7
sales_promo1_pr1 = sales_promo1_pr1[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
ggplot(sales_promo1_pr1, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw() 

b = b[,.(date,quantity_sold_kg,quantity_b,median_ratio = median_ratio_function(quantity_b,median_period)), by = storeID]
ggplot(b, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw() 

##looking at the sales_promo1
above = quantile(sales_promo1_pr1$median_ratio,probs =0.95)
sales_promo1_pr1 = sales_promo1_pr1[,extremes := median_ratio > above]
extreme_events = sales_promo1_pr1[extremes == TRUE]
setorder(extreme_events,date)
#date grouping 
cluster_dates = function(x){
    clusters = rep(NA,length(x))
    #clusters[1] = 1 
    a = diff(extreme_events$date,lag = 1)
    clusters = ifelse(a>3,1,0)
    
    index=1
    cluster_name = rep(NA,length(clusters))
    for (i in 1:length(clusters)){
        if (clusters[i]==0){
            cluster_name[i] = index
        }else{
            cluster_name[i] = index +1
            index = index +1 
        }
    }
}


## the smooth one 
above = quantile(b$median_ratio,probs =0.99)
b = b[,extremes := median_ratio > above]
extreme_events = b[extremes == TRUE]
setorder(extreme_events,date)
