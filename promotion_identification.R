library(data.table)
library(ggplot2)





stores= readRDS("stores.RData")

# Look which stores we need 
#master_train = readRDS("master_train_mktshare.RData")
#master_test = readRDS("master_test_mktshare.RData")
#take the storeIDs which are common for both train and test
#storeID_master = unique(c(master_test$storeID,master_train$storeID))

#rm(master_test)
#rm(master_train)


#subset stores for the ones which we want 
#stores= stores[storeID %in% storeID_master]


#find the not-changing-promo_group storeIDs
not_changing_promo_group_stores = c()
for(i in unique(stores$storeID)){
  if(length(unique(stores[storeID ==i,]$promo_group)) == 1){ 
    not_changing_promo_group_stores=c(not_changing_promo_group_stores,i)
    }
}

#subset only for those whose promo_group is not changing 
stores = stores[storeID %in% not_changing_promo_group_stores]

#take only one row for each store (nothing is changing now)
stores[,date:=NULL]
stores = stores[duplicated(stores)==FALSE]


#save it 
saveRDS(stores,"stores_promo_group.RData" )

#how many promotion groups there are 
length(unique(stores$promo_group))

#first remove the promo_group where there are less than 2 stores 
promo_group_filtered = as.data.frame(table(stores$promo_group))
promo_group_filtered = promo_group_filtered[promo_group_filtered$Freq>2,]

############################### Functions  ####################################

# clustering promotional periods function
cluster_dates = function(x) {
    
    days_since_previous_event = diff(x, lag = 1)
    #print(days_since_previous_event)
    
    cluster = rep(NA,length(x))
    
    index=1
    first_day = x[1]
    cluster[1] = index
    
    for (k in 1:length(days_since_previous_event)) {
        if (days_since_previous_event[k]<=3 & ((x[k+1] - first_day) < 12)) {
            cluster[k+1] = index
        }else{
            cluster[k+1] = index + 1
            index = index + 1 
            first_day = x[k+1]
        }
    }
    return(cluster)
}

# ratio lagged sales
simple_ratio_function = function(x){
    simple_ratio = x/shift(x,type="lag")
    simple_ratio = ifelse(is.na(simple_ratio), 1, simple_ratio)
}

################################################################################
#load in the sales data 
sales = readRDS("sales.RData")

##### 1. look at each promo group
# initialise empty datafame 
promo_prod_date <- data.frame(promo_group=integer(),
                              productID=character(),
                              promo_start_date=as.Date(character()),
                              stringsAsFactors=FALSE)


# this code takes ~5mins to run
for (i in 1:nrow(promo_group_filtered)) {
    cat("promo_group", i, "\n")
    promo1 = as.integer(as.character(promo_group_filtered$Var[i]))
    stores_promo1 = stores[promo_group == promo1]$storeID
    
    #subset for promo1
    sales_promo1 = sales[storeID %in% stores_promo1]
    
    common_products = c()
    for(i in unique(sales_promo1$productID)){
        if(length(unique(sales_promo1[productID==i]$storeID))==length(stores_promo1)){
            common_products = c(common_products,i)
        }
    }
    
    
    for (j in 1:length(common_products)) {
        cat("product",j)
        ##### 2. Simple ratio test
        product1 = common_products[j]
        
        # subset sales_promo1 for product j
        sales_promo1_pr1 = sales_promo1[productID==product1]

        sales_promo1_pr1 = sales_promo1_pr1[,.(productID, date,quantity_sold_kg,simple_ratio = simple_ratio_function(quantity_sold_kg)), by = storeID]
        
        
        #ggplot(sales_promo1_pr1, aes(x=date, y=simple_ratio, group = as.character(storeID), colour = as.character(storeID))) +
            #geom_line() +
            #theme_bw()
        
        # find 99th quantile for product j
        above = quantile(sales_promo1_pr1$simple_ratio, 0.99)
        
        # get records higher than this quantile
        extreme_events = sales_promo1_pr1[simple_ratio >= above]
        
        # plots
        #ggplot(extreme_events, aes(x=date, y=simple_ratio, group = as.character(storeID), colour = as.character(storeID))) +
            #geom_point() +
            #theme_bw()
        
        # pick promotional periods
        setorder(extreme_events, date)
        
        if (nrow(extreme_events)>1) {
            # call function
            extreme_events[,cluster:=cluster_dates(date)]
            
            # count no of stores in each cluster
            extreme_events <-extreme_events[,cluster_size:=length(storeID),by=cluster]
            
            # only tak events that occur in a minimum number of stores
            percentage_to_keep = 0.1
            no_stores_required = length(stores_promo1)* percentage_to_keep
            
            if ( max(extreme_events$cluster_size) >= no_stores_required ) {
                extreme_events <- extreme_events[cluster_size>=no_stores_required]
                
                # find the begining and start dates of these promotions
                extreme_events[, promo_start_date := min(date), by=cluster]
                
                # keep promo_group, productID, and promo_start_date
                extreme_events[,promo_group:=promo1]
                promo_prod_date_current <- unique(extreme_events[,.(promo_group, productID), by=promo_start_date])
                setcolorder(promo_prod_date_current, c("promo_group", "productID", "promo_start_date"))
                promo_prod_date <- rbind(promo_prod_date, promo_prod_date_current)
            }
        }
    }
}

saveRDS(promo_prod_date,"promotion_dates.RData")


################################################################################

# investigation - are promo groups actually the same as one another?
sum(duplicated(promo_prod_date[,.(productID, promo_start_date)]))
length(unique(promo_prod_date$productID))
a <- promo_prod_date[,.(promo_group_size=length(promo_group)), by=.(productID, promo_start_date)]
table(a$promo_group_size)
# they have similarities, but also many differences. 17/98 is not huge, and this is only for one product, one date and also not precise.
