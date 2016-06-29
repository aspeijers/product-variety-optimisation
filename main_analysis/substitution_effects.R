##################################################################################################
######## Description: Testing the different substitute rules and using the best to find the 
########              substitute pairs 
######## input: event_table_60d.RData, event_table_90d.RData, products.RData 
######## output: substitutes.RData 
##################################################################################################

event_table_60d <- readRDS("event_table_60d.RData")
event_table_90d <- readRDS("event_table_90d.RData")
products <- readRDS("products.RData")

#some variables are list, lets turn these into normal data frames
for(i in 15:20){
    event_table_60d[,i] <- unlist(event_table_60d[,i])
    event_table_90d[,i] <- unlist(event_table_90d[,i])
}

#removing obsrvation which are based on less than 10 events
event_table_60d[event_table_60d$number_of_intros60<10, 15:17] <- NA
event_table_60d[event_table_60d$number_of_outs60<10, 18:20] <- NA
event_table_90d[event_table_90d$number_of_intros90<10, 15:17] <- NA
event_table_90d[event_table_90d$number_of_outs90<10, 18:20] <- NA



#choosing substitutes
event_table_60d$substitute <- event_table_60d$sr_median_intro<1 &
    event_table_60d$sr_median_out>1 &
    event_table_60d$sr_smaller_than_1_intro>0.5 &
    event_table_60d$sr_bigger_than_1_out>0.5

event_table_90d$substitute <- event_table_90d$sr_median_intro<1 &
    event_table_90d$sr_median_out>1 &
    event_table_90d$sr_smaller_than_1_intro>0.5 &
    event_table_90d$sr_bigger_than_1_out>0.5

event_table_60d$substitute90 <- event_table_90d$substitute
event_table_60d$substitute_both <- event_table_60d$substitute & event_table_60d$substitute90


#joining with the opposite pairs
#(the treatment product is the inspected and the inspected is the treatment)
event_table_60d <- merge(event_table_60d, event_table_60d[,c(1, 2, 21)],
                         by.x = c("treatment_productID", "inspected_productID"),
                         by.y = c("inspected_productID", "treatment_productID"),
                         all.x=TRUE)
event_table_90d <- merge(event_table_90d, event_table_90d[,c(1, 2, 21)],
                         by.x = c("treatment_productID", "inspected_productID"),
                         by.y = c("inspected_productID", "treatment_productID"),
                         all.x=TRUE)
event_table_60d$substitute_double <- event_table_60d$substitute.x & event_table_60d$substitute.y
event_table_90d$substitute_double <- event_table_90d$substitute.x & event_table_90d$substitute.y
event_table_60d$substitute90_double <- event_table_90d$substitute_double

event_table_60d$substitute_double_both <- event_table_60d$substitute_double & event_table_60d$substitute90_double


#remove rows with NAs
event_table_60d <- na.omit(event_table_60d)
event_table_90d <- na.omit(event_table_90d)
sum(event_table_60d$substitute.x)
sum(event_table_90d$substitute.x)
sum(event_table_60d$substitute_both)
sum(event_table_60d$substitute_double)
sum(event_table_90d$substitute_double)
sum(event_table_60d$substitute_double_both)

#product variables
event_table_60d <- merge(event_table_60d, products, by.x="treatment_productID", by.y="productID")
event_table_60d <- merge(event_table_60d, products, by.x="inspected_productID", by.y="productID")
event_table_90d <- merge(event_table_90d, products, by.x="treatment_productID", by.y="productID")
event_table_90d <- merge(event_table_90d, products, by.x="inspected_productID", by.y="productID")

#Are the treatment and inspected products in the same group/family/subfamily/flavor/type?
event_table_60d$same_grup <- event_table_60d$grup.x == event_table_60d$grup.y
event_table_60d$same_fam <- event_table_60d$fam.x == event_table_60d$fam.y
event_table_60d$same_subFam <- event_table_60d$subFam.x == event_table_60d$subFam.y
event_table_60d$same_flavor <- event_table_60d$flavor.x == event_table_60d$flavor.y
event_table_60d$same_type <- event_table_60d$type.x == event_table_60d$type.y

event_table_90d$same_grup <- event_table_90d$grup.x == event_table_90d$grup.y
event_table_90d$same_fam <- event_table_90d$fam.x == event_table_90d$fam.y
event_table_90d$same_subFam <- event_table_90d$subFam.x == event_table_90d$subFam.y
event_table_90d$same_flavor <- event_table_90d$flavor.x == event_table_90d$flavor.y
event_table_90d$same_type <- event_table_90d$type.x == event_table_90d$type.y

sum(event_table_60d$same_grup)/nrow(event_table_60d)
sum(event_table_60d$same_grup[event_table_60d$substitute.x])/sum(event_table_60d$substitute.x)
sum(event_table_60d$same_grup[event_table_60d$substitute_both])/sum(event_table_60d$substitute_both)

sum(event_table_90d$same_grup)/nrow(event_table_90d)
sum(event_table_90d$same_grup[event_table_90d$substitute.x])/sum(event_table_90d$substitute.x)
sum(event_table_90d$same_grup[event_table_90d$substitute_double])/sum(event_table_90d$substitute_double)

sum(event_table_60d$same_fam)/nrow(event_table_60d)
sum(event_table_60d$same_fam[event_table_60d$substitute.x])/sum(event_table_60d$substitute.x)
sum(event_table_60d$same_fam[event_table_60d$substitute_both])/sum(event_table_60d$substitute_both)

sum(event_table_90d$same_fam)/nrow(event_table_90d)
sum(event_table_90d$same_fam[event_table_90d$substitute.x])/sum(event_table_90d$substitute.x)
sum(event_table_90d$same_fam[event_table_90d$substitute_double])/sum(event_table_90d$substitute_double)

sum(event_table_60d$same_subFam)/nrow(event_table_60d)
sum(event_table_60d$same_subFam[event_table_60d$substitute.x])/sum(event_table_60d$substitute.x)
sum(event_table_60d$same_subFam[event_table_60d$substitute_both])/sum(event_table_60d$substitute_both)

sum(event_table_90d$same_subFam)/nrow(event_table_90d)
sum(event_table_90d$same_subFam[event_table_90d$substitute.x])/sum(event_table_90d$substitute.x)
sum(event_table_90d$same_subFam[event_table_90d$substitute_double])/sum(event_table_90d$substitute_double)

sum(event_table_60d$same_flavor)/nrow(event_table_60d)
sum(event_table_60d$same_flavor[event_table_60d$substitute.x])/sum(event_table_60d$substitute.x)
sum(event_table_60d$same_flavor[event_table_60d$substitute_both])/sum(event_table_60d$substitute_both)

sum(event_table_90d$same_flavor)/nrow(event_table_90d)
sum(event_table_90d$same_flavor[event_table_90d$substitute.x])/sum(event_table_90d$substitute.x)
sum(event_table_90d$same_flavor[event_table_90d$substitute_double])/sum(event_table_90d$substitute_double)

sum(event_table_60d$same_type)/nrow(event_table_60d)
sum(event_table_60d$same_type[event_table_60d$substitute.x])/sum(event_table_60d$substitute.x)
sum(event_table_60d$same_type[event_table_60d$substitute_both])/sum(event_table_60d$substitute_both)

sum(event_table_90d$same_type)/nrow(event_table_90d)
sum(event_table_90d$same_type[event_table_90d$substitute.x])/sum(event_table_90d$substitute.x)
sum(event_table_90d$same_type[event_table_90d$substitute_double])/sum(event_table_90d$substitute_double)


library(igraph)
substitutes <- event_table_60d[event_table_60d$substitute_both, 1:2]
saveRDS(substitutes, "substitutes.RData")
s <- graph.data.frame(substitutes)
tkplot(s)

