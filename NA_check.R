# Checking for NA's in master table
library(data.table)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")
master <- readRDS("master_table.RData")

# check which variables have NA's
nam <- names(master)
for (i in 1:length(nam)) {
    a <- sum(is.na(master[,nam[i], with=FALSE]))  
    cat( paste(c(nam[i], a)) )
    cat("\n")
}

# results: chain, town, size, idComunidad, idProvince

# Find all rows that contain one or more NA's
NA_chain <- master[is.na(chain)==T]
NA_town <- master[is.na(town)==T]
NA_size <- master[is.na(size)==T]
NA_idCom <- master[is.na(idComunidad)==T]
NA_idProv <- master[is.na(idProvince)==T]

NA_all <- unique(rbind(NA_chain, NA_town, NA_size, NA_idCom, NA_idProv)) #121,343 rows

# check if Community and Provence NA's match up
nrow(unique(rbind(NA_idCom, NA_idProv))) # they match

#check if Town NA's match up with Provence NA's
nrow(unique(rbind(NA_town, NA_idProv))) #106,865. ie all NA_town are also NA_province

# check if Size NA's match up with Provence
nrow(unique(rbind(NA_size, NA_idProv))) #112,340 
sum(duplicated(rbind(NA_size, NA_idProv))) #12,211 duplicates
nrow(NA_size) + ...

nrow(setdiff(NA_size, NA_idProv)) #17,686 more NA's in size than in provence
nrow(setdiff(NA_idProv, NA_size)) #106,865 more NA's in provence than in town
nrow(NA_size) + nrow(NA_idProv) - 17686 - 106865 # 0 overlaps 

#check if Size NA's match up with town
nrow(setdiff(NA_size))

