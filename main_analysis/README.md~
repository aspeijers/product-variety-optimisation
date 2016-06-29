### rename_tables.R 
Does exactly what it says. We rename variables and tables (mostly from Spanish to English) to make them more understandable.
Running this file is a prerequisite for running all other files. 

### changing_ownership.R
Checks to see whether the chain, subchain and promo group for each store change over time. 
Prerequisite: "rename_tables.R"
Running this file is NOT a prerequisite for running any of the other files. 

### product_store_timeline.R
Brings all the assortment files together into one table. Calculates the number of days that each product was for sale in each store. Resulting tables are saved as "product_store_timeline.RData" and "product_store_timeline_total_days.RData".
Prerequisite: "rename_tables.R"
Running this file is a prerequisite for running "master_table_creation.R". 

### master_table_creation.R
Combines sales, with product and store variables. Sales are averaged over time. 
Prerequisite: "product_store_timeline.R"
Running this file is a prerequisite for running other files.

### aggregate_sales_subChain_subFam.R
Averages the sales over sub-chains and sub-families.
Prerequisite: "master_table.R"

### data_splitting.R
Splits the master table data into training (75percent) and test (25percent) by stores, keeping the distribution of store sizes equal. There is also the start of some code to split by time, however this is not to be run yet. 
Prerequisite: "master_table.R"
Running this file will be a prerequisite to trialling sales functions. 