This folder consistis of the main analysis of our project.
Short description of the main-analysis files:

- **assortment_changes:** Finding alltimer products and tagging "not_in_assortment_data"
- **creating_master_tabel:** Creatng the master table without market shares
- **create_mkt_sales_master:** Creatng the master table with sales market shares (shelf space market shares already exist)
- **creating_mkt_shelf_space_master:** Creatng the master table with shelf space market shares
- **CV-cluster_evaluation:** Evaluate clusters based on how well they predict the product market share (by sales) a customized cross-validation function
- **diffindiff:** Identifying the events for each store (intoduction/removal of a product)
- **mkt_function:** Average the product market share sell-in over the clusters in the best clustering
- **product_store_timeline:** Creating the product_store_timeline (combining the information of the assortment folder)
- **promotion_identification:** Identify promotion periods
- **shelf_space:** Creating product-store timeline with respect to shelf space
- **store_clustering:** Convert all variables in the master table to dummy variables for each store and cluster the stores based on various subsets of these variables
- **substitution_effects:** Testing the different substitute rules and using the best to find the substitute pairs
- **suggestion_function:** this ranks the products by their predicted mkt share and returns the suggested products; no substitution effects included
- **suggestion_with_substitution_effect:** this file takes into acctount both the suggestions and the substitution effects
