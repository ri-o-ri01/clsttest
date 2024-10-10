

# For Prod ----------------------------------------------------------------
## For Clustering Model Inputs----------------------------------------------------
# set a market signals for Clusterint Model
prod_ref_names_list1 <- list(
  c("VIX", "USGG10YR"),
  c("VIX", "USGG10YR", "USDJPY")
)

# set parameters about clustering models
cc_manager <- ClusteringConfigManager$new()
gmm_mclust_model_params = tribble(
  ~model_name, ~lib_name, ~start_dt,    ~seed, ~clust_num,
  "GMM",       "Mclust",  "2002-01-01", 123,   3
)
kmeans_stats_model_params3 = tribble(
  ~model_name, ~lib_name, ~start_dt,   ~seed, ~clust_num,
  "K-means",   "stats",  "2002-01-01", 123,   3
)
  
cc_manager$set_mclust_exper_param_tbl_list(
  "GMM_PROD_EXPER1", prod_ref_names_list1, gmm_mclust_model_params)
cc_manager$set_mclust_exper_param_tbl_list(
  "KMEANS_PROD_EXPER1", prod_ref_names_list1, kmeans_stats_model_params3)

# For Checking BIC
bic_gmm_mclust_model_params_tbl <- tribble(
  ~model_name, ~lib_name, ~start_dt,    ~seed,
  "GMM",       "Mclust",  "2002-01-01", 123
) %>% expand_grid(clust_num = 2:9)
cc_manager$set_mclust_exper_param_tbl_list(
  "GMM_PROD_EXPER1_BIC", prod_ref_names_list1, bic_gmm_mclust_model_params_tbl)

CLUST_CONFIG_SETTING <- register_clust_config_setting(cc_manager)

## For Clustering Invest Strategy Inputs-----------------------------------------------------
my_clust_elem  = "GMM_PROD_EXPER1"
my_clust_elem2 = "SIMP_PROD_EXPER1"
my_universe_set_name = "SIMP_PROD_UNIVERSE_TBL"

# set financial instruments for backtesting a portfolio based on clustering model results
config_manager <- ClustInvestConfigManager$new()
config_manager$add_universe(my_universe_set_name, "JT7203_LONG_ONLY", 
                            c("JT7203_TRI_LONG", "ZERO"), c("JT7203"))
config_manager$add_universe(my_universe_set_name, "JT7203", 
                            c("JT7203_TRI_LONG", "JT7203_TRI_SHORT", "ZERO"), c("JT7203"))
config_manager$add_universe(my_universe_set_name, "SPX_ETF_LONG_ONLY", 
                            c("SPX_ETF_TRI_LONG", "ZERO"), c("SPX_ETF"))
config_manager$add_universe(my_universe_set_name, "SPX_ETF", 
                            c("SPX_ETF_TRI_LONG", "SPX_ETF_TRI_SHORT", "ZERO"), c("SPX_ETF"))

# Set YAML parameters for use in generate.R
ENUM_HIER_CLUST_INVEST_TBL[[paste0(my_clust_elem, "-", my_clust_elem2)]] = create_simple_enum_hier_clust_invest_tbl(
  enum_param_tbl = CLUST_CONFIG_SETTING$ENUM_PARAM_TBL[[my_clust_elem]], 
  universe_tbl = config_manager$get_universe_tbl(my_universe_set_name), 
  exper_prefix = my_clust_elem2,
  from_file_type ="yf_path"
)
# Set CLUST_INVEST_SETTING_FACTORY
register_simple_clust_invest_setting_factories(
  clust_elem  = my_clust_elem,
  clust_elem2 = my_clust_elem2,
  universe_tbl = config_manager$get_universe_tbl(my_universe_set_name),
  start_dt     = "2000-01-01", 
  end_dt       = "2023-12-31"
)

