# For Prod ----------------------------------------------------------------
## For Clustering Model Inputs----------------------------------------------------
prod_ref_names_list1 <- list(
  c("VIX", "USGG2YR_USGG10YR_DIFF"),
  c("VIX", "USGG2YR_USGG10YR_DIFF", "USDJPY"),
  c("JT7203_HV_30D", "USDJPY")
)
cc_manager <- ClusteringConfigManager$new()
gmm_mclust_model_params = tribble(
  ~model_name, ~lib_name, ~start_dt,    ~seed, ~clust_num,
  "GMM",       "Mclust",  "2002-01-01", 456,   4
)
cc_manager$set_mclust_exper_param_tbl_list(
  "GMM_PROD_EXPER2", prod_ref_names_list1, gmm_mclust_model_params)

CLUST_CONFIG_SETTING <- register_clust_config_setting(cc_manager)

## For Clustering Invest Strategy Inputs-----------------------------------------------------
my_clust_elem  = "GMM_PROD_EXPER2"
my_clust_elem2 = "SIMP_PROD_EXPER2"
my_universe_set_name = "SIMP_PROD_UNIVERSE_TBL2"

config_manager <- ClustInvestConfigManager$new()
config_manager$add_universe(my_universe_set_name, "AAPL_LONG_ONLY", 
                            c("AAPL_TRI_LONG", "ZERO"), c("AAPL"))
config_manager$add_universe(my_universe_set_name, "AAPL", 
                            c("AAPL_TRI_LONG", "AAPL_SHORT_LONG","ZERO"), c("AAPL"))  
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