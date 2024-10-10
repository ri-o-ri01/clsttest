CLUST_INV_INPUTS = list()
CLUST_INV_INPUTS[["simple_basic"]] = list(
  ar_inf       = NA,
  wd_no        = 3,
  prob_infs    = 0.0,
  reb_freqs    = c("1w", "2w", "eom"),
  plp_type     = "simple"
)

register_simple_clust_invest_setting_factories <- function(
    clust_elem, clust_elem2, universe_tbl, tri_file_loader, 
    start_dt     = "2000-01-01", end_dt       = "2023-12-31",
    config_type = "simple_basic"
) {
  enum_plp_type = ENUM_PLP_TYPE$SIMPLE
  clust_inv_inputs = CLUST_INV_INPUTS[[config_type]]
  clust_inv_inputs[["start_dt"]] = start_dt
  clust_inv_inputs[["end_dt"]]   = end_dt
  
  common_inputs = getListFromObjs(clust_elem, clust_elem2, universe_tbl, 
                                  enum_plp_type, clust_inv_inputs)
  prefix        = "SIMP"
  spec_inputs   = getListFromObjs(prefix)
  
  # Dynamically register functions
  register_common_clust_invest_setting_factories(
    common_inputs, spec_inputs,
    function(common_inputs2, spec_inputs2) {  
      clust_trial         = common_inputs2$clust_trial
      clust_trial2        = common_inputs2$clust_trial2  
      universe            = common_inputs2$universe
      universe_tbl        = common_inputs2$universe_tbl
      clust_inv_inputs    = common_inputs2$clust_inv_inputs
      # Generate investment parameters
      clust_inv_param_tbl <- make_clust_inv_param_tbl(
        universe     = universe,
        ar_inf       = clust_inv_inputs[["ar_inf"]],
        wd_no        = clust_inv_inputs[["wd_no"]],#3,
        prob_infs    = clust_inv_inputs[["prob_infs"]],  #0.0,
        reb_freqs    = clust_inv_inputs[["reb_freqs"]], #c("1w", "2w", "eom"),
        universe_tbl = universe_tbl
      )

      config_tbl <- tibble(
        start_dt = clust_inv_inputs[["start_dt"]],#"2000-01-01", 
        end_dt   = clust_inv_inputs[["end_dt"]],#"2023-12-31",
        plp_type = clust_inv_inputs[["plp_type"]],#"simple", 
        reb_freqs = list(clust_inv_inputs[["reb_freqs"]]),
        wd_no    = clust_inv_inputs[["wd_no"]], 
        with_ew_port = FALSE, 
        lag_days = 1
      )
      
      function(clust_handler, tri_file_loader){
        clust_param_tbl <- clust_handler$load_params(clust_trial, remove_seq_id = TRUE)
        clust_all_param_tbl <- clust_param_tbl %>% 
          expand_grid(clust_inv_param_tbl) %>% 
          mutate(clust_trial = clust_trial)
        tri_tbl = tri_file_loader$load()
        getListFromObjs(clust_trial2, clust_all_param_tbl, config_tbl, tri_tbl)
      }
    }
  )
}

register_common_clust_invest_setting_factories <- function(
    common_inputs, spec_inputs, refister_func
) {
  clust_elem       = common_inputs$clust_elem
  clust_elem2      = common_inputs$clust_elem2  
  universe_tbl     = common_inputs$universe_tbl
  tri_file_loader  = common_inputs$tri_file_loader
  enum_plp_type    = common_inputs$enum_plp_type
  clust_inv_inputs = common_inputs$clust_inv_inputs
  
  # prefix         = spec_inputs$prefix
  band_strategy  = spec_inputs$band_strategy
  synthe_bs_type = spec_inputs$synthe_bs_type
  with_zero      = spec_inputs$with_zero
  
  enum_plp_type_name = enum_plp_type %>% pluck("name")
  nnn = if(is.null(universe_tbl)) 1 else(nrow(universe_tbl))
  
  # Split the data and dynamically generate strategies.
  for (iii in 1:nnn) {
    if(enum_plp_type_name == "simple") {
      clust_inv_type = paste0(clust_elem2, "_", iii)
      universe = universe_tbl$universe[[iii]]
    }else if(enum_plp_type_name %in% c("tsd", "synthe_bs_tsd", "mix_tsd")){
      clust_inv_type = clust_elem2
      universe = NULL
    }else{
      stop("Please set the folloting enum_plp_type: simple, tsd, synthe_bs_tsd, mix_tsd")
    }
    clust_trials <- CLUST_CONFIG_SETTING$ENUM_PARAM_TBL[[clust_elem]] %>% pull(clust_trial)
    for (clust_trial in clust_trials) {
      clust_trial2 <- paste0(clust_trial, "-", tolower(clust_inv_type))
      
      common_inputs2 = getListFromObjs(clust_trial, clust_trial2, clust_inv_type, clust_elem2,
                                       universe_tbl, universe, 
                                       tri_file_loader,
                                       # file_path, from_file_type,
                                       enum_plp_type,
                                       clust_inv_inputs
      )
      spec_inputs2   = getListFromObjs(band_strategy, synthe_bs_type, with_zero)
      # CLUST_INVEST_SETTING_FACTORY に動的に関数を登録
      CLUST_INVEST_SETTING_FACTORY[[clust_trial2]] <- {
        function(common_inputs2, spec_inputs2) {
          force(common_inputs2)  
          force(spec_inputs2)
          # 各戦略に応じた共通のロジック
          refister_func(common_inputs2, spec_inputs2) 
        }}(common_inputs2, spec_inputs2) 
    }
  }
}