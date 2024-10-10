ClusteringConfigManager <- R6Class(
  "ClusteringConfigManager",
  public = list(
    
    exper_param_tbl_list = NULL,
    
    initialize = function() {
      self$exper_param_tbl_list <- new.env()
    },
    
    set_mclust_exper_param_tbl_list = function(exper_name, ref_names, 
                                               model_param_tbl) {
      ret_tbl = model_param_tbl %>% 
        expand_grid(ref_name = ref_names)
      self$exper_param_tbl_list[[exper_name]] = ret_tbl %>%
        mutate(clust_trial = paste0(tolower(exper_name), "_", row_number())) %>%
        rowwise() %>%
        mutate(comb_ref_name = ref_name %>% str_c(collapse = "@")) %>%
        ungroup()

    }
  )
)


