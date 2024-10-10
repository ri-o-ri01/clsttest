register_clust_inv_plp_param_input_factory = function(experiments){
  for (exp in experiments) {
    clust_inv_param_tbl <- CLUST_INV_PLP_PARAM_INPUT_FACTORY[[paste0(exp, "_TBL")]]
    CLUST_INV_PLP_PARAM_INPUT_FACTORY[[exp]] <- {function(clust_inv_param_tbl){
      force(clust_inv_param_tbl)
      function(save_path, input_name) {
        
        CLUST_INV_PLP_PARAM_INPUT_FACTORY$common(
          clust_inv_param_tbl, save_path, input_name)
      }
    }}(clust_inv_param_tbl)
  }
}
