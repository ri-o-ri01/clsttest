register_clust_config_setting <- function(cc_manager){
  ret = new.env()
  ret[["ENUM_PARAM_TBL"]] = new.env()
  for (exper_name in cc_manager$exper_param_tbl_list %>% names()) {
    temp_tbl = cc_manager$exper_param_tbl_list[[exper_name]]
    ret[["ENUM_PARAM_TBL"]][[exper_name]] = temp_tbl
    for (iii in 1:nrow(temp_tbl)) {
      clust_param_tbl = temp_tbl[iii, ]
      clust_trial = clust_param_tbl$clust_trial[[1]]
      ret[[clust_trial]] = getListFromObjs(clust_trial, clust_param_tbl)
    }
  }
  
  ret
}
