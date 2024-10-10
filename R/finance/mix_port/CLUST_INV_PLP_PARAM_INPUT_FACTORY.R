CLUST_INV_PLP_PARAM_INPUT_FACTORY <- new.env()

CLUST_INV_PLP_PARAM_INPUT_FACTORY$common <- function(clust_inv_param_tbl, 
                                                     save_path, 
                                                     input_name
) {
  plp_type = ENUM_PLP_TYPE$CLUST_INV %>% pluck("name")
  getListFromObjs(clust_inv_param_tbl, save_path, 
                  # clust_inv_alloc, 
                  plp_type, input_name) 
}







