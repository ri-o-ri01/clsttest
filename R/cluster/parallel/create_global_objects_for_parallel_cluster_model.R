create_global_objects_for_parallel_cluster_model = function(mkt_data_tbl){
  f = function(row, variables){
    source(paste0(here("R", "cluster", "class"), "/ClusteringModel.R"))
    source(paste0(here("R", "cluster"), "/select_clustering_model.R"))
    mkt_data_tbl = variables[["mkt_data_tbl"]] 
    param_list = row %>% as_list() %>% 
      modify_at("ref_name", purrr::flatten_chr)
    obj = select_clustering_model(param_list, mkt_data_tbl) 
    obj$generate_results()
    if(obj$is_error()){
      ret_tbl = row %>% mutate(is_err = T) 
    }else{
      ret_tbl = row %>% rowwise() %>%
        mutate(
          obj           = list(obj),      
          is_clust_tbl  = obj$is_clust_tbl %>% list(),
          ref_clust_tbl = obj$ref_clust_tbl %>% list()) %>%
        unnest(ref_clust_tbl) %>%
        assertr::verify(ref_date == ref_date2) %>%
        select(-ref_date2) %>% 
        mutate(is_err = F)
    }
    return(ret_tbl)
  }
  variables  = list()
  variables[["mkt_data_tbl"]] = mkt_data_tbl
  getListFromObjs(f, variables)
}

