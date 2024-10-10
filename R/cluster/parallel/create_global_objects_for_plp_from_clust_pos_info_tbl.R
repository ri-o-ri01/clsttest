create_global_objects_for_plp_from_clust_pos_info_tbl = function(w_bds_tbl){
  variables = list()
  variables[["w_bds_tbl"]] = w_bds_tbl
  variables[["make_sim_pos_tbl"]] =make_sim_pos_tbl
  f = function(row, variables){
    list.files(here("R", "cluster"), "\\.R$", full.names = TRUE) %>% 
      lapply(source)
    
    w_bds_tbl         = variables[["w_bds_tbl"]]
    make_sim_pos_tbl  = variables[["make_sim_pos_tbl"]]
    ret_tbl =  tryCatch({
      row %>% rowwise() %>%
        mutate(
          sim_pos_dates = make_sim_pos_tbl(
            is_clust_tbl, w_bds_tbl, ref_date, "sim_h_start", "sim_h_end") %>% list(),
          is_success = T
        ) 
    }, error = function(e) {
      # Add emputy data when there is no past data with the same condition on ref_date
      row %>% rowwise() %>%
        mutate(sim_pos_dates = w_bds_tbl %>% 
                 mutate(pos=0, prob=0, 
                        mkt_start_dt = date, 
                        mkt_end_dt = lead(date, 1)) %>% 
                 filter(date <  ref_date) %>% 
                 select(date, pos, prob, mkt_start_dt, mkt_end_dt) %>% 
                 list(),
               is_success = F)  
    })
    ret_tbl
  }
  
  getListFromObjs(f, variables)
}