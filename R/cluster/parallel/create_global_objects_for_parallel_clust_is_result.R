create_global_objects_for_parallel_clust_is_result = function(
    keys_origin_dict, rule_param_tbl, all_w_bds_plp_tbl, 
    
    beg_start = as.Date("1999-01-04"), is_debug = F){
  variables = list()
  variables[["rule_param_tbl"]] = rule_param_tbl 
  variables[["keys_origin_dict"]] = keys_origin_dict
  variables[["all_w_bds_plp_tbl"]] = all_w_bds_plp_tbl
  variables[["is_debug"]] =  is_debug
  variables[["beg_start"]] =  beg_start

  f = function(row, variables){
   for (subfolder in c("backtest", "finance", "utils")) {
      list.files(here("R", subfolder), "\\.R$", full.names = TRUE) %>% 
        lapply(source)
    }
    
    rule_param_tbl = variables[["rule_param_tbl"]]
    keys_origin_dict = variables[["keys_origin_dict"]]
    all_w_bds_plp_tbl      = variables[["all_w_bds_plp_tbl"]]
    is_debug = variables[["is_debug"]] 
    beg_start = variables[["beg_start"]] 
    ret_tbl = row %>% 
      rowwise() %>% 
      mutate(
        sim_pos_dates = sim_pos_dates %>%
          mutate(use_prob = prob >= prob_inf,
                 pos_old = pos,
                 pos = pos * use_prob
          ) %>%
          list(),
        d_weight_tbl_s = sim_pos_dates %>% 
          select(-mkt_start_dt, -mkt_end_dt, -prob, -use_prob, -pos_old) %>% 
          bind_cols(map_dfc(codes, ~ sim_pos_dates$pos)) %>%
          rename_with(~ codes, starts_with("...")) %>%
          select(-pos)%>% 
          list(),
        d_ror_tbl_s = all_w_bds_plp_tbl %>% 
          select(date, codes) %>%
          list()
      )
    rm(all_w_bds_plp_tbl)
    gc()
    gc()
    ret_tbl = ret_tbl %>% 
      mutate(
        d_plp_tbl = calc_daily_port_plp_result(d_weight_tbl_s, d_ror_tbl_s, is_debug = T, c_rate =0.0) %>%
          pluck("d_plp_tbl") %>% select(-port) %>% 
          pivot_longer(cols = codes, names_to = "code",  values_to = "plp") %>% 
          list())
    if(!is_debug){
      ret_tbl = ret_tbl %>% 
        select(- any_of(c("sim_pos_dates", "d_weight_tbl_s", "d_ror_tbl_s")))
    }
    ret_tbl = ret_tbl %>% 
      mutate(
        bt_sche_tbl = tibble(
          type = "expand",
          criteria = criteria,
          n_top = n_top,
          start_dt = beg_start,
          end_dt   = ref_date,
          exec_pct_inf = NA,
          # ar_inf = NA,
          ar_inf = ar_inf ,
          e_rule_id = 1,
          cycle = 0,
          is_start = start_dt,
          is_end = end_dt,
          os_start = NA,
          os_end = NA
        ) %>% list(),
        insample_result = generate_insample_results(
          bt_sche_tbl, 
          rule_param_tbl,
          keys_origin_dict = keys_origin_dict,
          tsd_plp_tbl2 = d_plp_tbl
        ) %>% list()
      )
    if(!is_debug){
      ret_tbl = ret_tbl %>% 
        select(- any_of(c("d_plp_tbl", "bt_sche_tbl")))
    }
    ret_tbl = ret_tbl %>% 
      mutate(  
        bt_sche_rank_tbl = insample_result$bt_sche_rank_tbl %>% list()
      )
    if(!is_debug){
      ret_tbl = ret_tbl %>% 
        select(- any_of(c("insample_result")))
    }
    ret_tbl
  }
  
  getListFromObjs(f, variables)
  
}
