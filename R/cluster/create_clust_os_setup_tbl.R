create_clust_os_setup_tbl = function(clust_is_result_tbl, clust_all_param_tbl, 
                                     clust_trade_w_bds_tbl, all_tri_tbl, 
                                     c_rate = 0.0001,
                                     id_name = "cl_id"
){
  
  clust_os_setup_tbl = clust_is_result_tbl %>% rowwise() %>%   
    mutate(n_sche = nrow(bt_sche_rank_tbl) ) %>% 
    unnest(bt_sche_rank_tbl) %>% rowwise() %>% 
    mutate(date = make_weekdays(h_start, h_end) %>% list()) %>% 
    unnest(date) %>% 
    filter(date %in% clust_trade_w_bds_tbl$date) %>%
    mutate(pos = 1.0 / n_sche) %>% 
    select(clust_all_param_tbl %>% colnames(), code, date, pos) %>% 
    arrange(!!as.name(id_name), date) 
  
  all_trade_w_bds_plp_tbl = create_d_ror_tbl_from_all_tri_tbl(clust_trade_w_bds_tbl, all_tri_tbl) %>% 
    drop_na()
  
  clust_os_result_tbl = clust_os_setup_tbl %>% 
    select(!!as.name(id_name), code, date, pos) %>% 
    group_nest(!!as.name(id_name)) %>%  rowwise() %>%
    mutate(codes = data$code %>% unique() %>% list(),
           d_weight_tbl_s = clust_trade_w_bds_tbl %>% 
             left_join(data %>% 
                         pivot_wider(names_from = "code", values_from = "pos"),
                       by = join_by(date)
             ) %>% 
             filter(date <= max(data$date)) %>% 
             with_na_replace(col_names = codes) %>% 
             select(date, codes) %>% 
             # set pos for instrument "Zero" to 0 to ignore the effect of c_rate 
             # since this represents cash without carry and cost,
             {if ("ZERO" %in% colnames(.)) mutate(., ZERO = 0) else .} %>% 
             slice(1:(n() - 1)) %>%
             list(),  
  
           d_plp_tbl = calc_daily_port_plp_result(d_weight_tbl_s, all_trade_w_bds_plp_tbl, c_rate = c_rate) %>%
             list(),
           
    ) %>% 
    ungroup()
  
}
