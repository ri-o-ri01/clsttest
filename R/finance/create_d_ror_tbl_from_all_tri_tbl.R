create_d_ror_tbl_from_all_tri_tbl = function(date_tbl, all_tri_tbl, use_zero=F){
  
  all_tri_tbl_l = all_tri_tbl %>% 
    pivot_longer(cols = -date, names_to = "code")
  codes = all_tri_tbl_l$code %>% unique()
  all_w_bds_plp_tbl  = date_tbl %>% 
    arrange(date) %>% 
    mutate(mkt_start_dt = date, mkt_end_dt = lead(date)) %>% 
    select(date, mkt_start_dt, mkt_end_dt) %>% 
    expand_grid(tibble(code = codes)) %>% 
    left_join(all_tri_tbl_l %>% 
                select(date, code, value) %>% 
                rename(mkt_start_dt = date, start_value = value), 
              by = join_by(mkt_start_dt, code)) %>% 
    left_join(all_tri_tbl_l %>% 
                select(date, code, value) %>% 
                rename(mkt_end_dt = date, end_value = value),
              by = join_by(mkt_end_dt, code)) %>% 
    mutate(plp = end_value / start_value - 1.0) %>% 
    select(mkt_end_dt, code, plp) %>% 
    rename(date = mkt_end_dt) %>% 
    pivot_wider(names_from = "code", values_from = "plp") %>% 
    with_na_replace(col_names = codes)
  if(use_zero){
    all_w_bds_plp_tbl = all_w_bds_plp_tbl %>% 
      mutate(ZERO = 0.0)
  }
  
  
  new_tbl = all_w_bds_plp_tbl[1, ] %>% 
    mutate(date = date_tbl$date[[1]]) %>% 
    mutate(across(where(is.numeric), ~ 0))
  
  ret_tbl = new_tbl %>% 
    bind_rows(all_w_bds_plp_tbl)
  ret_tbl  
}