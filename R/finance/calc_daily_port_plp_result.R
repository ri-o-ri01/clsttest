calc_daily_port_plp_result = function(d_weight_tbl_s, d_ror_tbl_s,
                                      tgt_codes = NULL,
                                      c_rate = 0.0001,
                                      is_debug = FALSE
){
  if(is.null(tgt_codes)){
    tgt_codes = d_weight_tbl_s %>% select(- date) %>% colnames()
  }
  d_ror_tbl_s2 = d_ror_tbl_s %>%
    mutate(mkt_start_dt = lag(date, 1),
           mkt_end_dt   = date,
           date = mkt_start_dt
    ) %>%
    select(date, mkt_start_dt, mkt_end_dt, everything()) %>%
    drop_na(mkt_start_dt)
  
  d_ror_tbl_s3 = d_ror_tbl_s2 %>%
    filter(mkt_start_dt %in% d_weight_tbl_s[["date"]])
  
  all_dates = c(d_ror_tbl_s3$mkt_start_dt , d_ror_tbl_s3$mkt_end_dt) %>%
    unique() %>%
    sort()
  
  sche_tbl = tibble(date = all_dates) %>%
    left_join(d_ror_tbl_s2 %>% select(date, mkt_start_dt, mkt_end_dt),
              by = join_by(date))
  
  d_weight_mtrx = d_weight_tbl_s %>%
    select(tgt_codes) %>%
    as.matrix()
  d_ror_mtrx = d_ror_tbl_s3 %>%
    select(tgt_codes) %>%
    as.matrix()
  d_weight_mtrx
  
  rownames(d_weight_mtrx) = d_weight_tbl_s[["date"]]
  
  n_w = (nrow(d_weight_mtrx) - 1)
  
  d_weight_mtrx_prev = matrix(0, 1, ncol(d_weight_mtrx)) %>%
    rbind(d_weight_mtrx[1:(nrow(d_weight_mtrx) - 1), , drop = FALSE])
  d_diff_weight_mtrx = abs(d_weight_mtrx - d_weight_mtrx_prev)
  d_tcost_mtrx       = (d_diff_weight_mtrx * c_rate) %>% rbind(matrix(0, 1, ncol(d_weight_mtrx)))
  
  d_ror_mtrx2    = matrix(0, 1, ncol(d_weight_mtrx)) %>% rbind(d_ror_mtrx)
  d_weight_mtrx2 = matrix(0, 1, ncol(d_weight_mtrx)) %>% rbind(d_weight_mtrx)
  d_clean_plp_mtrx = (d_ror_mtrx2 * d_weight_mtrx2)
  d_plp_mtrx = d_clean_plp_mtrx - d_tcost_mtrx
  
  d_plp_tbl = d_plp_mtrx %>%
    as_tibble() %>%
    mutate(date = all_dates,
           port = apply(d_plp_mtrx, 1, sum)) %>%
    select(date,  everything())
  
  d_clean_plp_tbl = d_clean_plp_mtrx %>%
    as_tibble() %>%
    mutate(date = all_dates,
           port = apply(d_clean_plp_mtrx, 1, sum)) %>%
    select(date,  everything())
  
  d_cost_tbl = d_tcost_mtrx %>%
    as_tibble() %>%
    mutate(date = all_dates) %>%
    select(date,  everything())
  
  if(is_debug){
    return(getListFromObjs(d_plp_tbl, d_clean_plp_tbl, d_cost_tbl, sche_tbl, d_ror_tbl_s3))
  }else{
    return(d_plp_tbl %>% select(date, port))
    
  }
  
  
}