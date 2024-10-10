create_reb_sche_tbl <- function(w_bds_tbl, tgt_wd_no, reb_freq, lag_days = 1) {
  w_bds_tbl2 <- w_bds_tbl %>%
    # wd_noの追加
    group_by(year, week_no) %>%
    arrange(date) %>%
    mutate(wd_no = row_number()) %>%
    ungroup() %>%
    # is_wd_noの追加
    mutate(is_wd_no = ifelse(days == 4 & tgt_wd_no == 5 & wd_no == 4, TRUE,
                             ifelse(wd_no == tgt_wd_no, TRUE, FALSE))) %>% 
    
    # monthの追加
    mutate(month = month(date)) %>%
    # is_reb_dateの追加
    group_by(year, month) %>%
    arrange(year, month, week_no) %>%
    mutate(
      temp_id = row_number(), 
      is_reb_date = case_when(
        reb_freq %in% c("1w", "1w+") ~ is_wd_no,
        reb_freq %in% c("2w", "2w+") ~ is_wd_no & (week_no %% 2 == 1),
        reb_freq == "eom" ~ temp_id == max(is_wd_no * temp_id)
      )) %>%
    ungroup() %>%
    arrange(year, date) %>% 
    # ref_dateの追加
    mutate(ref_date = lag(date, lag_days), 
           date_1_lag = if(grepl("\\+", reb_freq)) lag(date, 0) else lag(date, 1)
    ) %>% 
    rename(h_start = date) %>% 
    select(week_no, days, wd_no, is_wd_no, is_reb_date, ref_date, h_start, date_1_lag) %>% 
    filter(is_reb_date) %>% 

    mutate(h_end = lead(date_1_lag)) %>% 
    drop_na(ref_date) %>% 
    select(- date_1_lag) %>% 
    drop_na(h_end)
  
  return(w_bds_tbl2)
}

create_reb_sche_nest_tbl = function(w_bds_tbl, config_tbl){
  reb_freqs = config_tbl$reb_freqs[[1]]
  plp_type  = config_tbl$plp_type[[1]]
  wd_no     = config_tbl$wd_no[[1]]
  lag_days  = config_tbl$lag_days[[1]]
  reb_sche_nest_tbl = tibble(reb_freq = reb_freqs, wd_no = wd_no) %>% rowwise() %>% 
    mutate(reb_sche = create_reb_sche_tbl(w_bds_tbl, wd_no, reb_freq, lag_days = lag_days) %>% 
             select(ref_date, h_start, h_end) %>% 
             list()) %>% 
    ungroup()
  reb_sche_nest_tbl
}