calc_cum_returns <- function(rets) {
  ret = purrr::map(1:length(rets), ~ PerformanceAnalytics::Return.cumulative(rets[1:.x])) %>% unlist()
  ret = 1.0 + ret
  ret
}

with_cumrets <- function(tbl_w, x_name, vars = NULL, prev_date = NULL) {
  if(is.null(vars)){
    ret_tbl = tbl_w %>%
      select(!!as.name(x_name), everything())
  }else{
    ret_tbl = tbl_w %>%
      select(!!as.name(x_name), all_of(vars))
  }
  ret_tbl2 = ret_tbl %>%
    modify_numeric(calc_cum_returns)
  if(!is.null(prev_date)){
    stopifnot(prev_date < min(tbl_w$date))
    ret_tbl2 = ret_tbl2 %>% 
      bind_rows(slice(., 1) %>%
                  mutate(date = prev_date,
                         across(where(is.numeric), ~ 1.0))
      ) %>% 
      arrange(date)
  }
  ret_tbl2
}

with_cumrets2 <- function(tbl_l, x_name, key_name, value_name, prev_date = NULL) {
  tbl_w = tbl_l %>% 
    select(x_name, key_name, value_name) %>% 
    pivot_wider(names_from = key_name, values_from = value_name)
  vars = tbl_w %>% select(- x_name) %>% colnames()
  with_cumrets(tbl_w, x_name, vars, prev_date)
}

