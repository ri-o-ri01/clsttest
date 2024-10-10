make_clust_inv_param_tbl <- function(universe, 
                                     ar_inf = NA,
                                     wd_no = 3, 
                                     prob_infs = c(0.0, 0.8, 0.9),
                                     reb_freqs = c("1w", "2w", "eom"),
                                     universe_tbl = UNIVERSE_TBL) {
  clust_inv_param_tbl <- expand_grid(
    universe = universe,
    criteria = c("shr"),
    n_top = c(1),
    prob_inf = prob_infs,
    ar_inf   = ar_inf,
    reb_freq = reb_freqs,
    wd_no = wd_no
  ) %>%
    left_join(universe_tbl, by = join_by(universe))
  
  return(clust_inv_param_tbl)
  
}

execute_clust_invest_setting_factory_element = function(env, clust_trial2, arg_list){
  env_get_for_clust_inv <- function(env, name) {
    if (name %in% ls(env)) {
      return(get(name, envir = env, inherits = FALSE))
    } else {
      stop(paste("Object", name, "not found in environment"))
    }
  }
  
  func = env_get_for_clust_inv(CLUST_INVEST_SETTING_FACTORY, clust_trial2)
  # 実際に使われる引数だけを取り出す関数
  filter_args <- function(func, arg_list) {
    valid_args <- intersect(names(arg_list), names(formals(func)))
    arg_list[valid_args]
  }# 必要な引数だけを抽出
  filtered_arg_list <- filter_args(func, arg_list)
  
  #実行し、エラーが発生した場合はエラーメッセージを表示
  result <- tryCatch({
    do.call(func, filtered_arg_list)
  }, error = function(e) {
    message("Error in function execution: ", e$message)
    stop()
  })

  return(result) 
}

make_sim_pos_tbl = function(is_clust_tbl, w_bds_tbl, ref_date, 
                            o_date = "sim_o_reb_date", c_date = "sim_c_reb_date"){
  
  
  is_pos_prob_tbl = is_clust_tbl %>% rowwise() %>%
    mutate(date = list(make_weekdays(
      !!as.name(o_date), !!as.name(c_date)))) %>%
    select(date, prob) %>% unnest(date) %>% unique() %>% 
    mutate(pos=1)
  
  sim_pos_tbl = w_bds_tbl %>% 
    mutate(pos=0, prob=0) %>% 
    left_join(is_pos_prob_tbl, by = "date", suffix = c("", "_A")) %>%
    mutate(pos = if_else(is.na(pos_A), pos, pos_A),
           prob = if_else(is.na(prob_A), prob, prob_A),
           mkt_start_dt = date, 
           mkt_end_dt = lead(date, 1), 
    ) %>%
    # filter(date <= ref_date) %>%
    filter(mkt_end_dt <= ref_date) %>%
    select(date, pos, prob, mkt_start_dt, mkt_end_dt)
  sim_pos_tbl
}

