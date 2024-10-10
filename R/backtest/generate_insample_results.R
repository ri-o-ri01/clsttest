generate_insample_results <- function(bt_sche_tbl, 
                                      rule_param_tbl,
                                      keys_origin_dict = list(
                                        trial = "zero", lmt_id = 0, wd_comb_id = 0),
                                      value_name = "plp", 
                                      
                                      tsd_plp_tbl2 = NULL,
                                      is_exec_result_tbl = NULL,
                                      N=10){
  
  keys = keys_origin_dict %>% names()
  keys2 = c(keys, "is_start", "is_end")
  is_sche_tbl = bt_sche_tbl %>%
    distinct(is_start, is_end)
  
  tsd_plp_tbl2 = tsd_plp_tbl2 %>%
    op_with_smpl_meta(ungroup) %>%
    op_with_smpl_meta(with_na_replace, 0, value_name) %>%
    op_with_smpl_meta(select, keys, date, value_name) %>%
    op_with_smpl_meta(arrange, date)
  
  is_plp_sche_nest_tbl  = is_sche_tbl %>% rowwise() %>%
    mutate(
      data = tsd_plp_tbl2 %>%
        filter(is_start <= date & date <= is_end) %>%
        list()
    )  %>%
    ungroup()
  is_tsd_perf_tbl = create_is_perf_tbl(is_plp_sche_nest_tbl, keys, value_name, N=N) %>% 
    with_na_replace(col_names = c("shr", "sor", "str"))
  
  rank_dict = create_rank_func_dict(rule_param_tbl, keys_origin_dict)

  if(!is.null(is_exec_result_tbl)){
    is_exec_result_tbl = is_tsd_perf_tbl %>%
      # Add the execution count
      op_with_smpl_meta(left_join, is_exec_result_tbl %>%
                          select(all_of(keys2), exec_pct),
                        by = keys2
      ) 
  }else{
    is_exec_result_tbl = is_tsd_perf_tbl
  }
  is_tsd_perf_nest_tbl = is_exec_result_tbl %>% 
    op_with_smpl_meta(group_nest, is_start, is_end)
  
  bt_sche_rank_tbl = create_bt_sche_rank_tbl(is_tsd_perf_nest_tbl, rank_dict, bt_sche_tbl)
  
  return(getListFromObjs(is_tsd_perf_tbl, bt_sche_rank_tbl,
                         tsd_plp_tbl2, is_plp_sche_nest_tbl, is_exec_result_tbl
                         
  ))
}