create_bt_sche_rank_tbl<- function(is_tsd_perf_nest_tbl, rank_dict,  bt_sche_tbl,
                                   is_se_keys = c("is_start", "is_end")
                                   
){
  
  is_rank_result_tbl = is_tsd_perf_nest_tbl %>%
    op_with_smpl_meta(rowwise) %>%
    op_with_smpl_meta(mutate, across(all_of(c("data")), rank_dict, .names = "{.fn}")) %>%
    op_with_smpl_meta(select, - all_of(c("data")))
  
  bt_sche_rank_tbl = bt_sche_tbl %>%
    op_with_smpl_meta(inner_join,
                      is_rank_result_tbl,
                      by = is_se_keys) %>%
    op_with_smpl_meta(rowwise) %>%
    op_with_smpl_meta(mutate,
                      tgt = paste0(
                        criteria,"_", n_top,
                        if_else(!is.na(exec_pct_inf), paste0("_", exec_pct_inf), "_N"),
                        if_else(!is.na(ar_inf), paste0("_", ar_inf), "_N")
                      )) %>%
    op_with_smpl_meta(mutate, rank_data = list(get(tgt))) %>%
    op_with_smpl_meta(select, e_rule_id, cycle, tgt,
                      is_start, is_end, os_start, os_end, rank_data) %>%
    op_with_smpl_meta(unnest, cols = c(rank_data))
  
  bt_sche_rank_tbl
  
}