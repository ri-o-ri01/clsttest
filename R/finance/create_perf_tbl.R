create_perf_tbl = function(plp_tbl, tgt_cols, tgt_name,
                           indeces = "date", is_debug = F){
  perf_dict = list(
    ar = function(x) {
      RcppPerfCalc::annualized_return(x, 252)
    },
    sd = function(x) {
      RcppPerfCalc::annualized_sd(x, 252)
    },
    dd = function(x) {
      RcppPerfCalc::annualized_dd(x, 252)
    },
    mdd = function(x) {
      RcppPerfCalc::mdd(x)
    },
    plp_sum = sum,
    plp_n = function(x) {
      sum(x != 0.0, na.rm = TRUE)
    }
  )
  
  plp_tbl = plp_tbl %>% 
    arrange(desc(indeces))
  
  perf_gcop = GroupbyChainOperator$new(plp_tbl)
  perf_gcop$update_by_agg(
    tgt_cols, tgt_names = c(tgt_name),
    func_dict = perf_dict,
    indices = indeces,
    is_debug = is_debug
  )
  
  return(perf_gcop$get_output_df(1) %>%
           rename_with( ~ gsub(tgt_name %+% "_", "", .x)) %>%
           op_with_smpl_meta(mutate,
                             shr = ar / sd,
                             sor = ar / dd,
                             str = ar / abs(mdd)) %>% 
           op_with_smpl_meta(arrange, desc(shr))
  )
  
}

create_perf_tbl2 = function(plp_tbl, 
                            indeces = "date", is_debug = F){
  plp_tbl %>% 
    pivot_longer(cols = select(., - indeces) %>% colnames()) %>% 
    create_perf_tbl("name", "value")
}
