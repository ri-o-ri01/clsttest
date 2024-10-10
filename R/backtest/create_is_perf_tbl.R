create_is_perf_tbl = function(is_plp_sche_nest_tbl, tgt_cols, tgt_name, N=10){
  is_tsd_perf_tbl = tibble()
  system.time({
    add_variables=list()
    add_variables[["tgt_cols"]] = tgt_cols
    add_variables[["tgt_name"]] = tgt_name
    
    is_plp_sche_tbl_list = is_plp_sche_nest_tbl %>% split_tibble(N=N)
    
    func_dict = list(
      perf = function(x, add_variables){
        add_variables[["tgt_cols"]] -> tgt_cols
        add_variables[["tgt_name"]] -> tgt_name
        create_perf_tbl(x, tgt_cols, tgt_name) %>% list()
      }
    )
    
    for(iii in  1:length(is_plp_sche_tbl_list)){
      system.time({
        y = is_plp_sche_tbl_list[[iii]]
        gcop = GroupbyChainOperator$new(y)
        gcop$update_by_map(groups = c("is_start", "is_end", "id"), tgt_names = c("data"),
                           func_dict = func_dict,
                           use_parallel = F,
                           add_variables = add_variables
        )
        is_tsd_perf_tbl = is_tsd_perf_tbl %>%
          bind_rows(gcop$get_output_df(1))
        print("Finish " %+% iii)
        rm(gcop)
        rm(y)
        gc()
        gc()
      })
    }
  })
  is_tsd_perf_tbl %>%
    op_with_smpl_meta(unnest) %>%
    op_with_smpl_meta(select, - id)
}
