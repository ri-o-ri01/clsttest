create_rank_func_dict <- function(combinations,
                                  is_keys_origin_dict =
                                    list(trial = "zero", lmt_id = 0, wd_comb_id = 0)){
  
  is_keys = is_keys_origin_dict %>% names()
  
  create_rank_dict <- function(is_keys, column_name, rank_no = NULL,
                               tgt_exec_pct = NA, tgt_ar = NA){
    
    arrange_select_list <- function(data, is_keys, column_name, rank_no = NULL,
                                    tgt_exec_pct = NA, tgt_ar = NA) {
      filter_expr = NA
      if(!is.na(tgt_exec_pct)) filter_expr = "exec_pct >= " %+% tgt_exec_pct
      if(!is.na(tgt_ar)){
        if(!is.na(filter_expr)){
          filter_expr = filter_expr %+% " & " %+% "ar >= " %+% tgt_ar
        }else{
          filter_expr =  "ar >= " %+% tgt_ar
        }
      }
      if(is.na(filter_expr)) filter_expr = "1==1"
      # Convert the filtering condition string to an expression
      filter_condition <- rlang::expr(!!rlang::parse_expr(filter_expr))
      
      ret = data %>%
        filter(!!filter_condition)  %>% 
        # Arrange data in desc order based on the specified criteria(e.g., shr, sor)
        arrange(desc(!!sym(column_name))) %>%  
        select(all_of(is_keys), !!sym(column_name)) %>%  
        rename(value = !!sym(column_name))
      
      if(!is.null(rank_no)){
        ret = ret %>%
          # Truncate to 6 decimal places
          mutate(score_rounded = floor(value * 1e6) / 1e6) %>%
          slice_max(order_by =score_rounded,  n = rank_no) %>%
          select(- score_rounded)
      }
      if(nrow(ret) == 0){
        ret = is_keys_origin_dict %>%
          as_tibble() %>%
          mutate(value = 0)
      }
      
      return(ret %>% list())
    }
    
    function(data){
      arrange_select_list(data, is_keys, column_name, rank_no, tgt_exec_pct, tgt_ar)
    }
  }
  
  # Creates a list of rank functions identified by unique key names, based on specified criteria.
  rank_dict <- pmap(combinations, function(criteria, n_top, exec_pct_inf, ar_inf) {
    exec_pct_inf_str = if(is.na(exec_pct_inf)) "N" else exec_pct_inf
    ar_inf_str = if(is.na(ar_inf)) "N" else ar_inf
    key_parts <- Filter(Negate(is.na), list(criteria, n_top, exec_pct_inf_str, ar_inf_str))
    key_name <- paste(key_parts, collapse = "_")
    list(name = key_name, result = create_rank_dict(is_keys, criteria, n_top, exec_pct_inf, ar_inf))
  })
  
  rank_dict <- set_names(purrr::map(rank_dict, "result"), 
                         purrr::map(rank_dict, "name"))
  rank_dict
}
