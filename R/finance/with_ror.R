with_ror =  function(tbl_s, x_name, vars = NULL, remove_fst = F, is_log = T) {
  if (is.null(vars)) {
    ret_tbl <- tbl_s %>%
      select(!!as.name(x_name), everything())
  } else {
    ret_tbl <- tbl_s %>%
      select(!!as.name(x_name), all_of(vars))
  }
  
  if(is_log){
    ret_tbl <- ret_tbl %>%
      mutate(across(where(is.numeric), ~ (log(. / lag(.)))))
  }else{
    ret_tbl <- ret_tbl %>%
      mutate(across(where(is.numeric), ~ (. / lag(.) - 1.0)))
  }
  # remove_fstがTRUEの場合はNAを削除、FALSEの場合はNAを0で置き換え
  if (remove_fst) {
    ret_tbl <- ret_tbl %>%
      drop_na()
  } else {
    ret_tbl <- ret_tbl %>%
      mutate(across(where(is.numeric), ~ replace_na(., 0)))
  }
  return(ret_tbl)
}

