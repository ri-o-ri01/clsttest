create_instruments_return_data <- function(mkt_data_tbl, securities, date_col = "date") {
  # Step 1: リターン計算
  eq_ror_tbl <- mkt_data_tbl %>% 
    select(all_of(c(date_col, securities))) %>% 
    with_ror(date_col, securities)
  
  # Step 2: TRI計算
  eq_tri_tbl <- eq_ror_tbl %>% 
    mutate(
      across(all_of(securities), list(
        TRI_LONG = ~ .,  # 元のリターンをそのまま使用
        TRI_SHORT = ~ -.)  # ショートポジションの場合は符号を反転
      )
    ) %>% 
    with_cumrets(date_col, unlist(lapply(securities, function(x) {
      paste0(x, c("_TRI_LONG", "_TRI_SHORT"))
    }))) %>% 
    mutate(ZERO = 1)
  
  # eq_ror_tblとeq_tri_tblをリストで返す
  return(list(eq_ror_tbl = eq_ror_tbl, eq_tri_tbl = eq_tri_tbl))
}
