#' Split a Large Tibble
#'
#' This function splits a large tibble into smaller chunks based on the specified column or equally into N parts if no column is specified.
#'
#' @param tbl A tibble to be split.
#' @param select_col An optional string specifying the column name to split the tibble by its unique values. If NULL, the tibble is split into N equal parts.
#' @param N An integer indicating the number of chunks to split the tibble into when select_col is NULL. Defaults to 10.
#'
#' @return A list of tibbles, each representing a chunk of the original tibble. If select_col is specified, each chunk corresponds to a unique value in the specified column. If select_col is NULL, the tibble is split into N equal parts.
#'
#' @examples
#' # Create a sample tibble
#' sample_tbl <- tibble(
#'   id = seq_len(100),
#'   category = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   value = rnorm(100)
#' )
#'
#' # Split the tibble into 5 equal parts (select_col is NULL)
#' split_by_id <- split_tibble(sample_tbl, N = 5)
#'
#' # Split the tibble by the 'category' column
#' split_by_category <- split_tibble(sample_tbl, select_col = "category")
#'
#' @export
split_tibble <- function(tbl, select_col = NULL, N = 10) {
  # N=1の場合はtblをそのまま返す
  if (N == 1) {
    return(list(tbl))
  }
  
  # select_colとNがともにNULLの場合のエラーチェック
  if (is.null(select_col) && is.null(N)) {
    stop("select_col and N cannot be both NULL.")
  }
  
  # select_colがNULLの場合、tblにidをつけてN分割
  if (is.null(select_col)) {
    tbl$id <- seq_len(nrow(tbl))
    split_tbl <- split(tbl, cut(tbl$id, N, labels = FALSE))
  } else {
    # select_colがtblの列名に含まれるかチェック
    if (!(select_col %in% names(tbl))) {
      stop("select_col is not a valid column name in tbl.")
    }
    
    # select_colに基づいてtblを分割
    split_tbl <- split(tbl, tbl[[select_col]])
  }
  return(split_tbl)
}

split_tibble_by_columns <- function(tibble, N, key_col="date") {
  cols <- colnames(tibble)
  other_cols <- setdiff(cols, key_col)
  split_cols <- split(other_cols, ceiling(seq_along(other_cols) / (length(other_cols) / N)))
  
  split_tibbles <- lapply(split_cols, function(cols) {
    select(tibble, all_of(key_col), all_of(cols))
  })
  
  return(split_tibbles)
}

#' Replace all NA and NaN with some value
#'
#'
#' @importFrom dplyr %>% mutate_all
#'
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' tbl = tibble(x = c(1,NA, 3), y = c(NaN,3, NA))
#' tbl %>% with_na_replace(0)
#' tbl %>% with_na_replace(1)
#' tbl %>% with_na_replace(1, c("x"))
with_na_replace = function(tbl, replace = 0, col_names = NULL){
  if(is.null(col_names)){
    tbl %>%
      mutate_all(list(function(x) ifelse(is.na(x),replace,x)))
  }else{
    tbl %>%
      mutate_each(list(function(x) ifelse(is.na(x),replace,x)), col_names)
  }
}

conv_to_tribble_strings <- function(data) {
  # 列名の取得
  col_names <- names(data)
  # 列名を~でプリフィックスする
  tribble_cols <- paste0("~", col_names, collapse = ", ")
  
  # 各行のデータを文字列に変換
  rows <- apply(data, 1, function(row) {
    # 値を適切にエスケープ
    row_vals <- sapply(row, function(x) {
      if(is.character(x)) paste0('"', x, '"') else x
    })
    paste(row_vals, collapse = ", ")
  })
  
  # 全行を結合
  all_rows <- paste(rows, collapse = ",\n")
  
  # tribble形式で出力
  tribble_code <- sprintf("tibble::tribble(\n%s,\n%s\n)", tribble_cols, all_rows)
  cat(tribble_code)
}

filter_with_check_null <- function(tbl, col_name, filter_value, operator) {
  # フィルタ条件をクエリとして作成
  condition <- rlang::parse_expr(glue::glue("{col_name} {operator} filter_value"))
  # filter_valueがNULLならフィルタを適用せず、そうでない場合に適用
  tbl %>%
    {if (is.null(filter_value)) . else filter(., !!condition)}

}

modify_numeric = function(data, f){
  func = function(x) if(is.numeric(x)) f(x) else x
  modify(data, func )
}