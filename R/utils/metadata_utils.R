op_with_smpl_meta <- function(tbl, operation_func, ...) {
  operation_name <- paste0(
    deparse(substitute(operation_func)),
    "-",
    paste(deparse(substitute(list(...))), collapse="_"))
  op_with_meta(tbl, operation_name, operation_func, ...)
}

op_with_meta <- function(tbl, operation_name, operation_func, ...) {
  # 操作を適用（追加の引数を含む）
  tbl <- operation_func(tbl, ...)
  
  # メタデータを追加
  tbl <- add_metadata(tbl, operation_name, ...)
  tbl
}

add_metadata <- function(tbl, operation, ...) {
  # 関数名を取得は不可能なので、operationにその情報を込める
  
  # 現在のメタデータを取得
  current_metadata <- attr(tbl, "metadata")
  
  # 新しいメタデータエントリを作成
  new_entry <- list(
    operation = operation
  )
  # メタデータを更新または作成
  if (is.null(current_metadata)) {
    current_metadata <- list()
  }
  current_metadata[[length(current_metadata) + 1]] <- new_entry
  
  # 更新したメタデータをtibbleに設定
  attr(tbl, "metadata") <- current_metadata
  tbl
}


get_metadata <- function(tbl){
  attributes(tbl)$metadata
}

set_metadata <- function(tbl, set_name){
  new_entry <- list(
    operation = set_name
  )
  attr(tbl, "metadata") <- new_entry
  return(tbl)
}