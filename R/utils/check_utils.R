# Check -------------------------------------------------------------------
check_column_in_tbl <- function(tbl, col_name) {
  if (!col_name %in% colnames(tbl)) {
    abort(glue::glue("The column '{col_name}' does not exist in the tibble."))
  }
}
