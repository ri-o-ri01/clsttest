GroupbyResult <- R6Class("GroupbyResult",
  public = list(
    output_name = NULL,
    method = NULL,
    input_name = NULL,
    groups = NULL,
    indices = NULL,
    tgt_names = NULL,
    func_dict = NULL,
    funcs_name = NULL,
    output_df = NULL,
    output_column_dict = NULL,
    level = NULL,

    initialize = function(output_name, method, input_name, groups,
                          indices, tgt_names, func_dict, funcs_name,
                          output_df, output_column_dict, level) {
      self$output_name <- output_name
      self$method <- method
      self$input_name <- input_name
      self$groups <- groups
      self$indices <- indices
      self$tgt_names <- tgt_names
      self$func_dict <- func_dict
      self$funcs_name <- funcs_name
      self$output_df <- output_df
      self$output_column_dict <- output_column_dict
      self$level <- level
    },

    print = function(...) {
      make_groupby_names <- function(groups) {
        # 各要素をアンダースコアで分割し、各部分の最初の文字を取得して結合
        abbreviated_names <- sapply(groups, function(name) {
          parts <- strsplit(name, "_")[[1]]
          paste0(substr(parts, 1, 1), collapse = "")
        })

        # 結果をハイフンで結合
        paste(abbreviated_names, collapse = "-")
      }
      if (self$level == 0) {
        cat(paste0(self$output_name, "_", self$level))
      } else {
        if (is.null(self$output_name)) {
          groupby_names <- make_groupby_names(self$groups)
          values <- list(self$method, groupby_names, self$funcs_name, self$level)
          cat(paste(unlist(values), collapse = "_"))
        } else {
          cat(paste0(self$output_name, "_", self$level))
        }
      }
      invisible(self)
    }
  )
)
