# TriFileLoaderクラスの定義
TriFileLoader <- R6Class(
  "TriFileLoader",
  public = list(
    file_path = NULL,
    from_file_type = NULL,
    
    initialize = function(file_path, from_file_type) {
      self$file_path = file_path
      self$from_file_type = from_file_type
    },
    
    load = function() {
      if(self$from_file_type == "yf_path") {
        print(self$file_path)
        tri_tbl <- readRDS(self$file_path)
      } 
      # Implement this section if you have a different logic to 
      # retrieve market data
      # else if(self$from_file_type == "other_path") {
      #   tri_tbl <- create_some_logic(self$file_path)
      # } 
      else {
        stop("tri_tbl is empty")
      }
      return(tri_tbl)
    }
  )
)

