apply_parallel <- function(data, f, libraries = NULL, variables = NULL,
                           no_cores = detectCores(), timeout = NULL) {
  if(is.null(timeout)){
    cl <- makeCluster(no_cores)
  }else{
    cl <- makeCluster(no_cores, timeout)
  }
  variables_to_export = c()
  assign("f", f, envir = .GlobalEnv)
  # 追加の変数をエクスポート
  if (!is.null(variables)) {
    for (var in names(variables)) {
      assign(var, variables[[var]], envir = .GlobalEnv)
    }
    variables_to_export <- c(variables_to_export, names(variables))
  }
  clusterExport(cl, variables_to_export)
  # 'libraries' をエクスポート
  if (!is.null(libraries)) {
    assign("libraries", libraries, envir = .GlobalEnv)
    clusterExport(cl,c("libraries"))
    clusterEvalQ(cl, {
      lapply(libraries, library, character.only = TRUE)
    })
  }
  class_type = class(data)
  # データがtibbleの場合、リストに変換
  if (is.data.frame(data)) {
    data <- split(data, 1:nrow(data))
  }
  if(is.null(variables)){
    results <- parLapply(cl, data, f)
  }else{
    results <- parLapply(cl, data, f, variables)
  }
  # クラスタを停止
  stopCluster(cl)
  
  if("tbl_df" %in% class_type){
    ret = bind_rows(results)
  }else{
    ret = results
  }
  
  rm(data)
  rm(variables)
  rm(cl)
  gc()
  gc()
  return(ret)
 
}

# Parallel Computation ----------------------------------------------------

split_function = function(clust_param_date_tbl, N=NULL){
  b_tsd_plp_nest_tbl = clust_param_date_tbl
  #  b_tsd_plp_nest_tbl
  # # A tibble: 45,912 × 4
  #    clust ref_name  model_name date      
  #    <dbl> <list>    <chr>      <date>    
  #  1     3 <chr [2]> Mclust     2002-01-01
  #  2     3 <chr [2]> Mclust     2002-01-02
  if(is.null(N)){
    N = nrow(b_tsd_plp_nest_tbl) %/% 4000 + 1
  }
  b_tsd_plp_nest_tbl_list = b_tsd_plp_nest_tbl %>% 
    split_tibble(N=N)
  print("Split " %+% N)
  b_tsd_plp_nest_tbl_list
  
}

# 並列計算を実行する関数
execute_parallel_computation_on_splits <- function(data, split_function,  
                                                   add_libraries = NULL, 
                                                   no_cores = detectCores()
) {
  libraries = c("lubridate", "tidyverse", "assertr")
  if(!is.null(add_libraries)){
    libraries = c(libraries, add_libraries)
  }
  
  result_tbl <- tibble()
  data_split_list <- split_function(data)
  
  for (i in seq_along(data_split_list)) {
    tbl <- data_split_list[[i]]
    s <- Sys.time()
    class_type = class(tbl)
    print(class_type)

    tmp <- 
      apply_parallel(
        data = tbl,
        f = global_f,
        variables = global_variables,
        libraries = libraries,
        no_cores = no_cores 
      )
    
    e <- Sys.time()
    es <- e - s
    print(i)
    print(es)
    
    result_tbl <- bind_rows(result_tbl, tmp)
    gc()
    gc()
  }
  
  result_tbl
}

