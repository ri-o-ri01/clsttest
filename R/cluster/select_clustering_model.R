select_clustering_model <- function(param_list, mkt_tbl) {
  lib_name   <- param_list$lib_name
  model_name <- param_list$model_name
  
  if (lib_name == "Mclust" && model_name == "GMM") {
    return(GmmModel$new(mkt_tbl, param_list))  # Generate GMM model
  } else if (lib_name == "stats" && model_name == "K-means") {
    return(KmeansModel$new(mkt_tbl, param_list))  # Generate K-means model
  } else {
    stop(paste("No matching model found for library:", lib_name, 
               "and model:", model_name))
  }
}