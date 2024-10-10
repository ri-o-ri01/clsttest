ClusteringModel = R6Class(
  "ClusteringModel",
  
  public = list(
    
    mkt_tbl = NULL,
    param_list = NULL,
    start_dt = NULL,
    seed = NULL,
    clust_num = NULL,
    ref_name = NULL,
    save_graphs = NULL,
    ref_date = NULL,
    model_params = NULL,
    
    model_obj = NULL, 
    error_flag = FALSE,
    
    clust_result_tbl = NULL,
    is_clust_tbl = NULL,
    ref_clust_tbl = NULL,
    
    silhouette_score = NULL,
    
    initialize = function(mkt_tbl, param_list) {
      self$mkt_tbl <- mkt_tbl
      self$param_list <- param_list
      self$start_dt <- param_list$start_dt
      self$seed <- param_list$seed
      self$clust_num <- param_list$clust_num
      self$ref_name <- param_list$ref_name
      self$ref_date <- param_list$ref_date
      self$save_graphs <- param_list$save_graphs
      self$model_params <- param_list$model_params
    },
    
    filter_mkt_tbl = function(){
      self$mkt_tbl %>%
        filter(date <= self$ref_date)
    },
    
    make_input_data = function(){
      self$filter_mkt_tbl() %>%
        select(all_of(self$ref_name))
    },
    
    create_clust_result_tbl = function(cluster_assignments) {
      self$clust_result_tbl <- self$filter_mkt_tbl() %>% 
        select(date) %>% 
        mutate(
          category = paste0("c", cluster_assignments), 
          prob = 1.0,
          is_ref_date = FALSE
        ) %>% 
        mutate(no = row_number()) %>% 
        mutate(is_ref_date = no == nrow(.)) %>% 
        select(date, category, prob, is_ref_date)
    },
    
    create_is_clust_tbl = function(){
      self$is_clust_tbl = self$clust_result_tbl %>% 
        group_nest(category, is_ref_date) %>% 
        group_by(category) %>% 
        filter(n() == 2) %>% 
        unnest(cols = c(data)) %>% 
        ungroup() %>% 
        mutate(ref_date2 = max(date)) %>% 
        filter(!is_ref_date) %>% 
        group_nest(category, ref_date2) %>% 
        select(data) %>% unnest(data)
    },
    
    create_ref_clust_tbl = function(){
      self$ref_clust_tbl = self$clust_result_tbl %>% 
        filter(is_ref_date) %>% 
        select(date, category, prob) %>% 
        rename(date2 = date) %>% 
        rename_with(~ paste0("ref_", .x))
    },
    
    # シルエットスコアの計算
    calc_silhouette_score = function(data, cluster_assignments) {
      silhouette_score <- cluster::silhouette(cluster_assignments, dist(data))
      return(mean(silhouette_score[, 3]))  # シルエット係数の平均を返す
    },
    
    is_error = function() {
      self$error_flag
    },
    
    generate_results = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    },
    
    get_cluster_assignments = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    },
    
    get_cluster_centers = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    },
    
    get_cluster_sizes = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    },
    
    get_cluster_distances = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    },
    
    plot_clusters = function() {
      stop("This is an abstract method. Please implement in the subclass.")
    }
  )
)

KmeansModel <- R6Class(
  "KmeansModel",
  inherit = ClusteringModel,
  
  public = list(
    
    # クラスタリング結果を生成するメソッド
    generate_results = function() {
      set.seed(self$seed)  # 乱数シードの設定
      
      # データのフィルタリング
      data <- self$make_input_data()
      
      # K-meansクラスタリングの実行
      # self$model_obj = stats::kmeans(data, centers = self$clust_num)
      self$model_obj = tryCatch({
        stats::kmeans(data, centers = self$clust_num)
      }, error = function(e) {
        self$error_flag = TRUE
        message("Error in Kmeans: ", e$message)
        return(NULL)  # エラー時に NULL を返す
      })
      
      cluster_assignments = self$model_obj$cluster
      
      self$clust_result_tbl = self$create_clust_result_tbl(cluster_assignments)
      self$is_clust_tbl = self$create_is_clust_tbl()
      self$ref_clust_tbl = self$create_ref_clust_tbl()
      
      self$silhouette_score = self$calc_silhouette_score(data, cluster_assignments) 
      
      return(self)
    },
    
    # 各データのクラスタ割り当てを取得
    get_cluster_assignments = function() {
      return(self$model_obj$cluster)
    },
    
    # クラスタのセンター（重心）を取得
    get_cluster_centers = function() {
      return(self$model_obj$centers)
    },
    
    # 各クラスタに含まれるデータポイント数を取得
    get_cluster_sizes = function() {
      return(table(self$model_obj$cluster))
    },
    
    # クラスタ間の距離を取得
    get_cluster_distances = function() {
      centers <- self$model_obj$centers
      return(dist(centers))
    },
    
    # クラスタのプロット
    plot_clusters = function() {
      data <- self$make_input_data()
      plot(data, col = self$model_obj$cluster, main = "K-meansクラスタリング")
      points(self$model_obj$centers, col = 1:self$clust_num, pch = 8, cex = 2)
      
    }

  )
)

GmmModel <- R6Class(
  "GmmModel",
  inherit = ClusteringModel,
  
  public = list(
    
    # クラスタリング結果を生成するメソッド
    generate_results = function() {
      set.seed(self$seed)  # 乱数シードの設定
      
      # データのフィルタリング
      data <- self$make_input_data()
      
      modelNames = self$model_params$modelNames
      
      # GMMクラスタリングの実行
      # self$model_obj <- Mclust(data, 
      #                          G = self$clust_num, 
      #                          modelNames = modelNames)
      self$model_obj = tryCatch({
        Mclust(data, G = self$clust_num, modelNames = modelNames)
      }, error = function(e) {
        self$error_flag = TRUE
        message("Error in Gmm: ", e$message)
        return(NULL)  # エラー時に NULL を返す
      })
      
      cluster_assignments = self$model_obj$classification
      # クラスタリング結果のテーブル生成
      self$clust_result_tbl = self$create_clust_result_tbl(cluster_assignments)
      self$is_clust_tbl = self$create_is_clust_tbl()
      self$ref_clust_tbl = self$create_ref_clust_tbl()
      
      self$silhouette_score = self$calc_silhouette_score(data, cluster_assignments) 
      return(self)
    },
    
    # 各データのクラスタ割り当てを取得
    get_cluster_assignments = function() {
      return(self$model_obj$classification)
    },
    
    # クラスタのセンター（重心）を取得
    get_cluster_centers = function() {
      return(self$model_obj$parameters$mean)
    },
    
    # 各クラスタに含まれるデータポイント数を取得
    get_cluster_sizes = function() {
      return(table(self$model_obj$classification))
    },
    
    # クラスタ間の距離を取得
    get_cluster_distances = function() {
      centers <- self$model_obj$parameters$mean
      return(dist(t(centers)))  # GMMでは転置が必要
    },
    
    # クラスタのプロット
    plot_clusters = function() {
      plot(self$model_obj, what = "classification")
    }
  )
)
