GmmStrategyInsampleSimulator  = R6Class(
  "GmmStrategyInsampleSimulator ",
  
  public = list(
    
    mkt_tbl = NULL,
    ref_mkt_tbl = NULL,
    trade_mkt_tbl = NULL,
    
    cluster_num   = NULL,
    
    initialize = function(mkt_tbl, ref_mkt_tbl = NULL
                          
    ){
      self$mkt_tbl = mkt_tbl
      self$ref_mkt_tbl = ref_mkt_tbl
    },
    
    make_results = function(cluster_num, tgt_ref_names, save_graphs=F,
                            seed = 255
                           
    ){
      set.seed(seed)
      if(is.null(self$ref_mkt_tbl)){
        ref_mkt_tbl   = self$mkt_tbl %>% select(c("date", tgt_ref_names))
      }else{
        ref_mkt_tbl   = self$ref_mkt_tbl
      }
      mod1 <- Mclust(self$mkt_tbl %>% select(tgt_ref_names), # データ
                     G = cluster_num, # コンポーネント数
                     modelNames = "VVI" # モデルの分散・共分散行列の構造の指定 (VVIは制約のない通常のGMM)
      )

      ## グラフ
      col_names = "c" %+% 1:cluster_num
     
      prob_tbl_g = mod1$z %>% 
        as_tibble() %>% 
        setNames(col_names) %>% 
        mutate(date = ref_mkt_tbl[["date"]]) %>% 
        pivot_longer(cols = - date, names_to = "category", values_to = "prob")
      # クラスタリング結果サマリー
      gmm_prob_tbl = ref_mkt_tbl %>% 
        mutate(category = "c" %+% mod1$classification) %>% 
        left_join(prob_tbl_g, by = c("date", "category")) %>% 
        mutate(no = row_number()) %>% 
        mutate(is_ref_date = no == nrow(.)) %>% 
        select(date, category, prob, is_ref_date) 
      
      if(save_graphs){
        g_gmm_prob = prob_tbl_g %>% 
          compare_vars("date", "category", "prob", is_facet = T)
        g_classification_func = function(){plot(mod1, what = "classification")} 
        
        return(getListFromObjs(gmm_prob_tbl, mod1, 
                               g_gmm_prob, g_classification_func))
      }else{
        return(getListFromObjs(gmm_prob_tbl, mod1))
      }
    }

  )
  
)

