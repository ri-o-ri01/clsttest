ClustInvestConfigManager <- R6::R6Class("ClustInvestConfigManager",
  public = list(
    universe_tbl_list = NULL,  # テーブルリストを保持
    
    initialize = function() {
      self$universe_tbl_list <- list(
        SIMP_UNIVERSE_TBL = tribble(
          ~universe,       ~codes,
          "TP",            c("TP_TRI_LONG", "TP_TRI_SHORT", "ZERO"),
          "TP_LONG_ONLY",  c("TP_TRI_LONG", "ZERO"),
          "NK",            c("NK_TRI_LONG", "NK_TRI_SHORT", "ZERO"),
          "NK_LONG_ONLY",  c("NK_TRI_LONG", "ZERO"),
          "NT_RATIO",      c("NT_RATIO_TRI_LONG", "NT_RATIO_TRI_SHORT", "ZERO"),
          "NT_RATIO_LONG_ONLY", c("NT_RATIO_TRI_LONG", "ZERO"),
          "NT_RATIO_SHORT_ONLY", c("NT_RATIO_TRI_SHORT", "ZERO"),
          "NK_TP_ALL",     c("TP_TRI_LONG", "TP_TRI_SHORT", "ZERO", 
                             "NK_TRI_LONG", "NK_TRI_SHORT", 
                             "NK_TP_TRI_LONG_LV1", "NK_TP_TRI_SHORT_LV1",
                             "NT_RATIO_TRI_LONG", "NT_RATIO_TRI_SHORT"),
          "NK_TP_ALL2",    c("TP_TRI_LONG", "TP_TRI_SHORT", "ZERO", 
                             "NK_TRI_LONG", "NK_TRI_SHORT", 
                             "NK_TP_TRI_LONG_LV1", "NK_TP_TRI_SHORT_LV1", 
                             "NT_RATIO_TRI_LONG_LV3", "NT_RATIO_TRI_SHORT_LV3")
        ),
        SIMP_UNIVERSE_TBL2 = tribble(
          ~universe,       ~codes,
          "TP_LONG_ONLY",  c("TP_TRI_LONG", "ZERO"),
          "NK_LONG_ONLY",  c("NK_TRI_LONG", "ZERO"),
          "NT_RATIO_LONG_ONLY", c("NT_RATIO_TRI_LONG", "ZERO"),
          "NT_RATIO_SHORT_ONLY", c("NT_RATIO_TRI_SHORT", "ZERO")
        ),
        SIMP_UNIVERSE_TBL3 = tribble(
          ~universe,       ~codes,
          "TP_LONG_NK_SHORT",  c("TP_TRI_LONG", "NK_TRI_SHORT", "ZERO"),
          "NK_LONG_TP_SHORT",  c("NK_TRI_LONG", "TP_TRI_SHORT", "ZERO")
        ),
        SIMP_UNIVERSE_TBL4 = tribble(
          ~universe,         ~codes,
          "JY_CURNCY",       c("JY_TRI_LONG", "JY_TRI_SHORT", "ZERO"),
          "JY_CURNCY_LONG",  c("JY_TRI_LONG", "ZERO"),
          "JY_CURNCY_SHORT", c("JY_TRI_SHORT", "ZERO")
        )
      )
    },
    
    # ユニバーステーブルの取得メソッド
    get_universe_tbl = function(tbl_name) {
      if (!is.null(self$universe_tbl_list[[tbl_name]])) {
        return(self$universe_tbl_list[[tbl_name]])
      } else {
        stop("Table not found.")
      }
    },
    
    add_universe = function(tbl_name, universe, codes, securities = NA) {
      if (is.null(self$universe_tbl_list[[tbl_name]])) {
        self$create_universe_tbl(tbl_name, universe, codes, securities)
      } else {
        self$append_universe_tbl(tbl_name, universe, codes, securities)
      }
    },
    
    create_universe_tbl = function(tbl_name, universe, codes, securities) {
      self$universe_tbl_list[[tbl_name]] <- tibble(
        universe = universe, codes = list(codes), securities = list(securities))
    },
    
    append_universe_tbl = function(tbl_name, universe, codes, securities) {
      if (universe %in% self$universe_tbl_list[[tbl_name]]$universe) {
        warning(paste("The universe", universe, "already exists. No row added."))
      } else {
        self$universe_tbl_list[[tbl_name]] <- self$universe_tbl_list[[tbl_name]] %>%
          add_row(universe = universe, codes = list(codes), securities = list(securities))
      }
    }
  )
)
