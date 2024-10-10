create_simple_enum_hier_clust_invest_tbl <- function(
    enum_param_tbl, universe_tbl, exper_prefix, 
    from_file_type = "threshold_input_path") {
  n = nrow(universe_tbl)
  expand_grid(
    clust_trial = enum_param_tbl %>% pull(clust_trial),
    clust_inv_type = map_chr(1:n, ~paste0(exper_prefix, "_", .))
  ) %>%
    mutate(clust_trial2 = paste0(clust_trial, "-", tolower(clust_inv_type)),
           from_file_type = from_file_type
    )
  
  universe_tbl %>% 
    mutate(id = row_number()) %>% 
    expand_grid(enum_param_tbl) %>% 
    mutate(
      clust_inv_type = paste0(exper_prefix, "_", id),
      clust_trial2 = paste0(clust_trial, "-", tolower(clust_inv_type)),
      from_file_type = from_file_type
    ) %>% 
    select(clust_trial, clust_inv_type, clust_trial2, from_file_type, securities)
}
