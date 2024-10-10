with_hv_tbl = function(db_price_tbl, codes, width = 30){
  # Check if the necessary columns exist in the table
  map(codes, ~ check_column_in_tbl(db_price_tbl, .))
  
  # Calculate HV for each code (asset)
  hv_tbl_list <- lapply(codes, function(code) {
    
    # Calculate daily return on business dates (rate of return) for each asset
    ror_tbl <- db_price_tbl %>% 
      select(date, all_of(code)) %>% 
      drop_na() %>% # Remove non business dates
      mutate(d_ror = !!sym(code) / lag(!!sym(code), 1) - 1.0,
             name = code
             ) %>% 
      select(- all_of(code)) %>% 
      drop_na() #Remove first line
    
    # Use GroupbyChainOperator to calculate HV over the specified rolling window
    gcop <- GroupbyChainOperator$new(ror_tbl %>% mutate(temp="temp"))
    gcop$update_by_transform(
      c("temp"), tgt_names = c("d_ror"), 
      func_dict = list(function(x) {
        rollapply(x, width = width, 
                  calculate_hv, 
                  TT = 1, by.column = FALSE, align = "right", fill = NA)
      })
    )
    
    # Format the result by renaming columns and removing missing values
    hv_tbl <- gcop$merge_all_level_df() %>% 
      as_tibble() %>% 
      select(date, d_ror_1) %>% 
      rename(!!paste0(code, "_HV_", width, "D") := d_ror_1
             ) %>% 
      drop_na()
    
    return(hv_tbl)
  }) 
  
  db_price_tbl %>% 
    left_join(Reduce(function(x, y) full_join(x, y, by = "date"), hv_tbl_list),
              by = join_by(date)
              ) 
}
