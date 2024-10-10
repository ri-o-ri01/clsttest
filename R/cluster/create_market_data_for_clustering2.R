#' Create Market Data for Clustering Analysis
#'
#' This function retrieves market data for specified symbols from Yahoo Finance using the `yfinance` 
#' Python library.
#' It processes the data by calculating key indicators such as Simple Moving Averages (SMA) and spreads, 
#' and prepares the data for clustering analysis.
#'
#' @param start_dt A date representing the start date for fetching market data. Default is "1999-01-04".
#' @param end_dt A date representing the end date for fetching market data. Default is "2023-12-31".
#' @param symbol_name_vec A named vector of symbol names. The keys are Yahoo Finance symbols, 
#' and the values are the corresponding column names for the returned tibble.
#'
#' @return A tibble containing the processed market data with calculated indicators 
#' such as 30-day SMA, 5-day/20-day SMA ratios, and yield spreads.
#' @details
#' - The function uses a fixed set of symbols including 
#' NKY, SPX, USDJPY, USGG2YR, USGG10YR, VIX, JT7203, SPY (SPX ETF), and AAPL.
#' - It retrieves market data from Yahoo Finance and processes it to calculate various indicators such as:
#'   - 30-day Simple Moving Averages (SMA) for VIX and USGG10YR.
#'   - Differences between current values and their SMAs.
#'   - Yield spread between USGG2YR and USGG10YR.
#'   - USDJPY 5-day and 20-day SMAs and their ratios.
#' - The function ensures that enough historical data is retrieved by 
#' subtracting 360 days from `start_dt` to calculate the indicators accurately.
#'
#' @examples
#' \dontrun{
#'  # Example usage:
#'  mkt_data_tbl <- create_market_data_for_clustering2(start_dt = as.Date("2000-01-01"), end_dt = as.Date("2023-12-31"))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import zoo
#' @export
create_market_data_for_clustering2 = function(
                                              start_dt = as.Date("2000-01-04"), 
                                              end_dt   = as.Date("2023-12-31"),
                                              symbol_name_vec = c(
                                                "^N225" = "NKY", 
                                                "^GSPC" = "SPX", 
                                                "JPY=X" = "USDJPY",
                                                "^IRX"  = "USGG2YR",
                                                "^TNX"  = "USGG10YR", 
                                                "^VIX"  = "VIX",
                                                "7203.T" = "JT7203",
                                                "SPY"    = "SPX_ETF",
                                                "AAPL"   = "AAPL"
                                              )
                                              ){
  
  
  # To calculate indicators(such as HV and SMA) at`start_dt`, we need historical data 
  # from a certain number of days before the actual start date. 
  # Subtracting 360 days ensures sufficient prior data.
  bef_start_dt = start_dt - 360
  price_tbl = create_price_tbl_from_python_yf(bef_start_dt, end_dt, symbol_name_vec) %>%
    select(date, map_chr(names(symbol_name_vec), ~symbol_name_vec[[.]]))
  
  mkt_data_tbl = price_tbl %>% 
    # Use previous day's value when the value is NA due to a holiday
    fill(everything(), .direction = "down") %>%
    with_hv_tbl(c("NKY",  "USDJPY", "JT7203")) %>%
    rename_with(.cols = where(is.numeric), .fn = toupper) %>% 
    mutate(VIX_SMA30D  = rollapply(VIX, width = 30, FUN = mean, align = "right", fill = NA),
           USGG10YR_SMA30D = rollapply(USGG10YR, width = 30, FUN = mean, align = "right",
                                       fill = NA),
           VIX_SMA30D_DIFF = VIX - VIX_SMA30D,
           USGG2YR_USGG10YR_DIFF = USGG2YR - USGG10YR,
           USGG10YR_SMA30D_DIFF = USGG10YR - USGG10YR_SMA30D,
           USDJPY_SMA5D = rollapply(USDJPY, width = 5, FUN = mean, align = "right",
                                    fill = NA),
           USDJPY_SMA20D = rollapply(USDJPY, width = 20, FUN = mean, align = "right",
                                     fill = NA),
           USDJPY_SMA5D20D_RATIO = USDJPY_SMA5D / USDJPY_SMA20D,
           NKY_SMA5D = rollapply(NKY, width = 5, FUN = mean, align = "right",
                                 fill = NA),
           NKY_SMA20D = rollapply(NKY, width = 20, FUN = mean, align = "right",
                                  fill = NA),
           NKY_SMA5D20D_RATIO = NKY_SMA5D / NKY_SMA20D,
           
    ) %>% filter(date >= start_dt)
  
  mkt_data_tbl
  
}
