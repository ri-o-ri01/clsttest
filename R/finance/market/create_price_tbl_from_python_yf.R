#' Create a Price Table from Yahoo Finance Data using yfinance in Python
#'
#' This function retrieves market data from Yahoo Finance using the Python library `yfinance`.
#' The data includes various financial instruments, such as 
#' NKY, SPX, USDJPY, USGG2YR, USGG10YR, VIX, JT7203, SPY (SPX ETF), and AAPL.
#' The data is processed and returned as a wide-format tibble.
#' The table includes the adjusted closing prices (Adj Close) of the selected financial instruments.
#'
#' @param start_dt A string representing the start date for fetching data. Default is "1999-01-04".
#' @param end_dt A string representing the end date for fetching data. Default is "2023-12-31".
#' @param symbol_name_vec A named vector of symbol names. The keys are Yahoo Finance symbols, and the values are the corresponding column names for the returned tibble.
#'
#' @return A wide-format tibble containing the date and adjusted closing prices (Adj Close) for the specified financial instruments.
#' @details 
#' - The function uses the `reticulate` package to run Python code from R.
#' - It creates a temporary Python virtual environment, installs `yfinance`, and then downloads the data.
#' - The temporary environment is removed after the function completes.
#' - The data returned includes the adjusted closing prices (`Adj Close`) rather than the regular closing prices (`Close`).
#'
#' @examples
#' \dontrun{
#'  # Example usage:
#'  create_price_tbl_from_python_yf(start_dt = "2000-01-01", end_dt = "2023-12-31")
#' }
#'
#' @import reticulate
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
create_price_tbl_from_python_yf = function(start_dt = "1999-01-04", 
                                           end_dt   = "2023-12-31",
                                           
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
  temp_path    <- tempdir()
  temp_env_dir <- file.path(temp_path, "r-yfinance-env")
  
  if (dir.exists(temp_env_dir)) {
    unlink(temp_env_dir, recursive = TRUE, force = TRUE)
    message("Temporary virtual environment has been completely removed.")
  }
  # Create a new virtual environment (using tempdir if not reusing)
  tryCatch({
    system(paste("python -m venv", temp_env_dir))
    system(paste(temp_env_dir, "/Scripts/pip install yfinance", sep = ""))
  }, error = function(e) {
    message("An error occurred during virtual environment creation or yfinance installation: ", e$message)
  })
  
  
  # Activate the virtual environment
  use_virtualenv(temp_env_dir, required = TRUE)
  
  # Convert R symbol names to Python dictionary
  symbol_name_dict <- r_to_py(as.list(symbol_name_vec))
  
  # Python code for fetching asset prices
  py_run_string("
import yfinance as yf
def load_asset_prices(start_date, end_date, symbol_to_name):
    index_data = {}

    for yf_symbol, name in symbol_to_name.items():
        data = yf.download(yf_symbol, start=start_date, end=end_date)
        index_data[name] = data

    return index_data
")
  
  # Fetching the asset prices
  result <- py$load_asset_prices(start_dt, end_dt, symbol_name_dict)
  
  # Process the result into a tidy tibble
  result2 = 
    map2(result, names(result), function(x,y){ 
      x %>% 
        mutate(date = as.Date(rownames(x))) %>%   # Add date as a column
        mutate(name = y) %>%                      # Add symbol name as a column
        rename_with(~ sub(".*\\.", "", .), .cols = -date) %>% # Clean up column names
        as_tibble()
    }) %>% 
    bind_rows() %>% 
    select(c("date", "Adj Close", "name")) %>%
    pivot_wider(names_from = "name", values_from = "Adj Close") %>% 
    arrange(date)
  
  # Clean up the virtual environment directory (not the entire tempdir)
  unlink(temp_env_dir, recursive = TRUE, force = TRUE)
  
  return(result2)
}



