calculate_hv <- function(log_returns, TT = 252) {
  # Calculate the standard deviation of the logarithmic returns
  sigma <- sd(log_returns, na.rm = TRUE)
  # Annualize the volatility
  hv <- sigma * sqrt(TT)
  return(hv)
}