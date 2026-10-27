# Stochastic RSI (StochRSI)
# Base R only - no external libraries.
# Depends on rsi() from rsi.R and sma() from sma.R
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

# data     : numeric vector of prices
# period   : RSI period
# k_period : smoothing period for %K
# d_period : smoothing period for %D
# Returns: list(k_line, d_line)
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0) {
    # Not enough data to compute RSI (length(data) <= period)
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(valid)
    max_rsi <- max(valid)
    if (max_rsi == min_rsi) {
      k_values <- ifelse(is.na(rsi_values), NA_real_, 0)   # flat RSI -> avoid 0/0
    } else {
      k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
    }
  }

  # Calculate the %K line (StochRSI smoothed with an SMA of k_period)
  k_line <- if (length(k_values) >= k_period) sma(k_values, k_period) else rep(NA_real_, length(k_values))

  # Calculate the %D line (SMA of %K over d_period)
  d_line <- if (length(k_line) >= d_period) sma(k_line, d_period) else rep(NA_real_, length(k_line))

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
