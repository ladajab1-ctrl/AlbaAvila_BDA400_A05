# Standard Deviation (population: divides by n, as in the assignment formula)
# Base R only - no external libraries (does not use sd()).
#
# data   : numeric vector
# Returns: single numeric value
stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- numeric(0)
  for (element in data) {
    diff_values <- c(diff_values, element - mean_value)
  }

  # Calculate the squared differences
  squared_diff <- numeric(0)
  for (element in diff_values) {
    squared_diff <- c(squared_diff, element * element)
  }

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
