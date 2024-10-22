data_scaling <- function(data) {
  data_scaled <- data
  data_scaled[, c(
    "AveragePrice", "Total.Volume", "X4046", "X4225", "X4770"
  )] <- scale(
    data[, c("AveragePrice", "Total.Volume", "X4046", "X4225", "X4770")]
  )
  return(data_scaled)
}
