data_scaling <- function(data) {
    data_scaled <- data
    data_scaled[, c("AveragePrice", "Total.Volume", "4046", "4225", "4770")] <- scale(data[, c("AveragePrice", "Total.Volume", "4046", "4225", "4770")])
    return(data_scaled)
}