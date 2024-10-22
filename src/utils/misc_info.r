resume <- function(df) {
  return(summary(df))
}

mean_price <- function(df) {
  return(mean(df$AveragePrice))
}

total_volume_by_size <- function(df) {
  return(rowSums(df[, c("4046", "4225", "4770")]))
}

std_dev_price <- function(df) {
  return(sd(df$AveragePrice))
}

small_bag_percentage <- function(df) {
  return((df$Small.Bags / df$Total.Volume) * 100)
}

price_range <- function(df) {
  return(c(min(df$AveragePrice), max(df$AveragePrice)))
}