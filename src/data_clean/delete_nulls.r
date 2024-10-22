delete_nulls <- function(data) {
  data$AveragePrice[
    is.na(data$AveragePrice)
  ] <- median(data$AveragePrice, na.rm = TRUE)
  data$Total.Volume[
    is.na(data$Total.Volume)
  ] <- mean(data$Total.Volume, na.rm = TRUE)
  return(na.omit(data))
}
