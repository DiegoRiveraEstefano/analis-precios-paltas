date_format <- function(data) {
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  return(data)
}
