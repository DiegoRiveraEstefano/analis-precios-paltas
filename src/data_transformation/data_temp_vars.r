create_temp_vars <- function(data) {
  data$Month <- as.numeric(format(data$Date, "%m"))
  data$Quarter <- quarters(data$Date)
  return(data)
}
