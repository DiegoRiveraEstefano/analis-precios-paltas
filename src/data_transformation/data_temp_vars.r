create_temp_vars <- function(data) {
    data$Month <- format(data$Date, "%m")
    data$Quarter <- quarters(data$Date)
    return(data)
}