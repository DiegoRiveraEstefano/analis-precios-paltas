data_index <- function(data) {
  data$id <- 1:nrow(data)
  return(data)
}
