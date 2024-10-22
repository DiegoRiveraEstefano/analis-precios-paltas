library(dplyr)

variable_encoding <- function(data) {
  data <- data %>%
    mutate(region_code = as.numeric(factor(region)))
  data <- data %>%
    mutate(type_code = as.numeric(factor(type)))
  return(data)
}
