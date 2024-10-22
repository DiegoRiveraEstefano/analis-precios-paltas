library(dplyr)

create_temp_vars <- function(data) {
  data$Month <- as.numeric(format(data$Date, "%m"))
  data$Quarter <- quarters(data$Date)
  data <- data %>%
    mutate(
      most_selled_size = case_when(
        X4046 > X4225 & X4046 > X4770 ~ "small",
        X4225 > X4046 & X4225 > X4770 ~ "normal",
        X4770 > X4046 & X4770 > X4225 ~ "large",
        TRUE ~ "normal"
      )
    )
  return(data)
}
