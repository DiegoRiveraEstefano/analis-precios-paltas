library(dplyr)

group_by_year_and_region <- function(data) {
  data_agrupada <- data %>%
    group_by(year, region) %>%
    summarise(
      Avg_Price = mean(AveragePrice),
      Total_Volume = sum(Total.Volume)
    )
  return(data_agrupada)
}

group_by_month <- function(data) {
  data_agrupada <- data %>%
    group_by(Month, year) %>%
    summarise("sum_Volume" = sum(Total.Volume))
  return(data_agrupada[, c("sum_Volume", "Month")])
}

group_by_region <- function(data) {
  data_agrupada <- data %>%
    group_by(region) %>%
    mutate("sum_volumen" = sum(Total.Volume))
  return(data_agrupada[, c("sum_volumen", "region")])
}

total_income_per_hass_size <- function(data, size) {
  return(
    data$AveragePrice * data[paste("X", str(size), sep = "")]
  )
}

total_income <- function(data, size) {
  return(
    data$AveragePrice * data$Total.Volume
  )
}

total_income_per_volumen <- function(data, size) {
  return(
    data$Total.Volume / data$AveragePrice
  )
}
