group_by_year_and_region <- function(data) {
  data_agrupada <- data %>%
    group_by(year, region) %>%
    summarise(
      Avg_Price = mean(AveragePrice),
      Total_Volume = sum(Total.Volume)
    )
  return(data_agrupada)
}
