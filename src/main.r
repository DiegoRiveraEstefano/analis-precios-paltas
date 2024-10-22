source("src/utils/install_packages.r")
source("src/utils/load_functions.r")
source("src/utils/misc_info.r")

# part 0: package, functions and data load

# install all package need for the project
pkg_load(packages = c(
  "dbscan", "ggplot2", "rpart",
  "neuralnet", "forecast", "lpSolve", "EnvStats", "rpart.plot", "dplyr"
))


library(ggplot2)
library(dplyr)

# load data from a .csv file
data <- read.csv("data/avocado.csv", fileEncoding = "utf-8")

# part 0: data schemas and presentation

print(
  # columns names
  colnames(data)
)

print(
  # rows count
  nrow(data)
)

print(
  # columns count
  ncol(data)
)

print(head(data))

print(str(data))

# unique data from regions
print(unique(data$region))

# unique data from years
print(unique(data$year))

# unique data from types
print(unique(data$type))

# part 1: data clean
load_sources("src/data_clean/")
data <- delete_nulls(data)
data <- delete_duplicates(data)
# data <- data_scaling(data)
data <- date_format(data)

print(str(data))

# part 2: data transformation
load_sources("src/data_transformation/")

data <- data_standardization(data)
# data <- variable_discretization(data)
data <- create_temp_vars(data)
data <- bias_reduction(data)
data <- variable_encoding(data)

print(str(data))

# part 3: data minig

# part 3.1 descriptives
load_sources("src/data_mining/descriptives")
print(eda(data))
print(type_segmentation(data))
print(analysis_by_region(data))
print(temporal_analysis(data))
print(correlation_analysis(data))

# part 3.2 clustering
load_sources("src/data_mining/clustering")

numeric_data <- data[, c(
  "X", "AveragePrice", "Total.Volume", "X4046", "X4225", "X4770",
  "Total.Bags", "Small.Bags", "XLarge.Bags", "Log_TotalVolume"
)]
print(k_means_clustering(numeric_data))
print(hierarchical_clustering(numeric_data))
print(DBSCAN(numeric_data))

# part 3.3: predictives
load_sources("src/data_mining/predictives")

# print(ARIMA(numeric_data)) # no ejecutar por alto consumo de recursos

print(multiple_linear_regression(data))
print(neural_networks(data))
print(logistic_regression(data))
print(decision_trees(data))


# part 3.4: prescriptives

load_sources("src/data_mining/prescriptives")
print(decision_analysis(data))
print(resource_optimization(data))
print(scenario_simulation(data))


# part 4: math operations

# part 4.1: Media del Precio Promedio:
mean_price <- function(df) {
  return(mean(df$AveragePrice))
}
cat("Media del Precio Promedio:", mean_price(data), "\n")


# part 4.2: Volumen total vendido (suma de volúmenes por tamaño):
total_volume_by_size <- function(df) {
  return(base::rowSums(df[, c("X4046", "X4225", "X4770")], na.rm = TRUE))
}
data$total_volume_size <- total_volume_by_size(data)
cat(
  "Volumen total vendido (suma de volúmenes por tamaño): ",
  head(data$total_volume_size)
)


# part 4.3: Desviación estándar del Precio Promedio:
std_dev_price <- function(df) {
  return(sd(df$AveragePrice))
}
cat("Desviación estándar del Precio Promedio:", std_dev_price(data), "\n")


# part 4.4: Porcentaje de ventas por bolsas pequeñas respecto al volumen total:
small_bag_percentage <- function(df) {
  return((df$Small.Bags / df$Total.Bags) * 100)
}
data$small_bag_pct <- small_bag_percentage(data)
cat(
  "Porcentaje de ventas por bolsas pequeñas respecto al volumen total: ",
  head(data$small_bag_pct)
)


# part 4.5: Máximo y mínimo del Precio Promedio:
price_range <- function(df) {
  return(c(min(
    df$AveragePrice,
    na.rm = TRUE
  ), max(df$AveragePrice, na.rm = TRUE)))
}
cat("Rango de Precio Promedio (mín, máx):", price_range(data), "\n")

resumen_numericas <- function(data, variables_numericas) {
  data %>%
    select(all_of(variables_numericas)) %>%
    summary()
}
cat("resumen_numericas", resumen_numericas(data, c(
  "AveragePrice", "Total.Volume", "X4046", "X4225", "X4770",
  "Total.Bags", "Small.Bags", "XLarge.Bags"
)), "\n")


# part 5: Vectores, Dataframes and plots

# part 5.1: Vectores
fechas <- seq.Date(
  from = as.Date("2015-01-01"),
  to = as.Date("2020-12-31"), by = "day"
)
precios <- runif(length(fechas), min = 1, max = 5)
volumen <- round(runif(length(fechas), min = 100, max = 1000))

# part 5.2: DataFrames
df1 <- data.frame(
  Date = fechas, AveragePrice = precios, TotalVolume = volumen
)
df2 <- data.frame(
  Date = fechas, AveragePrice = runif(length(fechas), min = 1, max = 5)
)

df_final <- rbind(data, df1, df2)
head(df_final)
