source("src/utils/install_packages.r")
source("src/utils/load_functions.r")
source("src/utils/misc_info.r")


# part 0: package, functions and data load

# install all package need for the project
pkg_load(packages = c(
  "dbscan", "ggplot2", "rpart",
  "neuralnet", "forecast", "lpSolve", "EnvStats", "rpart.plot", "dplyr"
))

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
data <- delete_duplicates(data)
data <- delete_nulls(data)
data <- data_scaling(data)
data <- date_format(data)

print(str(data))

# part 2: data transformation
load_sources("src/data_transformation/")

data <- data_standardization(data)
data <- variable_discretization(data)
data <- create_temp_vars(data)
data <- bias_reduction(data)

print(str(data))

# part 3: data minig

# part 3.1 descriptives
load_sources("src/data_mining/descriptives")
print(type_segmentation(data))
print(type_segmentation(data))
print(type_segmentation(data))
print(type_segmentation(data))



# part 3.2 clustering

# part 3.3 predictives

# part 3.4 prescriptives
