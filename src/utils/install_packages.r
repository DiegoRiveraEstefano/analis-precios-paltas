pkg_load <- function(packages = "favourites") {
  if (length(packages) == 1L && packages == "favourites") {
    packages <- c(
      "dbscan", "ggplot2", "rpart",
      "neuralnet", "forecast", "lpSolve", "EnvStats", "rpart.plot", "dplyr"
    )
  }

  packagecheck <- match(packages, utils::installed.packages()[, 1])

  packagestoinstall <- packages[is.na(packagecheck)]

  if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall,
      repos = "http://cran.csiro.au"
    )
  } else {
    print("All requested packages already installed")
  }

  for (package in packages) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE, quietly = TRUE)
    )
  }
}
