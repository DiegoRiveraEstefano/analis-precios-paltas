library(dbscan)
library(ggplot2)


DBSCAN <- function(data) {
  # Aplicación de DBSCAN
  dbscan_result <- dbscan(data, eps = 0.5, minPts = 2)

  # Agregamos los resultados al dataframe
  data$dbscan_cluster <- as.factor(dbscan_result$cluster)

  # Visualización de clusters y ruido
  ggplot(data, aes(
    x = AveragePrice, y = Total.Volume, color = dbscan_cluster
  )) +
    geom_point(size = 3) +
    ggtitle("DBSCAN: Clustering basado en densidad") +
    xlab("Precio Promedio") +
    ylab("Volumen Total")
}
