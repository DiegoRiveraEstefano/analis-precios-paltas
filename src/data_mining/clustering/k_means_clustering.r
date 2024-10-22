library(ggplot2)

k_means_clustering <- function(data) {
  # Aplicación de K-means con 3 clusters
  set.seed(123) # Semilla para reproducibilidad
  kmeans_result <- kmeans(data, centers = 3, nstart = 25)

  # Agregamos los clusters al dataframe original
  data$cluster <- as.factor(kmeans_result$cluster)

  # Visualización de los clusters con ggplot2
  ggplot(
    data, aes(x = AveragePrice, y = Total.Volume, color = cluster)
  ) +
    geom_point(size = 3) +
    ggtitle("K-means Clustering: Precio vs Volumen") +
    xlab("Precio Promedio") +
    ylab("Volumen Total")
}
