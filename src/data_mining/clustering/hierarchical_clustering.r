library(ggplot2)

hierarchical_clustering <- function(data) {
  # Cálculo de la matriz de distancias
  dist_matrix <- dist(data, method = "euclidean")

  # Aplicación de clustering jerárquico
  hc <- hclust(dist_matrix, method = "average")

  # Visualización del dendrograma
  plot(hc,
    labels = data$region, main = "Dendrograma de Clustering Jerárquico",
    xlab = "Región", ylab = "Distancia"
  )
}
