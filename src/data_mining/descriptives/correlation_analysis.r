library(ggplot2)

correlation_analysis <- function(data) {
  correlation <- cor(data$AveragePrice, data$Total.Volume)
  ggplot(data, aes(x = Total.Volume, y = AveragePrice)) +
    geom_point(color = "red") +
    ggtitle("Relación entre Volumen Total y Precio Promedio") +
    xlab("Volumen Total") +
    ylab("Precio Promedio")
  return(correlation)
}
