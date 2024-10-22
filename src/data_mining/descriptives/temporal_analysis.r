library(ggplot2)

temporal_analysis <- function(data) {
  ggplot(data, aes(x = Date, y = AveragePrice)) +
    geom_line(color = "blue", size = 1) +
    ggtitle("Evolución del Precio Promedio del Aguacate Hass") +
    xlab("Fecha") +
    ylab("Precio Promedio")
}
