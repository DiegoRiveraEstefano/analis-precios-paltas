library(ggplot2)

type_segmentation <- function(data) {
  ggplot(data, aes(x = type, y = AveragePrice, fill = type)) +
    geom_bar(stat = "identity") +
    ggtitle("Precio Promedio por Tipo de Aguacate") +
    xlab("Tipo") +
    ylab("Precio Promedio")

  ggplot(data, aes(x = "", y = Total.Volume, fill = type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    ggtitle("Proporción del Volumen Total por Tipo de Aguacate")
}
