library(ggplot2)

analysis_by_region <- function(data) {
  ggplot(data, aes(x = region, y = Total.Volume, fill = type)) +
    geom_bar(stat = "identity") +
    ggtitle("Volumen Total por Región y Tipo de Aguacate") +
    xlab("Región") +
    ylab("Volumen Total") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggplot(data, aes(x = region, y = AveragePrice)) +
    geom_boxplot(fill = "lightblue") +
    ggtitle("Variación del Precio Promedio por Región") +
    xlab("Región") +
    ylab("Precio Promedio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
