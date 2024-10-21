install.packages(ggplot2)
library(ggplot2)

resume <- function(data) {
    ggplot(data, aes(x = AveragePrice)) + 
    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
    ggtitle("Distribución del Precio Promedio del Aguacate Hass") +
    xlab("Precio Promedio") + ylab("Frecuencia")
}