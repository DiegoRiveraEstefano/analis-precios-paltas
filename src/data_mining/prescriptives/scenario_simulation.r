library(EnvStats)
library(ggplot2)

scenario_simulation <- function(data) {
  # Simulación de precios y volúmenes futuros con Monte Carlo
  set.seed(123)
  precios_sim <- rnorm(
    1000,
    mean = mean(data$AveragePrice), sd = sd(data$AveragePrice)
  )
  volumen_sim <- rnorm(
    1000,
    mean = mean(data$Total.Volume), sd = sd(data$Total.Volume)
  )

  # Cálculo de ingresos simulados
  ingresos_sim <- precios_sim * volumen_sim

  ggplot(data.frame(ingresos_sim), aes(x = ingresos_sim)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    ggtitle("Distribución de Ingresos Simulados") +
    xlab("Ingresos") +
    ylab("Frecuencia")
}
