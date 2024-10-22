library(neuralnet)

neural_networks <- function(data) {
  # Ajuste de la red neuronal para predecir el precio promedio
  set.seed(123)
  modelo_red <- neuralnet(
    AveragePrice ~ Total.Volume + year + region_code,
    data = data, hidden = 3, linear.output = TRUE
  )

  # Visualización de la red neuronal
  plot(modelo_red)

  # Predicción con la red neuronal
  data$predicted_nn <- neuralnet::compute(
    modelo_red, data[, c("Total.Volume", "year", "region_code")]
  )$net.result

  return(modelo_red)
}
