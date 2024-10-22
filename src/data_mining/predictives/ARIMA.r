library(forecast)

ARIMA <- function(data) {
  # Ajuste del árbol de decisión para predecir el tipo de aguacate

  # Ajuste del modelo ARIMA
  serie <- ts(data$AveragePrice, frequency = 52) # Serie temporal semanal
  modelo_arima <- auto.arima(serie)

  # Pronóstico para las próximas 10 semanas
  pronostico <- forecast(modelo_arima, h = 10)

  # Visualización del pronóstico
  plot(pronostico)

  return(modelo_arima)
}
