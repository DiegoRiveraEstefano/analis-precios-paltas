library(ggplot2)


multiple_linear_regression <- function(data) {
  # Ajuste del modelo de regresión lineal múltiple
  modelo_lineal <- lm(
    AveragePrice ~ Total.Volume + year + region + type,
    data = data
  )

  # Resumen del modelo
  summary(modelo_lineal)

  # Visualización del ajuste entre valores reales y predichos
  data$predicted_price <- predict(modelo_lineal, data)
  ggplot(data, aes(x = AveragePrice, y = predicted_price)) +
    geom_point(color = "blue") +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    ggtitle("Regresión Lineal: Precio Real vs Predicho") +
    xlab("Precio Real") +
    ylab("Precio Predicho")
  # return(modelo_lineal)
}
