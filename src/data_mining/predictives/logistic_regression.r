library(ggplot2)


logistic_regression <- function(data) {
  # Ajuste del modelo de regresión logística
  modelo_logistico <- glm(
    type_code ~ AveragePrice + Total.Volume,
    data = data, family = "binomial"
  )

  # Resumen del modelo
  summary(modelo_logistico)

  # Predicción y visualización
  data$predicted_type <- ifelse(
    predict(
      modelo_logistico,
      type_code = "response"
    ) > 0.5, 1, 0
  )

  ggplot(data, aes(
    x = AveragePrice, y = Total.Volume, color = predicted_type
  )) +
    geom_point(size = 3) +
    ggtitle("Regresión Logística: Predicción del Tipo de Aguacate") +
    xlab("Precio Promedio") +
    ylab("Volumen Total")
}
