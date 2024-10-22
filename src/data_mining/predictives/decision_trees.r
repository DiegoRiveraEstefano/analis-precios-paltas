library(rpart)
library(rpart.plot)

decision_trees <- function(data) {
  # Ajuste del árbol de decisión para predecir el tipo de aguacate
  modelo_arbol <- rpart(
    type ~ AveragePrice + Total.Volume + year,
    data = data, method = "class"
  )

  # Visualización del árbol de decisión
  rpart.plot(modelo_arbol)

  # Predicción con el árbol de decisión
  data$predicted_tree <- predict(modelo_arbol, data, type = "class")

  return(modelo_arbol)
}
