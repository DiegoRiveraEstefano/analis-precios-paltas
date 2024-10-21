variable_discretization <- function(data) {
    data$Price_Category <- cut(data$AveragePrice, 
                           breaks = c(0, 1, 1.5, 2), 
                           labels = c("Bajo", "Medio", "Alto"))
    return(data)
}