data_standardization <- function(data) {
    data_normalizada <- data
    data_normalizada$AveragePrice <- (data$AveragePrice - min(data$AveragePrice)) / 
                                (max(data$AveragePrice) - min(data$AveragePrice))

    # Estandarización (media 0, desviación estándar 1)
    data_estandarizada <- data
    data_estandarizada$Total.Volume <- scale(data$Total.Volume)

    return(data)
}