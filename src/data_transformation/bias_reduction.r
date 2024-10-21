bias_reduction <- function(data) {
    data$Log_TotalVolume <- log(data$Total.Volume + 1)  # Se suma 1 para evitar log(0)
    return(data)
}