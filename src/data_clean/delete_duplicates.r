delete_duplicates <- function(data) {
    # Eliminar duplicados
    return(data[!duplicated(data), ])
}