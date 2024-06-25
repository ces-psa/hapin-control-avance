# Instala y carga el paquete readxl

library(readxl)

# Lee el archivo Excel
boletas <- read_excel("C:/Users/aramirez/Downloads/boletas.xlsx")


# Define una función para extraer los IDs de muestra y el número de caja
extraer_info <- function(bloque) {
  numero_caja <- as.character(bloque[grepl("Numero de caja", bloque$`Fecha colecta  boleta campo`), "44911"])
  ids_muestra <- as.vector(unlist(bloque[!grepl("Numero de caja", bloque$`Fecha colecta  boleta campo`), -c(1:6)]))
  return(data.frame(id_muestra = ids_muestra, contenedor = numero_caja))
}

# Aplica la función a cada bloque
resultados <- lapply(bloques, extraer_info)

# Combina los resultados en un único marco de datos
resultado_final <- do.call(rbind, resultados)

# Muestra el resultado final
print(resultado_final)

# Muestra el resultado final
print(resultado_final)

resultado_final %>% filter(!is.na(id_muestra)) %>% mutate(largo=str_length(id_muestra)) %>% filter(
  largo>1
) %>% 
  View()
