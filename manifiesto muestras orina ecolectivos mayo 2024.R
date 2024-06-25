library(dplyr)
library(readxl)
library(stringr)

# Leer el archivo Excel
datos_brutos <- read_excel("C:/Users/aramirez/Downloads/boletas_depurado.xlsx", col_names = FALSE) %>%
  janitor::clean_names() %>%
  mutate_all(as.character)

# Crear un dataframe vacío para almacenar los resultados finales
resultados <- data.frame(fecha_colecta = character(),
                         alícuota = character(),
                         numero_caja = character(),
                         id_muestra = character(),
                         stringsAsFactors = FALSE)

# Variables temporales para almacenar la información mientras se itera
fecha_temp <- NA
aliquota_temp <- NA
numero_caja_temp <- NA

# Iterar sobre cada fila de los datos
for (i in 1:nrow(datos_brutos)) {
  fila <- datos_brutos[i, , drop = FALSE]  # Extraer la fila actual
  texto_fila <- as.character(fila[1,2])  # Convertir a texto para fácil manipulación
  
  # Capturar datos según su contenido
  if (str_detect(texto_fila, "Fecha colecta")) {
    fecha_temp <- as.Date(as.numeric(fila[1,4]), origin = "1899-12-30")  # Convertir el número Excel a fecha
  } else if (str_detect(texto_fila, "Alícuota")) {
    aliquota_temp <- as.character(fila[1,4])
  } else if (str_detect(texto_fila, "Numero de caja")) {
    numero_caja_temp <- as.character(fila[1,4])
  } else {
    # Extraer ids de muestra si la fila contiene identificadores válidos
    ids_muestra <- as.character(unlist(fila))
    ids_muestra <- ids_muestra[grep("^[0-9]{4}-[A-Z][0-9]-[A-Z][0-9]$", ids_muestra)]
    
    if (length(ids_muestra) > 0 && !is.na(fecha_temp) && !is.na(aliquota_temp) && !is.na(numero_caja_temp)) {
      # Agregar los ids a los resultados finales junto con los datos temporales capturados
      resultados <- rbind(resultados, data.frame(fecha_colecta = rep(fecha_temp, length(ids_muestra)),
                                                 alícuota = rep(aliquota_temp, length(ids_muestra)),
                                                 numero_caja = rep(numero_caja_temp, length(ids_muestra)),
                                                 id_muestra = ids_muestra,
                                                 stringsAsFactors = FALSE))
    }
  }
}

# Verificar los resultados
print(resultados)
head(resultados)

resultados %>% group_by(numero_caja) %>% count() %>% View()
#resultados %>% filter(numero_caja=="E0025")

resultados %>% filter(numero_caja!="E0033" | numero_caja!="E0038" | numero_caja!="E0043") %>%  transmute(
  "Sample ID"=id_muestra, "MATRIX"="Urine", "VOL (mL)"=alícuota, "Package #"=numero_caja
) %>% writexl::write_xlsx("output/manifiesto_muestras_orina_eco_enviadas_mayo_2024.xlsx")

resultados %>% filter(numero_caja=="E0033" | numero_caja=="E0038" | numero_caja=="E0043") %>%  transmute(
  "Sample ID"=id_muestra, "MATRIX"="Urine", "VOL (mL)"=alícuota, "Package #"=numero_caja
) %>% writexl::write_xlsx("output/muetras_orina_enuvg_pendientes_enviar.xlsx")

resultados %>% group_by(alícuota) %>% count()

resultados %>% group_by(numero_caja) %>% count() %>% View()
