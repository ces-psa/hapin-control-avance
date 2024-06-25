# Carga el paquete rgdal
library(rgdal)
library(dplyr)

# Ruta del directorio que contiene los archivos GPX
directorio <- "C:/Users/aramirez/OneDrive - Emory University/Exposure Data--GUATEMALA/Main Trial/GPS data/"

# Obtiene la lista de archivos GPX en el directorio
archivos_gpx <- list.files(path = directorio, pattern = "\\.gpx$", full.names = TRUE)

# Inicializa un dataframe vacío para almacenar los datos combinados
df_combinado <- data.frame()

# Itera a través de cada archivo GPX y combina los datos
for (archivo_gpx in archivos_gpx) {
  # Lee el archivo GPX
  datos_gpx <- readOGR(dsn = archivo_gpx, layer = "waypoints")
  
  # Convierte los datos en un dataframe
  df_gpx <- as.data.frame(datos_gpx)
  
  # Combina los datos al dataframe principal
  df_combinado <- bind_rows(df_combinado, df_gpx)
}

# Imprime el dataframe combinado
print(df_combinado)

df_combinado_final<-df_combinado %>% select(
  fecha=time, name, lat=coords.x1, long=coords.x2, alt=ele, notas=cmt
)
df_combinado_final %>% saveRDS("C:/Documentos/Github/hapin-control-avance/output/rds_coordenadas_hapin.rds")

df_combinado_final %>% writexl::write_xlsx("C:/Documentos/Github/hapin-control-avance/output/revision_coordenadas_gpx.xlsx")
