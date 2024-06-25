library(tidyverse)

pruebas_flujos<-readxl::read_xlsx("C:/Users/aramirez/ownCloud/Exposure_group/experimento_flujos/EXPERIMENTO MEDICION DE FLUJOS/DATOS RECOPILADOS/EXPERIMENTO UPAS 1.xlsx")


pruebas_flujos<-pruebas_flujos %>% janitor::clean_names()


# Cargar la librería ggplot2 si aún no está cargada
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Calcular la diferencia y la media de las dos variables
pruebas_flujos$diferencia <- pruebas_flujos$flujo_gilibrator - pruebas_flujos$flujo_alicat
pruebas_flujos<-pruebas_flujos %>% filter(diferencia<0.07)
pruebas_flujos$media <- (pruebas_flujos$flujo_gilibrator + pruebas_flujos$flujo_alicat) / 2

# Calcular la media y la desviación estándar de las diferencias
media_diferencia <- mean(pruebas_flujos$diferencia)
sd_diferencia <- sd(pruebas_flujos$diferencia)

# Crear la gráfica Bland-Altman
bland_altman_plot <- ggplot(pruebas_flujos, aes(x = media, y = diferencia)) +
  geom_point() +  # Puntos de dispersión
  geom_abline(intercept = media_diferencia, slope = 0, color = "red", linetype = "dashed") +  # Línea de la media
  geom_abline(intercept = media_diferencia + sd_diferencia, slope = 0, color = "blue", linetype = "dashed") +  # Línea de media + 1 desviación estándar
  geom_abline(intercept = media_diferencia - sd_diferencia, slope = 0, color = "blue", linetype = "dashed") +  # Línea de media - 1 desviación estándar
  labs(
    title = "Gráfica Bland-Altman",
    x = "Media de flujo_gilibrator y flujo_alicat",
    y = "Diferencia entre flujo_gilibrator y flujo_alicat"
  )

# Mostrar la gráfica
print(bland_altman_plot)


#REVISIÓN DE FLUJOS POR UPAS
# Cargar las librerías necesarias si aún no están cargadas
library(tidyverse)

# Leer el archivo Excel y limpiar los nombres de las columnas
pruebas_flujos <- readxl::read_xlsx("C:/Users/aramirez/ownCloud/Exposure_group/experimento_flujos/EXPERIMENTO MEDICION DE FLUJOS/DATOS RECOPILADOS/EXPERIMENTO UPAS 1.xlsx") %>%
  janitor::clean_names()

# Calcular la diferencia y la media de las dos variables
pruebas_flujos$diferencia <- pruebas_flujos$flujo_gilibrator - pruebas_flujos$flujo_alicat
pruebas_flujos <- pruebas_flujos %>% filter(diferencia < 0.07)
pruebas_flujos$media <- (pruebas_flujos$flujo_gilibrator + pruebas_flujos$flujo_alicat) / 2

# Calcular la media y la desviación estándar de las diferencias
media_diferencia <- mean(pruebas_flujos$diferencia)
sd_diferencia <- sd(pruebas_flujos$diferencia)

# Crear la gráfica Bland-Altman con puntos coloreados por "id_upas"
bland_altman_plot <- ggplot(pruebas_flujos, aes(x = media, y = diferencia, color = id_upas)) +
  geom_point() +  # Puntos de dispersión
  geom_abline(intercept = media_diferencia, slope = 0, color = "red", linetype = "dashed") +  # Línea de la media
  geom_abline(intercept = media_diferencia + sd_diferencia, slope = 0, color = "blue", linetype = "dashed") +  # Línea de media + 1 desviación estándar
  geom_abline(intercept = media_diferencia - sd_diferencia, slope = 0, color = "blue", linetype = "dashed") +  # Línea de media - 1 desviación estándar
  labs(
    title = "Gráfica Prueba de Flujos",
    x = "Media de flujo_gilibrator y flujo_alicat",
    y = "Diferencia entre flujo_gilibrator y flujo_alicat"
  )

# Mostrar la gráfica
print(bland_altman_plot)


