library(tidyverse)
library(openxlsx)

df <- data.frame(
  Nombre = c("Juan", "María", "Pedro", "Laura"),
  Edad = c(25, 32, 27, 20),
  Salario = c(2000, 3000, 2500, 1800)
)


# Crear libro y hoja de Excel
wb <- createWorkbook()
hoja <- addWorksheet(wb, "Hoja1")

# Escribir el dataframe en la hoja de Excel
writeData(hoja, df)

# Crear formatos condicionales
formato_edad_rojo <- createStyle(
  fgFill = "#FF0000",
  bgFill = "#FFFFFF",
  fontWeight = "bold"
)
formato_edad_verde <- createStyle(
  fgFill = "#00FF00",
  bgFill = "#FFFFFF",
  fontWeight = "bold"
)
formato_salario_amarillo <- createStyle(
  fgFill = "#FFFF00",
  bgFill = "#FFFFFF",
  fontWeight = "bold"
)

# Aplicar formatos condicionales a las columnas correspondientes
condicion_edad_rojo <- CellStyle(df$Edad > 30, formato_edad_rojo)
addStyle(hoja, style = condicion_edad_rojo, rows = 2:(nrow(df)), cols = which(names(df) == "Edad"))

condicion_edad_verde <- CellStyle(df$Edad <= 30, formato_edad_verde)
addStyle(hoja, style = condicion_edad_verde, rows = 2:(nrow(df)), cols = which(names(df) == "Edad"))

condicion_salario_amarillo <- CellStyle(df$Salario > 2500, formato_salario_amarillo)
addStyle(hoja, style = condicion_salario_amarillo, rows = 2:(nrow(df)), cols = which(names(df) == "Salario"))

# Guardar archivo de Excel
saveWorkbook(wb, "archivo_excel.xlsx", overwrite = TRUE)
