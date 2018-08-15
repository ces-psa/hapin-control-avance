## Instrucciones para generar dashboard

1. Verificar que un diccionario actualizado en formato csv se encuentra en la
carpeta  
`data/dictionaries`  
del proyecto.

2. Descargar los datos crudos de RedCap Emory en formato CSV,
y guardar con el nombre predefinido en la carpeta  
`data/exports`  
del proyecto.

> Es muy importante que no se cambie el nombre del export, porque de allí se toman datos para general el dashboard.

3. Generar el dashboard usando el comando `Ctrl + Shift + K` desde el archivo
`dashboard-avance.Rmd` (o dando click en el botón "Knit", con ícono que parece
una bola de lana con agujas de tejer).


### Conección a base de datos RedCap Guatemala

Este dashboard requiere una conección directa a la base de datos de RedCap
Guatemala.
Para configurar la conección de ODBC, utilice el ip, usuario y clave de siempre
con las siguientes consideraciones:

- El origen de datos debe llamarse `hapin-gt`
- El driver de ODBC debe ser para MySql 5.x.
En Windows puede usar esta versión:  
http://mysql.localhost.net.ar/Downloads/Connector-ODBC/5.1/mysql-connector-odbc-5.1.5-winx64.msi  
(hash md5 `a53bf11e4a42e57afbf0b104ee25e0fe`)


### Otros requerimientos

Para generar este dashboard se necesita:

- R versión 3.1.x o mayor
- Rstudio actualizado
- Estos paquetes de R:
    + `tidyverse`
    + `flexdashboard`
    + `leaflet`
    + `DT`

Para mejores resultados, actualizar todos los paquetes instalados y después
usar este código para instalar los paquetes requeridos:

```r
install.packages(
    pkgs = c("tidyverse", "flexdashboard", "leaflet", "DT"),
    dependencies = c("Depends", "Imports")
)
```
