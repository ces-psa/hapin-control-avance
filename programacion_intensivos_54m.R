library(tidyverse)
library(lubridate)

#leer cat intensivos
cat_intensivos<-read_csv("data/dictionaries/cat_intensivo_cohorte.csv", show_col_types = FALSE)
cat_intensivos<-cat_intensivos %>% mutate(id=as.character(id))

#extraer datos de osm
source("scripts/0_get_osm_uvg.R")

# Crear el data frame de ejemplo
set.seed(07122023)  # Para reproducibilidad


# Contar la cantidad de registros
num_registros <- nrow(cat_intensivos)

# Tomar una muestra aleatoria del 50% de los registros
muestra_50_porciento <- sample(1:num_registros, size = round(0.5 * num_registros))

# Agregar la nueva columna "duplicado" con todos los valores inicializados a "No"
cat_intensivos <- cat_intensivos %>%
  mutate(duplicado = "No")

# Asignar "Si" a la muestra del 50%
cat_intensivos$duplicado[muestra_50_porciento] <- "Si"


# Tomar una muestra aleatoria del 50% de los registros
muestra_10_porciento_ecm <- sample(1:num_registros, size = 11, replace = FALSE)
muestra_10_porciento_upas <- sample(1:num_registros, size = 10, replace = FALSE)

cat_intensivos <- cat_intensivos %>%
  mutate(blanco = "")

#asignar blancos ecm
cat_intensivos$blanco[muestra_10_porciento_ecm] <- "ECM"

#asignar blancos upas
cat_intensivos$blanco[muestra_10_porciento_upas] <- "UPAS"


progra_intensivos<-cat_intensivos %>% mutate(
  #para febrero hacer el cambio
  #fecha_visita_54m=as.Date(fecha_nacimiento)+days(1643),
  fecha_visita_54m=as.Date(fecha_nacimiento)+days(1620),
  #fecha_visita_dias=as.Date(fecha_nacimiento)+days(1643),
  #edad_en_meses=interval(as.Date(fecha_nacimiento), Sys.Date()) %/% months(1),
  `Año`=format(fecha_visita_54m,"%Y"),
  Mes=format(fecha_visita_54m, "%b")
) %>% 
  filter(
  `Año`==2023 | (`Año`=="2024" & Mes=="abr.")
) %>% select(id_estudio=id, fecha_nacimiento, fecha_visita_intensivo=fecha_visita_54m, duplicado, blanco) %>% left_join(
  datos_participantes %>% select(
    `ID tamizaje`, id_estudio=`ID estudio`, Comunidad_1=`Comunidad embarazada (original z10)`, Comunidad_2=`Comunidad embarazada (nueva)`,
    Nombre_madre=`Nombre embarazada`, Nombre_bebe=Nombre_bb, sector_old, direccion_old, referencia_old,
    sector_new, direccion_new, referencia_new, telefono_new, `Celular embarazada`, `Celular esposo`, Celular_otro_miembro
    )
) %>% arrange(as.Date(fecha_visita_intensivo)) 
#agregar rutas segun comunidad 1
progra_intensivos<-progra_intensivos %>% left_join(
  rutas %>% select(Comunidad_1=comunidad, ruta)
)

progra_intensivos<-progra_intensivos %>% left_join(
  gt_participants %>% select(`ID tamizaje`=record_id, id_estudio)
)

progra_intensivos<-progra_intensivos %>% transmute(
  categoria="intenviso",
  visita="54m", `ID tamizaje`, id=id_estudio, FOT="",Blanco=blanco, Duplicado=duplicado, comunidad_1=Comunidad_1, comunidad_2=Comunidad_2,
  fecha_nacimiento, fin_ventana="", dias_pasados_fecha_visita="", `Nombre embarazada`=Nombre_madre,
  Nombre_bebe, ruta, sector_old, direccion_old, referencia_old, sector_new, direccion_new,
  referencia_new, telefono_new, `Celular embarazada`, `Celular esposo`, Celular_otro_miembro,
  tipo_estufa_control="NA"
  
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% 
      select(id,contacto1_h56=h56_phone1, contacto2_h56=h56_phone2,contacto3_h56=h56_phone3) 
    ) %>% group_by(id) %>% slice(1)


progra_intensivos_update<-progra_intensivos %>% anti_join(
gt_osm_uvg_data %>% select(id, hapi_date)
)
#%>% writexl::write_xlsx(paste0("output/visitas/exposicion/programacion_intensivo_",Sys.Date(),".xlsx"))

