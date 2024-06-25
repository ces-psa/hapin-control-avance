#librerias
library(package = "tidyverse")

#DATOS CON VERSION ANTERIOR

#datos piloto 1 Encabezados ----
directorio_upa<-list.dirs("C:/Users/aramirez/ownCloud/Ecolectivos/ECO_Exposure_Data/ESTUDIO PRINCIPAL/UPAS/LB/old") 
#saber la cantidad de subdirectorios
#maximo=length(directorio_upa)

#Ciclo para leer encabezados piloto 1
for (i in 2:length(directorio_upa)){
  encabezados1<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, n_max = 61, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    encabezados_ciclo_estudio <- encabezados1
  } else{
    encabezados_ciclo_estudio <- encabezados_ciclo_estudio %>% rbind(encabezados1)
  }
  
}
#length(directorio_upa)
#ciclo para leer datos de los archivos del piloto 1
for (i in 2:length(directorio_upa)){
  datos_estudio<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, skip = 89, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    datos_ciclo_estudio <- datos_estudio
  } else{
    datos_ciclo_estudio <- datos_ciclo_estudio %>% rbind(datos_estudio)
  }
  
}

upas_encabezados<-encabezados_ciclo_estudio
upas_data<-datos_ciclo_estudio



#datos piloto 1 Encabezados ----
directorio_upa<-list.dirs("C:/Users/aramirez/ownCloud/Ecolectivos/ECO_Exposure_Data/ESTUDIO PRINCIPAL/UPAS/LB/update") 
#saber la cantidad de subdirectorios
#maximo=length(directorio_upa)

#Ciclo para leer encabezados piloto 1
for (i in 2:length(directorio_upa)){
  encabezados1<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, n_max = 61, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    encabezados_ciclo_estudio <- encabezados1
  } else{
    encabezados_ciclo_estudio <- encabezados_ciclo_estudio %>% rbind(encabezados1)
  }
  
}
#length(directorio_upa)
#ciclo para leer datos de los archivos del piloto 1
for (i in 2:length(directorio_upa)){
  datos_estudio<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, skip = 89, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    datos_ciclo_estudio <- datos_estudio
  } else{
    datos_ciclo_estudio <- datos_ciclo_estudio %>% rbind(datos_estudio)
  }
  
}

upas_encabezados_Update<-encabezados_ciclo_estudio
upas_data_update<-datos_ciclo_estudio


upas_encabezados<-upas_encabezados_Update %>% bind_rows(
  upas_encabezados
)

upas_data<-upas_data_update %>% bind_rows(
  upas_data
)


#datos piloto M4 Encabezados ----
directorio_upa<-list.dirs("C:/Users/aramirez/ownCloud/Ecolectivos/ECO_Exposure_Data/ESTUDIO PRINCIPAL/UPAS/4M") 
#saber la cantidad de subdirectorios
#maximo=length(directorio_upa)

#Ciclo para leer encabezados piloto 1
for (i in 2:length(directorio_upa)){
  encabezados1<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, n_max = 61, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    encabezados_ciclo_4m <- encabezados1
  } else{
    encabezados_ciclo_4m<- encabezados_ciclo_4m %>% rbind(encabezados1)
  }
  
}
#length(directorio_upa)
#ciclo para leer datos de los archivos del piloto 1
for (i in 2:length(directorio_upa)){
  datos_estudio<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, skip = 89, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    datos_ciclo_4m <- datos_estudio
  } else{
    datos_ciclo_4m <- datos_ciclo_4m %>% rbind(datos_estudio)
  }
  
}

upas_encabezados_4m<-encabezados_ciclo_4m
upas_data_4m<-datos_ciclo_4m


upas_encabezados<-upas_encabezados_4m %>% bind_rows(
  upas_encabezados
)

upas_data<-upas_data_4m %>% bind_rows(
  upas_data
)
