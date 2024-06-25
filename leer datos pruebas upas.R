library(tidyverse)
#datos piloto 1 Encabezados ----
directorio_upa<-list.dirs("C:/Users/aramirez/Downloads/20230126/") 
#saber la cantidad de subdirectorios
#maximo=length(directorio_upa)

#Ciclo para leer encabezados piloto 1
for (i in 2:length(directorio_upa)){
  encabezados1<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, n_max = 113, show_col_types = FALSE)) %>% 
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
  datos_estudio<-map(fs::dir_ls(directorio_upa[i]), ~ read_csv(.x, skip = 113, show_col_types = FALSE)) %>% 
    bind_rows(.id = directorio_upa[i]) %>% rename(.,ORIGEN=directorio_upa[i]) 
  
  if(i == 2){
    datos_ciclo_estudio <- datos_estudio
  } else{
    datos_ciclo_estudio <- datos_ciclo_estudio %>% rbind(datos_estudio)
  }
  
}

upas_encabezados_Update<-encabezados_ciclo_estudio
upas_data_update<-datos_ciclo_estudio

upas_data_update %>% group_by(ORIGEN) %>% count()

upas_encabezados_Update %>% group_by(ORIGEN) %>% count()

upas_encabezados_Update %>% select(ORIGEN, PARAMETER, VALUE) %>% group_by(ORIGEN, PARAMETER) %>% 
  spread(key = "PARAMETER", value = "VALUE") %>% View()
