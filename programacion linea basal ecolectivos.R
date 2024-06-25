#librerias ----
library("tidyverse")

#cargar los datos del estudio principal de ecolectivos (visitas elegibilidad, tamizaje, 4 y 12 meses) ----
source("script/0_get_data_eco_estudio.R")
#Cargar los datos de exposición ----
source("script/0_get_data_eco_exposure.R")
#cargar los datos de laboratorio ----
source("script/0_get_data_eco_lab.R")
#leer los datos de los archivos de los upas piloto 1 y piloto 2 ----
source("script/0_get_data_piloto.R")

# cat de sectores
sectores<-read_csv("data/cat/cat_sectores.csv")
sectores <- sectores %>% mutate_all(as.character)
# Generar puntos para cada sector ----
# Grupo 1 : Los Lopez

dt_los_lopez<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
  id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
)  %>% filter(id_sector=="26") %>%filter(f1_sleep_15_44>0) %>% filter(
  f1_ecolectivo_participate=='1'
) %>%  left_join(
  sectores
) %>% transmute(id_censo,id_sector, nombre_sector, grupo="1" ) %>% left_join(
  
  gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                               
                                                               z1_name_contac1, z1_contact_cel_1
                                                               )
  
) %>% left_join(
  gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                               z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
) %>% mutate(
  fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
) %>% mutate(
  lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
  long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
)

puntos<-dt_los_lopez %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
  longitude=case_when(
    substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
    substr(long,1,1)>0 ~ paste0("-", long)
  )
) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/liea_basal/los_lopez.txt")
  
  dt_los_lopez %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/programacion_los_lopez.xlsx")

# Grupo 1: Miramundo
  dt_miramundo<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="28") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="1" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_miramundo %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/miramundo.txt")
  
  dt_los_lopez %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/miramundo_programacion.xlsx")


# Grupo 2: Duraznal
  dt_durznal<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="4") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="2" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_duraznal %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/duraznal.txt")
  
  dt_durznal %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/duraznal_programacion.xlsx")

#Grupo 2: El paraiso
  dt_el_paraiso<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="10") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="2" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_el_paraiso %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/el_paraiso.txt")
  
  dt_el_paraiso %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/el_paraiso_programacion.xlsx")

#Grupo 3: Laguna el Pito
  dt_laguna_el_pito<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="16") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="3" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_laguna_el_pito %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/laguna_el_pito.txt")
  
  dt_laguna_el_pito %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/laguna_el_pito_programacion.xlsx")

#Grupo 3: La paz

  dt_la_paz<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="19") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="3" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_la_paz %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/la_paz.txt")
  
  dt_la_paz %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/la_paz_programacion.xlsx")
  
#Grupo 4: El durazno
  dt_el_durazno<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="8") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="4" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_el_durazno %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/el_durazno.txt")
  
  dt_el_durazno %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/el_durazno_programacion.xlsx")

#Grupo 4: La toma

  dt_la_toma<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="20") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="4" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_la_toma %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/la_toma.txt")
  
  dt_la_toma %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/la_toma_programacion.xlsx")

#Grupo 5: Laguna del Sapo
  dt_laguna_del_sapo<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="21") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="5" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_laguna_del_sapo %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/laguna_del_sapo.txt")
  
  dt_laguna_del_sapo %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/laguna_del_sapo_programacion.xlsx")

#Grupo 5: Sansirisay

  dt_sansirisay<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="31") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="5" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_sansirisay %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/sansirisay.txt")
  
  dt_sansirisay %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/sansirisay_programacion.xlsx")

#Grupo 6: Los Izotes

  dt_los_izotes<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="24") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="6" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_los_izotes %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/los_izotes.txt")
  
  dt_los_izotes %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/los_izotes__programacion.xlsx")
  
#Grupo 6: El Bosque

  dt_el_bosque<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="5") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="6" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_el_bosque %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/el_bosque.txt")
  
  dt_el_bosque %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/el_bosque_programacion.xlsx")

#Grupo 7: Patagalano
  
  dt_patagalana<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="30") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="7" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_patagalana %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/patagalana.txt")
  
  dt_patagalana %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/patagalana_programacion.xlsx")

#Grupo 7: Itzacoba
  
  dt_itzacoba<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="14") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="7" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_itzacoba %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/itzacoba.txt")
  
  dt_itzacoba %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/itzacoba_programacion.xlsx")

#Grupo 8: Yerbabuena
  dt_yerbabuena<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="37") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="8" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_yerbabuena %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/yerbabuena.txt")
  
  dt_yerbabuena %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/yerbabuena_programacion.xlsx")

#Grupo 8: Las Guacamayas
  dt_las_guacamayas<-gt_eco_estudio_p_data %>% filter(visit=="lb") %>% filter(!is.na(f1_date)) %>% select(
    id_censo, f1_ecolectivo_participate, id_sector=f1_sector, z1_date, f1_sleep_15_44
  )  %>% filter(id_sector=="22") %>%filter(f1_sleep_15_44>0) %>% filter(
    f1_ecolectivo_participate=='1'
  ) %>%  left_join(
    sectores
  ) %>% transmute(id_censo,id_sector, nombre_sector, grupo="8" ) %>% left_join(
    
    gt_eco_estudio_p_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_referencie_household,
                                                                 
                                                                 z1_name_contac1, z1_contact_cel_1
    )
    
  ) %>% left_join(
    gt_eco_estudio_p_data %>% filter(!is.na(z0_date)) %>% select(id_censo, z0_fid_sample, 
                                                                 z0_repleaced_coord,z0_lat, z0_long, z0_lat_2, z0_long_2)
  ) %>% mutate(
    fid=if_else(z0_repleaced_coord==0 | is.na(z0_repleaced_coord), z0_fid_sample, NA_character_)
  ) %>% mutate(
    lat=if_else(!is.na(z0_lat_2), z0_lat_2, z0_lat),
    long= if_else(!is.na(z0_long_2), z0_long_2, z0_long)
  )
  
  puntos<-dt_las_guacamayas %>% select(ident=id_censo, latitude=lat, long) %>% mutate(
    longitude=case_when(
      substr(long,1,1)==0 ~ paste0("-",substr(long,2,9)),
      substr(long,1,1)>0 ~ paste0("-", long)
    )
  ) %>% select(ident, latitude, longitude)
  puntos %>% write_csv("output/linea_basal/las_guacamayas.txt")
  
  dt_las_guacamayas %>% select(
    id_censo, nombre_sector, nombre_contacto=z1_name_contac1, telefono=z1_contact_cel_1,
    referencia=z1_referencie_household
  ) %>% writexl::write_xlsx("output/linea_basal/las_guacamayas_programacion.xlsx")

