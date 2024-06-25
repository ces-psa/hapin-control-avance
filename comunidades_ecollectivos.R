#comunidades y participantes hapin
comunidade_coordenadas<-gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id) %>% left_join(
  comunidades %>% select(id_tamizaje, id=house_id, community, lat, long)
) %>% filter(!is.na(lat)) %>% group_by(community) %>% summarize(lat=median(lat), long=median(long)) %>% ungroup() %>% print()

comunidade_conteos<-gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id) %>% left_join(
  comunidades %>% select(id_tamizaje, id=house_id, community, lat, long)
) %>%  group_by(community) %>% count() %>% left_join(
  rutas %>% select(community=comunidad, ruta)
) %>% print()

comunidade_conteos %>% left_join(
  comunidade_coordenadas
) %>% write_csv("output/comunidades_hapin.csv")


#revision datos solicitados por Lisa
comunidade_conteos %>% left_join(
  comunidade_coordenadas
) %>% mutate(
  flag=if_else(ruta=="12",1,0)
) %>% filter(!is.na(flag) & flag=="0")

#leed datos Lisa
community_candidate<-readxl::read_xlsx("C:/HAPIN/Ecollectivos/comunidades_jalapa.xlsx")
pop_jalapa<-readxl::read_xls("C:/HAPIN/Ecollectivos/Jalapa_pop_2018.xls", sheet = "data")

community_candidate %>% left_join(
  pop_jalapa
) %>% writexl::write_xlsx("output/comunidades_pop_2018_join.xlsx")
