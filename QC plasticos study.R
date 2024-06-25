#export de plastic_study
data_plastic<-read_csv("D:/Descargas/PlasticsSubstudy_DATA_2020-03-13_1150.csv")

#Datos de crf de tamizaje
tamizaje<-data_plastic %>% filter(!is.na(t_date)) %>% select(hhid=record_id,t_date,id_plasticos=t_id_plasticos)
#crf de adolescentes aire
adolescentes_aire<-data_plastic %>% filter(!is.na(aa_date)) %>% select(hhid=record_id, id_plasticos=aa_id_plasticos,aa_iniciales,  )
#crf de orina
orina<-data_plastic %>% filter(!is.na(el_dia)) %>% select(hhid=record_id,id_plasticos=plasticos_identificacion,quien_completo_formula,el_dia,escanee_o_escriba_la_etiqu) 

tamizaje %>% anti_join(
  orina %>% select(id_plasticos)
)

tamizaje %>% anti_join(
  adolescentes_aire %>% select(id_plasticos)
)

orina %>% anti_join(
  tamizaje %>% select(id_plasticos)
)

