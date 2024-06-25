comunidades

library("tidyverse")

lista_productos<-read_csv("d:/Descargas/lista_productos.csv")


lista_productos %>% mutate(id_hogar=as.character(ID)) %>%  
  select(id_estudio=id_hogar, articulo, Cantidad) %>%  left_join(
  comunidades
) %>% write_csv("output/entrega_productos.csv")

as.character()


gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_participant, e1_title, e1_outcome, e1_category___1 ) %>%
filter(e1_title=="2")

salidas %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id,e3_reason )
) %>% mutate(
  e3_reason=case_when(
    e3_reason=="2" ~ "No elegible",
    e3_reason=="3" ~ "Retiro voluntario de laparticipante",
    e3_reason=="5" ~ "Se mudo del area de estudio",
    e3_reason=="8" ~ "Otro",
    TRUE ~ NA_character_
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id,e3_reason_c )
) %>% mutate(
  e3_reason_c=case_when(
    e3_reason_c=="2" ~ "No elegible",
    e3_reason_c=="3" ~ "Retiro voluntario de laparticipante",
    e3_reason_c=="5" ~ "Se mudo del area de estudio",
    e3_reason_c=="8" ~ "Otro",
    TRUE ~ NA_character_
  )
) %>% write_csv("output/e3.csv")
  group_by(e3_reason) %>% summarize(n=n())
e3_reason_o

gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select("id_estudio"=id, "fecha_instalacion"=h50_date, "iniciales"=h50_by) %>% left_join(
  comunidades %>% select(id_estudio, "comunidad"=codigo)
) %>% write_csv("output/lista_instalaciones_estufas.csv")

gt_emory_repeat_data %>% filter(!is.na(record_id)) %>% select(record_id,h51_hhid, h51_date, h51_by) %>% mutate(
  id_manual=substring(record_id,1,5),
  inconsistencia=if_else(
    as.character(id_manual)!=as.character(h51_hhid), "1",NA_character_
  )
) %>% filter(!is.na(inconsistencia)) %>% writexl::write_xlsx("output/inconsistencias_id_h51.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date) %>% arrange(desc(h50_date))


entregar<-read_csv("d:/Descargas/productos_octubre.csv")
comunidades

entregar %>% transmute(house_id=as.character(`ID Casa`), articulo , Cantidad ) %>% left_join(
  comunidades %>% transmute(house_id, comunidad=codigo)
) %>% write_csv(paste0("output/lista_entrega_productos_al_",Sys.Date(),".csv"))

gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, fecha_randomizacion=s6_date, s6_arm) %>% 
  mutate(brazo=recode(
  s6_arm, "1"="Intervencion", "0"="Control"
      )
      ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% transmute(id, fecha_instalacion_estufa=h50_date)
      ) %>% select(-s6_arm) %>% left_join(
        salidas %>% transmute(id,tiene_salida="Si")
      ) %>% mutate(
        mes=lubridate::month(fecha_instalacion_estufa)
      )%>% writexl::write_xlsx("output/lista_randomizacion_estufas_al_2019-10-30.xlsx")


#detectar sobre escritos en segun sea necesario
gt_emory_data_arm2 %>% filter(!is.na(c36a_date)) %>% select(id, c36a_date, visit) %>% 
  mutate(visit=recode(visit,"segun_sea_necesari_arm_2d"="libres4", "segun_sea_necesari_arm_2e"="libres5")) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c40_date)) %>% select(id, c40_date, visit) %>% 
      mutate(visit=recode(visit,"segun_sea_necesari_arm_2d"="libres4", "segun_sea_necesari_arm_2e"="libres5"))
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date, visit) %>% 
      mutate(visit=recode(visit,"segun_sea_necesari_arm_2d"="libres4", "segun_sea_necesari_arm_2e"="libres5"))
  ) %>% filter(c40_date!=e2_date)


# DT::renderDataTable(escape = FALSE) %>%
DT::datatable(
  # Keep html in cells
  escape = FALSE,
  # Fill vertically
  height = "100%",
  # declare extensions
  extensions = c(
    # provide downloads
    "Buttons",
    # fix columns with IDs
    "FixedColumns",
    # Fixed header
    # "FixedHeader",
    # Only render visible
    "Scroller"
  ),
  fillContainer = TRUE,
  # configure options
  options = list(
    dom = "Bfrtip",
    scrollX = TRUE, scrollY = TRUE,
    fixedColumns = list(leftColumns = 4),
    # fixedHeader = TRUE,
    extend = "collection",
    buttons = c("csv", "excel"),
    # Scroller
    deferRender = TRUE,
    scroller = TRUE
  )
  
)


gt_emory_repeat_data %>% filter(!is.na(h51_date)) %>%  select(record_id, h51_hhid, h51_date, h51_by) %>% DT::datatable(.,
                                                                                                                       escape = TRUE,
                                                                                                                       # Fill vertically
                                                                                                                       height = "400%",
                                                                                                                       extensions = c(
                                                                                                                         # provide downloads
                                                                                                                         "Buttons",
                                                                                                                         # fix columns with IDs
                                                                                                                         "FixedColumns",
                                                                                                                         # Fixed header
                                                                                                                         # "FixedHeader",
                                                                                                                         # Only render visible
                                                                                                                         "Scroller"
                                                                                                                       ),
                                                                                                                       fillContainer = TRUE,
                                                                                                                       # configure options
                                                                                                                       options = list(
                                                                                                                         dom = "Bfrtip",
                                                                                                                         scrollX = TRUE, scrollY = TRUE,
                                                                                                                         fixedColumns = list(leftColumns = 4),
                                                                                                                         # fixedHeader = TRUE,
                                                                                                                         extend = "collection",
                                                                                                                         buttons = c("csv", "excel"),
                                                                                                                         # Scroller
                                                                                                                         deferRender = TRUE,
                                                                                                                         scroller = TRUE
                                                                                                                       )
                                                                                                                       

  ) %>%  htmlwidgets::saveWidget(file = "h51_enviados.html")


gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, visit, c33_by)


gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id,id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  comunidades %>% select(id=id_estudio, comunidad=community)
) %>% group_by(comunidad) %>% summarize(n=n()) %>% write_csv("output/hogares_comunidades.csv")

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id,id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  comunidades %>% select(id=id_estudio, comunidad=community)
) %>% write_csv("output/listado_tmp.csv")


gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
) %>% arrange(m17_ga)

              
#revision de fechas, codigo de oscar
gt_emory_data_arm2 %>%
  select(id, matches("dob"), -matches("(m10|s[13]|c30|m60)_")) %>%
  gather(crf, entered_dob, -id, na.rm = TRUE) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(c30_date)) %>%
      select(id, reference_dob = c30_dob)
  ) %>%
  mutate_at(
    vars(matches("dob")),
    list(as.Date)
  ) %>%
  mutate(
    crf = sub("_dob", "", crf),
    difference = as.numeric(entered_dob - reference_dob, unit = "days")
  ) %>%
  filter(difference != 0)

#revision de c42 vacuna de poliomielitis
gt_emory_data_arm2 %>% filter(!is.na(c42_date)) %>% select(id, evento=redcap_event_name, c42_date, bacilo=c42_bgc0, vop1=c42_opv1, vpi=c42_ipv1, 
                                                           vop2=c42_opv2, vpi2=c42_ipv2, vop3=c42_opv3) %>% writexl::write_xlsx(
                                                             "output/c42_revision.xlsx"
                                                           )


gt_emory_data_arm2 %>% filter(!is.na(h54_date)) %>% select(id, h54_date, h54_stove_use) %>% group_by(h54_stove_use) %>% summarize(
  n=n()
)

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(
  id=s4_main_id, fecha=s4_date, iniciales=s4_by
) %>% grepl(
  
)

