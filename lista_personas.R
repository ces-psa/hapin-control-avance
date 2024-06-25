library(package = "tidyverse")

s0 %>% select(record_id, s0_name) %>% filter(!is.na(s0_name))

gt_emory_data %>% 
  select(id_tamizaje=id, s2_date, s2_participate) %>% 
    filter(!is.na(s2_date), s2_participate=="1") %>% left_join(
gt_z10 %>%
  mutate_all(
    funs(iconv(., from = "Latin1", to = "UTF-8"))
  ) %>%
  mutate_at(
    vars(
      z10_p_name, z10_p_lastname_2,
      z10_aow_name, z10_aow_lastname,
      z10_hh_name, z10_hh_lastname_2
    ),
    funs(
      if_else(
        condition = is.na(.),
        true = "",
        false = .
      )
    )
  ) %>%
  mutate(
    id_tamizaje=if_else(
                grepl("^G[0-9]{4}",record_id),
                record_id, 
                id_estudio),
    house_id=if_else(
      grepl("^G[0-9]{4}",record_id),
      id_estudio,
      record_id),           
    embarazada = paste(z10_p_name, z10_p_lastname_2),
    adulta = paste(z10_aow_name, z10_aow_lastname),
    esposo = paste(z10_hh_name, z10_hh_lastname_2)
  ) 
 ) %>% mutate(
    evento=if_else(is.na(id_estudio) & !is.na(s2_date),"Invitada","")
  ) %>% select(
    "ID tamizaje" = id_tamizaje,
    "ID estudio" = house_id,
    "Nombre embarazada" = embarazada,
    # "DPI embarazada" = z10_dpi,
    # "Fecha nacimiento embarazada" = z10_dob,
    "Nombre esposo" = esposo,
    "Evento actual" = evento,
    "Fecha de parto" = fecha_parto,
    "Evento" = evento,
    "Nombre otra adulta" = adulta
  ) %>% DT::datatable() %>% print()

  htmlwidgets::saveWidget(file = "lista_personas.html")

file.rename(
  from = "participants.html",
  to = "output/reference/participants.html"
)
