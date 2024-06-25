#listado de vales de canje enviados

vales_saldo<-read_csv("data/vales_saldo.csv")
vales_saldo<-vales_saldo %>% mutate(id= as.character(id))

#h56 redcap uvg
salidas_uvg <- read_csv(
  file = "data/exports/FinalizacinHAPIN_DATA_2020-03-02_0927.csv",
  col_types = cols(.default = col_character())
) %>%
  print()
#h56 redcap emory
salidas_emory<-gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(id, h56_date)

#h56_emory
h56_emory <- salidas_emory %>% anti_join(
  salidas_uvg %>% select(id=record_id, h56g_date)
)

#h56uvg
h56_uvg <-salidas_uvg %>% select(id=record_id, h56g_date) %>% anti_join(
  salidas_emory %>% select(id)
)

#total de h56 registrados
h56_total<-h56_emory %>% bind_rows(
  h56_uvg %>% transmute(id, h56_date=as.Date(h56g_date))
)



# Datos de vales
source(file = "scripts/0_get_control_vales_data.R", encoding = "UTF-8")
#canjes de vales
all_canjes <- canje_vales1 %>% 
  select(
    id = record, fecha_canje, matches("id_vale[0-9]$")
  ) %>% bind_rows( 
    list(
      canje_vales2 %>% 
        select(
          id = record, fecha_canje, matches("id_vale[0-9]$")
        )
      ,
      canje_vales3 %>% 
        select(
          id = record, fecha_canje, matches("id_vale[0-9]$")
        )
    )
  ) %>% 
  gather(key, vale, -id, -fecha_canje, na.rm = TRUE) %>%
  print()

#suma de vales canjeados por hogar
canjes <- all_canjes %>%
  group_by(id) %>%
  summarize(
    vales_canjeados = vale %>% unique() %>% length()
  ) %>%
  print()



#Listado de vales de saldo que deben canjearse con efectivo
h56_canjes<-h56_total %>% left_join(
  canjes %>% select(id, vales_canjeados)
  )

canjes_cuatro<-canjes %>% filter(vales_canjeados=="4")

vales_saldo %>% left_join(
  canjes_cuatro
) %>% filter(!is.na(vales_canjeados))  %>%  left_join(
  h56_total
) %>% left_join(
  datos_participantes %>% select(id_tamizaje=`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`, `Comunidad embarazada (nueva)`,
                                 `Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`, `Nombre otra adulta`)
) %>% writexl::write_xlsx("output/vales_saldo_con_efectivo_07-04-2021.xlsx")

