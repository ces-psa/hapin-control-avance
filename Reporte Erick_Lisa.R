# load use packages
library(package = "tidyverse")
library(package = "crosstalk")

###CARGA DE DATOS EMORY Y UVG
# Pre-screening information (s0)
source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Load helper functions
source(file = "scripts/zz_output.R")

# Emory RedCap dictionary

source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Stops knitting if lpg delivery data is not up to date
source(file = "scripts/uso-estufas.R", encoding = "UTF-8")

#fecha de la semana
fecha1<-'2019-01-27'
fecha2<-'2019-02-02'
#1 separar hogares intervención y control
s6_h50<-gt_emory_data %>% select (id_tamizaje=id, s1_date, s1_community_code, s1_community_name) %>% filter(!is.na(s1_date)) %>% 
  left_join(
    gt_emory_data %>% select(id_tamizaje=id, id= s4_main_id, s4_date) %>% filter(!is.na(s4_date))
  ) %>% filter(!is.na(id)) %>% left_join(
    gt_emory_data %>% select(id, s6_date, s6_arm) %>% filter(!is.na(s6_date))
  ) %>% left_join(
    gt_emory_data %>% select(id, h50_date, h50_traditional) %>% filter(!is.na(h50_date))
  ) %>% 
  print()
  
#   
# 
#   select(id_tamizaje=id,s6_date,s6_arm) %>% filter(!is.na(s6_date)) %>% 
#   mutate(
#     tipo_hogar= case_when(
#         s6_arm=="1" ~ "Intervencion",
#         TRUE ~ "Control"
#     )
#   ) %>% left_join(gt_emory_data %>% select(id,h50_date,h50_traditional) %>% filter(!is.na(h50_date)) %>% 
#   mutate(
#     ubicacion_estufa_trad = case_when(
#       h50_traditional=="1" ~ "En la misma cocina",
#       h50_traditional=="2" ~ "En cuarto diferente",
#       h50_traditional=="3" ~ "Afuera",
#       h50_traditional=="4" ~ "Almacenada",
#       h50_traditional=="5" ~ "Destruida",
#     )
#   )) %>% print()
# s6_h50 %>% count(tipo_hogar,ubicacion_estufa_trad)
View(dots_setup_rc)
##tabla de H40 todas las visitas
dots_setup_rc <- gt_emory_data %>%
  mutate(irc = "guatemala") %>%
  select(
    irc, HHID = id, redcap_event_name,
    matches("h40_.*(visit|time|date|dot|area|stove|add_stove|data)"),
    # remove because we are not using it but was included because it matched dot
    -matches("complete|problem|action|other")
  ) %>%
  mutate_all(as.character) %>%
  #----------------------------------------------------------------------------*
  # Reshape and subset the data to the variables that are needed
  #----------------------------------------------------------------------------*
  gather(
    key = column, value = value,
    matches("visit|time|date|dot|area|stove|add_stove|data"),
    -matches("h40_date"),
    na.rm = TRUE
  ) %>%
  mutate(
    # explicit crf_copy on variable names
    column = if_else(
      condition = !grepl("_v[0-9]$", column),
      true = paste0(column, "_v1"),
      false = column
    ),
    # Fix non-standard variablenames
    column = gsub(
      pattern = "countinue",
      replacement = "continue",
      column
    )
  ) %>%
  # separate variable context
  extract(
    col = column,
    into = c("crf", "variable", "dot_correlative", "crf_copy"),
    regex = "(^[^_]+)_([^0-9]+)([0-9]+)?_v([0-9]+$)"
  ) %>%
  spread(key = variable, value = value) %>%
  # keep the correct date given the crf copy
  mutate(
    crf_date = if_else(
      condition = crf_copy == 1,
      true = h40_date,
      false = h40_date_v2
    )
  ) %>%
  arrange(HHID, crf_date) %>%
  # collect the data depending on the visit type
  select(
    irc, HHID, redcap_event_name, crf_date,
    crf_copy, dot_id = dot, visit,
    everything(), -crf, -h40_date, -h40_date_v2
  ) %>%
  gather(key = variable, value = value, matches("(ins|dl|stop)_(date|time)")) %>%
  separate(
    col = variable,
    into = c("step", "variable")
  ) %>%
  filter(!is.na(value)) %>%
  spread(key = variable, value = value) %>%
  rename(step_date = date) %>%
  arrange(HHID, dot_id, crf_date) %>% print()

dots_setup_rc %>% count(add_stove) %>% print()
View(dots_setup_rc)

gt_emory_data %>% select(id, h52_date, h52_by) %>% filter(!is.na(h52_date)) %>% print()

h52<-gt_emory_data %>% select(id,h52_date, h52_delay_reason) %>% filter(!is.na(h52_date)) %>% count(h52_delay_reason)%>% print() %>% 
