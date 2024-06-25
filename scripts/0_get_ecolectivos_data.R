#get data ecolectivos
#libraries ----
library("tidyverse")

#read  the most recent export file censo and baseline ----
# gt_eco_file_1 <- list.files(
#   path = "data/exports", pattern = "Ecolectivos_DATA_.+csv", full.names = TRUE
# ) %>%
#   tibble(
#     file = .,
#     export_time = file %>%
#       gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
#       lubridate::ymd_hm(tz = "America/New_York")
#   ) %>%
#   slice(which.max(export_time))
# 
# ## read_data censo and baseline ----
# eco_censo_lb<-gt_eco_file_1 %>% 
#   pull(file) %>%
#   read_csv(col_types = cols(.default = col_character()))
# 
# eco_censo_lb<-eco_censo_lb %>% 
#   mutate(
#   visit=case_when(
#     redcap_event_name=="rapid_assessment_arm_1" ~ "censo",
#     redcap_event_name=="baseline_arm_2" ~ "baseline",
#     TRUE  ~ NA_character_
#   )
# ) 

# ### dta censo ----
# eco_censo<- eco_censo_lb %>% filter(visit=="censo")
# ### dta baseline ----
# eco_baseline<- eco_censo_lb %>%  filter(visit=="baseline")
  
# read the most recent export file  -----
gt_eco_file_2 <- list.files(
  path="data/exports", pattern = "EcolectivosEstudio_DATA_.+csv", full.names = TRUE
) %>% 
tibble(
  file= .,
    export_time = file %>% 
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>% 
      lubridate::ymd_hm(tz = "America/New_York")
) %>% slice(
  which.max(export_time)
)

# read data ecolectivos estudio
eco_estudio_data <- gt_eco_file_2 %>% 
  pull(file) %>% 
  read_csv(col_types = cols(.default = col_character()))
## data elegibilidad, estudio, 4 y 12 month ----
eco_estudio_data<-eco_estudio_data %>% mutate(
  visit=case_when(
    redcap_event_name=="elegibilidad_arm_1" ~ "elegibilidad",
    redcap_event_name=="biomonitoreo_lb_arm_1" ~ "biomonitoreo_lb",
    redcap_event_name=="biomonitoreo_4m_arm_1" ~ "biomonitoreo_4m",
    redcap_event_name=="biiomonitoreo_12m_arm_1" ~ "biomonitoreo_12m",
    TRUE ~ NA_character_
  )
)

# read the most recent export file  -----
gt_eco_file_3 <- list.files(
  path="data/exports", pattern = "EcolectivosExposure_DATA_.+csv", full.names = TRUE
) %>% 
  tibble(
    file= .,
    export_time = file %>% 
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>% 
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>% slice(
    which.max(export_time)
  )

## read data ecolectivos exposure -----
eco_exposure_data <- gt_eco_file_3 %>% 
  pull(file) %>% 
  read_csv(col_types = cols(.default = col_character()))

eco_exposure_data<-eco_exposure_data %>% mutate(
  visit="medicion_flujos"
)
