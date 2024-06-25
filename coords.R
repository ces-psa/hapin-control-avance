library(package = "tidyverse")
library(package = "crosstalk")
# Emory RedCap dictionary

source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Emory exposure_start export data
source(file = "scripts/0_get_exposure_start.R", encoding = "UTF-8")

house_coords <- gt_emory_data_arm1 %>%
  # datos de casas inscritas
  filter(!is.na(s1_date), !is.na(s4_main_id)) %>%
  select(
    study_id = s4_main_id, screening_id = id, community = s1_community_name
  ) %>%
  # agregar las coordenadas
  left_join(
    # de z10
    gt_participants %>%
      select(screening_id = record_id, long, lat) %>%
      left_join(
        # con id de estudio
        gt_emory_data_arm1 %>%
          filter(!is.na(s1_date), !is.na(s4_main_id)) %>%
          select(
            study_id = s4_main_id, screening_id = id
          )
      ) %>%
      # y las que el equipo de exposición colecta en cada visita
      bind_rows(
        gt_exposure_start %>%
          select(study_id = record_id, long = gth4x_long, lat = gthx_lat) %>%
          mutate_at(
            vars(long, lat),
            funs(as.double)
          )
      ) %>%
      filter(long > -90.5)
  ) %>%
  # solo las disponibles
  filter(complete.cases(.)) %>%
  # quitar valores extremos en las coordenadas para cada casa
  group_by(study_id, screening_id, community) %>%
  filter(
    # solo el 50% más común de las coordenadas por casa
    between(long, quantile(long, 0.25), quantile(long, 0.75)),
    between(lat, quantile(lat, 0.25), quantile(lat, 0.75))
  ) %>%
  # y resumir
  summarize_at(
    vars(long, lat),
    funs(median)
  ) %>%
  ungroup() %>%
  print()


conteo_hogares<-house_coords %>%
  # solo las disponibles
  filter(complete.cases(.)) %>%
  group_by(community) %>% count(community) %>% View()
#conteo de hogares alexander
conteo_hogares %>%  mutate(
    proporcion = n/308
) %>% write_csv("output/proporcion_hogares.csv") 

community_coords <- house_coords %>%
  # solo las disponibles
  filter(complete.cases(.)) %>%
  group_by(community) %>%
  # quitar valores extremos en las coordenadas para cada casa
  filter(
    # solo el 50% más común de las coordenadas por casa
    between(long, quantile(long, 0.25), quantile(long, 0.75)),
    between(lat, quantile(lat, 0.25), quantile(lat, 0.75))
  ) %>%
  # y resumir
  summarize_at(
    vars(long, lat),
    funs(median)
  ) %>% 
  ungroup() %>%
  print()
View(community_coords)

community_coords %>% write_csv("output/comunity_coords.csv")
