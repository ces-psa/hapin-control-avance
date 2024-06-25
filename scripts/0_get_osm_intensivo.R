#buscar el mas reciente
file_osm<-list.files(
  path = "data/exports", pattern = "Osmproject_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

#cargar los datos
dt_osm_project<-read_csv(file_osm$file, cols(.default = "c"),show_col_types = FALSE)
#renombrer columnas
names(dt_osm_project) <- dt_osm_project[1,]
dt_osm_project<-dt_osm_project[-1,]

dt_osm_project<-dt_osm_project %>% filter(record_id!="99999") %>% mutate(id=as.character(record_id),
                visit=recode(redcap_event_name,
                "54m_arm_2"="54m",
                "consent_arm_2"="consent")) %>% select(
  id, visit, everything(), -record_id,
)


dt_osm_project %>% group_by(redcap_event_name) %>% count()
