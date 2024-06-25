#buscar el mas reciente
file_hapinplus<-list.files(
  path = "data/exports", pattern = "HAPINPLuS_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

#cargar los datos
dt_hapin_plus<-read_csv(file_hapinplus$file, cols(.default = "c"),show_col_types = FALSE)
#renombrer columnas
names(dt_hapin_plus) <- dt_hapin_plus[1,]
dt_hapin_plus<-dt_hapin_plus[-1,]

dt_hapin_plus<-dt_hapin_plus %>% filter(record_id!="99999") %>% mutate(id=as.character(record_id),
                visit=recode(redcap_event_name,
                "5_arm_1"="5",
                "5.5_arm1"="5.5")) %>% select(
  id, visit, everything(), -record_id,
)


