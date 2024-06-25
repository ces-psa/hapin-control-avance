# FIRST LOAD DATA

library(package = "tidyverse")


# find most recent files
arm1_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm1_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm1_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

main_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm2_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm2_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

# Manually select data export
arm1 <- read_csv(
  file = arm1_file$file,
  col_types = cols(.default = col_character())
) %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$"), m17_ga, c30_dob),
    list(as.Date)
  ) %>%
  filter(id != "99999") %>%
  mutate(
    visit = redcap_event_name %>%
      sub("_arm.*", "", .) %>%
      recode(
        .,
        linea_de_base = "baseline",
        .default = .
      ) %>%
      factor(
        levels = c("baseline", "p1", "p2", "b1", "b2", "b4")
      ),
    # manual fix for overwriten s4
    s4_main_id = if_else(
      condition = id == "G1488",
      true = "33488",
      false = s4_main_id
    )
  ) %>%
  print()

arm2 <- read_csv(
  file = main_file$file,
  col_types = cols(.default = col_character())
) %>%
  filter(id != "99999") %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$"), c30_dob),
    list(as.Date)
  ) %>%
  mutate(
    visit = redcap_event_name %>%
      sub("_arm.*", "", .) %>%
      recode(
        .,
        linea_de_base = "baseline",
        .default = .
      ) %>%
      factor(
        levels = c("baseline", "p1", "p2", "b1", "b2", "b4")
      )
  ) %>%
  print()


# repeated crfs ----
repeats_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaRepeat_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaRepeat_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

repeats <- read_csv(
  file = repeats_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()


# arm3 monthly visits ----

# find most recent files
arm3_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm3_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm3_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

# Manually select data export
arm3 <- read_csv(
  file = arm3_file$file,
  col_types = cols(.default = col_character())
) %>%
  filter(id != "99999") %>%
  print()





#------------------------------------------------------------------------------*
# get reported uploads ----
#------------------------------------------------------------------------------*
#leer hoja de excel de clinica
flor <- readxl::read_excel(
  "data/envios/rc-uploads.xlsx", sheet = "flor", col_names = FALSE
) %>%
  print()

clinic_data <- flor %>%
  set_names(letters[1:length(.)]) %>%
  separate(a, into = c("visit", "id")) %>%
  mutate(group = "clinic") %>%
  gather(c, crf, -group, -id, -visit) %>%
  mutate_all(tolower) %>%
  filter(!grepl("^[^a-z]*$", crf), !is.na(crf)) %>%
  select(-c) %>% 
  print()




# erick <- readxl::read_excel(
#   "data/envios/rc-uploads2.xlsx", sheet = "erick", col_names = TRUE
# ) %>%
#   print()
#erick <-tibble()

#leer hoja de excel de exposure
erick <- readxl::read_excel(
  "data/envios/rc-uploads.xlsx", sheet = "erick", col_names = TRUE
) %>%
  print()

exposure_data <- erick %>%
  mutate(group = "exposure") %>%
  rename(id = ID, visit = Visita) %>%
  gather(crf, value, -group, -id, -visit) %>%
  mutate_all(tolower) %>%
  filter(!grepl("^[^a-z]*$", value), !is.na(value)) %>%
  select(-value) %>% 
  print()



# roberto <- readxl::read_excel(
#   "data/logs/rc-uploads.xlsx", sheet = "roberto"
# ) %>%
#   print()
# roberto <- readxl::read_excel(
#   "data/envios/rc-uploads_intervencion.xlsx", sheet = "exposicion", col_names = TRUE
# ) %>%
#   print()

#leer hoja de excel de exposure
roberto <- readxl::read_excel(
  "data/envios/rc-uploads.xlsx", sheet = "roberto", col_names = TRUE
) %>%
  print()

intervencion<-roberto %>%
  mutate(group = "intervencion") %>%
  rename(id = ID, visit = Visita) %>%
  gather(crf, value, -group, -id, -visit) %>%
  mutate_all(tolower) %>%
  filter(!grepl("^[^a-z]*$", value), !is.na(value)) %>%
  select(-value) %>% 
  print()
data_intervencion<-intervencion %>% mutate(date=Sys.Date())
# inter_data <- roberto %>%
#   rownames_to_column() %>%
#   separate_rows(crf, sep = " ") %>%
#   mutate(crf = tolower(crf), type = if
#   ) %>%
#   print(n = Inf)

e7 <- readxl::read_excel(
  "data/envios/rc-uploads.xlsx", sheet = "e7", col_names = TRUE
) %>%
  print()

all_crfs %>% group_by(visit) %>% count()

# performed crfs
all_crfs <- arm2 %>%
  select(id, visit = redcap_event_name, matches("^[^_]+_date$")) %>%
  mutate(
    visit = visit %>%
      sub("mes_([0-9]+)", "mes\\1", .) %>%
      sub("_.+", "", .) %>%
      sub("mes", "m", .)
  ) %>%
  gather(key, date, -id, -visit, na.rm = TRUE) %>%
  mutate(date = as.character(date)) %>%
  separate(key, into = c("crf", NA)) %>%
  # add repeated crfs
  bind_rows(
    repeats %>%
      filter(!is.na(h54_date) | !is.na(h51_date)) %>%
      select(h51_hhid, h51_date, h54_hhid, h54_date) %>%
      rownames_to_column() %>%
      gather(key, value, -rowname, na.rm = TRUE) %>%
      separate(key, into = c("crf", "var")) %>%
      spread(var, value) %>%
      select(-rowname, id = hhid)
  ) %>%
  print()


clinic_data %>%
  bind_rows(
    list(
      exposure_data
      ) 
    )%>% 
  mutate(
    visit = case_when(
      crf %in% c("h51", "h54") ~ NA_character_,
      TRUE ~ visit
    )
  ) %>% left_join(
    all_crfs 
    ) %>% arrange(!is.na(date), id, crf) %>%
     filter(is.na(date)) %>%
     print(n = Inf) %>% 
  write_csv(paste0("output/envios/revision_al_", Sys.Date(),".csv"))

#revisar H54 específicos:
#all_crfs %>% filter(id=="33037" & is.na(visit) ) %>%filter(crf=="h51" | crf=="h54") %>% filter(date=="2019-12-16") %>%  arrange(desc(date))

all_crfs %>% filter(id=="35505" & is.na(visit) ) %>%filter(crf=="h51") %>% filter(date="2020-08-27") %>%  arrange(desc(date))

data_intervencion %>% transmute(id, crf, as.Date(date)) %>% anti_join(
  all_crfs %>% transmute(id, crf, as.Date(date)+ lubridate::days(1) ) %>% filter(crf=="h51" | crf=="h54" | crf=="h52")
)
all_crfs %>% group_by(visit) %>% count()

#+lubridate::days(1)
#all_crfs %>% select(1:4,formulario=crf)

e8_realizados<-read_csv("D:/Descargas/e8_enviados.csv")
e8_realizados<-e8_realizados %>% mutate(id=as.character(E8))

e8_realizados %>% select(id) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e8_date)) %>% select(id)  
)


e7<-e7 %>% mutate_all(as.character)
e7 %>%  select(id) %>%  anti_join(
  arm2 %>% filter(!is.na(e7_date)) %>% select(id)   
)


crf_intervencion<-repeats %>%
  filter(!is.na(h54_date) | !is.na(h51_date)) %>%
  select(h51_hhid, h51_date, h54_hhid, h54_date) %>%
  rownames_to_column() %>%
  gather(key, value, -rowname, na.rm = TRUE) %>%
  separate(key, into = c("crf", "var")) %>%
  spread(var, value) %>%
  select(-rowname, id = hhid)


crf_intervencion %>% filter(id=="35144") %>%
  filter(crf=="h51") %>% filter(date=="2020-08-31") %>%  arrange(desc(date))


