# FIRST LOAD DATA

library(package = "tidyverse")


# find most recent files
#amr1 file
h41_file <- list.files(
  path = "data/h41",
  pattern = "HAPINGuatemalaMainSt-H41completo_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-H41completo_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()


# Manually select data export
h41 <- read_csv(
  file = h41_file$file,
  col_types = cols(.default = col_character())
) %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$")),
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
        levels = c("baseline", "p1", "p2", "b1", "b2", "b3", "b4")
      )
  ) %>%
  print()


#todos los adtaos de H41
h41<-h41 %>% filter(!is.na(h41_date))
h41 %>% group_by(visit) %>% count()
#identificar errores en la pregunta A1 tipo de visita
qc_tipo_visita<-h41 %>% mutate(
  field=case_when(
    visit=="baseline" & (h41_visit=="2" | is.na(h41_visit)) ~ "Incorrect value in baseline",
    visit=="p1" & (h41_visit=="1" | is.na(h41_visit)) ~ "Incorrect value in p1",
    visit=="p2" & (h41_visit=="1" | is.na(h41_visit)) ~ "Incorrect value in p2",
    visit=="b1" & (h41_visit=="1" | is.na(h41_visit)) ~ "Incorrect value in b1",
    visit=="b2" & (h41_visit=="1" | is.na(h41_visit)) ~ "Incorrect value in b2",
    visit=="b4" & (h41_visit=="1" | is.na(h41_visit)) ~ "Incorrect value in b4",
    TRUE ~ NA_character_
    )
) %>% select(id, h41_date, h41_by, visit, field) %>% filter(!is.na(field)) %>% mutate(
  crf="h41 main study"
) %>% 
  transmute(QueryID="Guatemala_H41",
            DateQueryGenerated=Sys.Date(),
            CRF="H41",
            HHID=id,
            timepoint=visit,
            Field=paste0(crf, field),
            QueryType="Incorrect value",
            QueryDescription="Incorrect type visit Question A1",
            QueryGeneratedBy="auto",
            ResponseDate=" " ,
            ResponseFrom=" ",
            ResponseType=" ",
            FinalResolutionDate=" "
            
            
            
  ) %>% print()


#h41_k_change
#identificar errores en la pregunta A1 tipo de visita
qc_a1a<-h41 %>% mutate(
  field=case_when(
    visit=="baseline" & (!is.na(h41_k_change)) ~ "Incorrect value",
    visit!="baseline" & is.na(h41_k_change) ~ "Missing value",
    TRUE ~ NA_character_
  )
) %>% select(id, h41_date, h41_by, visit, field) %>% filter(!is.na(field)) %>% mutate(
  crf="h41 main study"
) %>% 
  transmute(QueryID="Guatemala_H41",
            DateQueryGenerated=Sys.Date(),
            CRF="H41",
            HHID=id,
            timepoint=visit,
            Field=paste0(crf, field, "on ", visit),
            QueryType="Incorrect value",
            QueryDescription="Incorrect or missing value Question A1a",
            QueryGeneratedBy="auto",
            ResponseDate=" " ,
            ResponseFrom=" ",
            ResponseType=" ",
            FinalResolutionDate=" "
            
            
            
  ) %>% print()


#h41_pri_roof
#Missing value A2 techo
qc_a2<-h41 %>% mutate(
  field=case_when(
    visit=="baseline" & (is.na(h41_pri_roof)) ~ "Missing value",
    h41_k_change=="1" & is.na(h41_k_change) ~ "Missing value",
    TRUE ~ NA_character_
  )
) %>% select(id, h41_date, h41_by, visit, field) %>% filter(!is.na(field)) %>% mutate(
  crf="h41 main study"
) %>% 
  transmute(QueryID="Guatemala_H41",
            DateQueryGenerated=Sys.Date(),
            CRF="H41",
            HHID=id,
            timepoint=visit,
            Field=paste0(crf, field, "on ", visit),
            QueryType="Incorrect value",
            QueryDescription="Missing value Question A2",
            QueryGeneratedBy="auto",
            ResponseDate=" " ,
            ResponseFrom=" ",
            ResponseType=" ",
            FinalResolutionDate=" "
            
    
            
  ) %>% print()



#h41_pri_wall
#Out range or Missing  value A2a techo
qc_a2a<-h41 %>% mutate(
  field=case_when(
    h41_pri_roof=="1" & (is.na(h41_pri_wall)) ~ "Missing value",
    as.numeric(h41_pri_wall) < 0 ~ "Out range value",
    as.numeric(h41_pri_wall) > 10 ~ "Out range value",
    TRUE ~ NA_character_
  )
) %>% select(id, h41_date, h41_by, visit, field) %>% filter(!is.na(field)) %>% mutate(
  crf="h41 main study"
) %>% 
  transmute(QueryID="Guatemala_H41",
            DateQueryGenerated=Sys.Date(),
            CRF="H41",
            HHID=id,
            timepoint=visit,
            Field=paste0(crf, field, "on ", visit),
            QueryType="Out range or Missing value",
            QueryDescription="Our Range or Missing value Question A2a",
            QueryGeneratedBy="auto",
            ResponseDate=" " ,
            ResponseFrom=" ",
            ResponseType=" ",
            FinalResolutionDate=" "
            
            
            
  ) %>% print()


#write xls file QC similar to DMC
qc_tipo_visita %>% bind_rows(
  list(
    qc_a1a,
    qc_a2,
    qc_a2a
  ) 
) %>%  writexl::write_xlsx(paste0("output/QC_H41_",Sys.Date(),".xlsx"))



dt_h41 <- dt_h41 %>% filter(!is.na(h41_date)) %>%  mutate_all(as.character)
dt_h41 %>% filter(!is.na(h41_date)) %>% group_by(h41_visit) %>% count()
dt_h41 %>% select(id,redcap_event_name, h41_visit, h41_by,h41_pri_roof,h41_pri_wall,
                  h41_k_length, h41_k_width,h41_k_diameter) %>% 
  filter(h41_k_diameter!="888")
group_by(h41_k_diameter) %>% count() %>% print(n=Inf)
