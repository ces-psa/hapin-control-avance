#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
gt_hapin_II_file <- list.files(
  path = "data/exports", pattern = "HAPINIIGuatemala_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) 


gt_hapin_II_data <- gt_hapin_II_file %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    id=record_id
  ) %>% select(-record_id) %>% mutate(
    visit=case_when(
      redcap_event_name=="24_month_arm_1" ~ "b5",
      redcap_event_name=="year_3_q1_36m_arm_1" ~ "b6",
      redcap_event_name=="year4_q1_48m_arm_1" ~ "b7",
      redcap_event_name=="year5_arm_1" ~ "b8",
      redcap_event_name=="as_needed_arm_1" ~ "ss1",
      redcap_event_name=="year4q3_54m_arm_1" ~ "b7b8",
      TRUE ~ NA_character_
    )
  ) %>% 
  select( redcap_event_name, id, everything()) %>% filter(id!="99999") 


# End of script
