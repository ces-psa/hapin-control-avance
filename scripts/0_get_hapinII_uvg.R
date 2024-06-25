#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
gt_hapin_II_uvg_file <- list.files(
  path = "data/exports", pattern = "HAPINIIGT_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


gt_hapin_II_uvg_data <- gt_hapin_II_uvg_file %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    id=record_id
  ) %>% select(-record_id) %>% 
  select( redcap_event_name, id, everything()) %>% filter(id!="99999") %>% print()


# End of script
