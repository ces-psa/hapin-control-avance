#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
gt_emory_repeat_file <- list.files(
  path = "data/exports", pattern = "Repeat.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


gt_emory_repeat_data <- gt_emory_repeat_file %>%
  pull(file) %>%
  read_csv() %>%
  mutate_at(
    vars(matches("hhid")),
    list(~ as.character(.))
  ) %>%
  mutate(
    # Default change for all "monthly" visits
    visit = "mensual",
    arm = case_when(
      grepl("^h46", redcap_event_name) ~ "ambient",
      grepl("^h51", redcap_event_name) ~ "gas"
    ),
    cor_crf = case_when(
      !is.na(h51_hhid) & is.na(h46b_hhid) ~ "h51",
      !is.na(h46b_hhid) & is.na(h51_hhid) ~ "h46",
      TRUE ~ NA_character_
    ),
    id = case_when(
      !is.na(h51_hhid) & is.na(h46b_hhid) ~ h51_hhid,
      !is.na(h46b_hhid) & is.na(h51_hhid) ~ h46b_hhid,
      TRUE ~ NA_character_
    ),
    manual_id = gsub("^ *([0-9]*).+", "\\1", record_id),
    is_id = grepl("^ *[0-9]{5} +", record_id),
    study_id = case_when(
      grepl("^3[35][0-9]{3}$", id) ~ id,
      grepl("^3[35][0-9]{3}$", manual_id) ~ manual_id
    )
  ) %>%
  select(redcap_event_name, visit, study_id, id, manual_id, is_id, everything())


# End of script
