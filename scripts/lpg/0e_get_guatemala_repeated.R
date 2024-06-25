#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
gt_repeat_file <- list.files(
  path = "data/exports", pattern = "Repeti.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


gt_repeat_data <- gt_repeat_file %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    vars(matches("date")),
    list(as.Date)
  )


# End of script
