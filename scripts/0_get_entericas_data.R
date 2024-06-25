#library
library(package = "tidyverse")


# find most recent files
entericas_file <-list.files(
  path = "data/exports",
  pattern = "HAPINentericas_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINentericas_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()


# Manually select data export
data_entericas <- read_csv(
  file = entericas_file$file,
  col_types = cols(.default = col_character())
) %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$"), c39_date),
    list(as.Date)
  ) %>%
  mutate(
    visit = redcap_event_name %>%
      sub("_arm.*", "", .) %>%
      recode(
        .,
        .default = .
      ) %>%
      factor(
        levels = c("b2", "b3", "b4")
      )
    ) %>%
  print()
