
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






#REVISAR DATOS INTERVENCION
crf_intervencion<-repeats %>%
  filter(!is.na(h54_date) | !is.na(h51_date)) %>%
  select(h51_hhid, h51_date, h54_hhid, h54_date) %>%
  rownames_to_column() %>%
  gather(key, value, -rowname, na.rm = TRUE) %>%
  separate(key, into = c("crf", "var")) %>%
  spread(var, value) %>%
  select(-rowname, id = hhid)


crf_intervencion %>% filter(id=="33488") %>%
  filter(crf=="h51") %>% filter(date=="2020-08-27") %>%  arrange(desc(date))
