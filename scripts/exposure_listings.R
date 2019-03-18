#------------------------------------------------------------------------------*
# Process exposure data ----
#------------------------------------------------------------------------------*


source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")


exposure_data <- gt_emory_data %>%
  select(
    visit, redcap_event_name, id, matches("^h41b?_"),
    m11_date, m14a_date
  ) %>%
  mutate_at(
    vars(matches("date|time|end|_st")),
    list(as.character)
  )


# Get h41b (duplicates) data
duplicates <- exposure_data %>%
  filter( visit %in% c("baseline", "p1", "p2", "b1", "b2", "b3", "b4") ) %>%
  select(
    id, visit,
    matches("^h41b_")
  ) %>%
  filter(!is.na(h41b_date)) %>%
  select(
    id, visit,
    date_start = h41b_date,  n_installed = h41b_device,
    matches("^h41_"), matches("^h41b_")
  ) %>%
  gather(
    key = variable, value = value,
    matches("(type|filter|pump|ecm|time|stop|end|envir)[1-9]"), -matches("other")
  ) %>%
  filter(
    # Value not empty
    !is.na(value),
    # and not declared NA, ie. 888
    !grepl("^88+$", value) # this literally means value is not only eights, from 2 to any number
  ) %>%
  select(-matches("h41b_")) %>%
  extract(
    col = variable, into = c("crf", "variable", "dup_correlative"),
    regex = "([^_]+)_([a-z]+)([0-9]+)"
  ) %>%
  spread(key = variable, value = value) %>%
  # Decode values
  mutate(
    envir = recode(
      envir,
      "1" = "kitchen 1",
      "2" = "kitchen 2",
      "3" = "sleeping area",
      "4" = "resting area 1",
      "5" = "resting area 1",
      "6" = "outside",
      "7" = "aap",
      "8" = "pregnant woman",
      "9" = "other adult woman",
      "10" = "child"
    ),
    type = recode(
      type,
      "1" = "ECM",
      "2" = "Bomba y filtro (SKC)",
      "3" = "Bomba y filtro (HPEMs)",
      "4" = "Bomba y filtro (BGI)",
      "5" = "PATS+",
      "6" = "UPAS",
      "7" = "Monitor de CO Lascar",
      "8" = "Sistema de monitoreo Nexleaf",
      "9" = "Sistema de monitoreo Sweetsense",
      "10" = "Otro"
    ),
    instrument = case_when(
      type == "ECM" ~ "ecm",
      grepl("Bomba", type) ~ "pump",
      type == "PATS+" ~ "pats",
      type == "UPAS" ~ "upas",
      grepl("lascar", type, ignore.case = TRUE) ~ "lascar",
      grepl("nexleaf", type, ignore.case = TRUE) ~ "nexleaf",
      grepl("sweetsense", type, ignore.case = TRUE) ~ "sweetsense",
      TRUE ~ "error"
    ),
    instrument_id = case_when(
      instrument == "pump" ~ pump,
      instrument == "ecm" ~ ecm,
      instrument == "pats" ~ ecm,
      instrument == "upas" ~ ecm,
      instrument == "lascar" ~ ecm,
      TRUE ~ "error"
    ),
    exposure_sampling_started = !is.na(time),
    exposure_sampling_completed = !is.na(end)
  ) %>%
  select(
    id, visit, matches("expected"), matches("exposure_sampling"),
    date_start, matches("date_end"),
    crf,
    target = envir, instrument,
    start_time = time, instrument_stopped = stop, end_time = end,
    filter_id = filter,
    dup_type = type, dup_correlative, instrument_id = ecm
  )



# Subset to relevant data
main_study <- exposure_data %>%
  filter( visit %in% c("baseline", "p1", "p2", "b1", "b2", "b3", "b4") ) %>%
  mutate(
    expected = case_when(
      # Data on baseline baseline
      visit %in% c("baseline", "p1", "p2") ~ !is.na(m11_date) & !is.na(m14a_date),
      visit %in% c("b1", "b2", "b4") ~ !is.na(m11_date)
    ),
    exposure_sampling_started = !is.na(h41_date), # or record in RC Guatemala
    exposure_sampling_completed = !is.na(h41_date_2)
  ) %>%
  select(
    id, visit, expected, matches("exposure_sampling"),
    date_start = h41_date, date_end = h41_date_2,
    matches("^h41_"),
  ) %>%
  gather(
    key = variable, value = value,
    matches("(kap|sap|rap|hop|aap|_o_|_m_|_c_|_b_).*_(stop|end|st|id|fid)")
  ) %>%
  filter(
    grepl("ecm|lascar|log|bgi|skc|har|beacon", variable),
    !is.na(value)
  ) %>%
  # separate variables
  separate(
    col = "variable", into = c("crf", "target", "instrument", "variable")
  ) %>%
  mutate(
    instrument = recode(
      instrument,
      log = "logger"
    ),
    variable = recode_factor(
      variable,
      st = "start_time",
      stop = "instrument_stopped",
      end = "end_time",
      fid = "filter_id",
      id = "instrument_id",
      id1 = "beacon_1",
      id2 = "beacon_2"
    ),
    target = recode(
      target,
      "kap1" = "kitchen 1",
      "kap2" = "kitchen 2",
      "sap" = "sleeping area",
      "rap1" = "resting area 1",
      "rap2" = "resting area 1",
      "hop" = "outside",
      "aap" = "aap",
      "m" = "pregnant woman",
      "o" = "other adult woman",
      "c" = "child",
      "b" = "blank"
    )
  ) %>%
  select(
    -matches("h41b?_")
  ) %>%
  spread(key = variable, value = value)



# bind main exposure measure and duplicates
all_instruments <- main_study %>%
  bind_rows(duplicates) %>%
  arrange(id, visit, date_start) %>%
  filter(!grepl("^*8{3,}", instrument_id)) %>%
  mutate(
    instrument_id = instrument_id %>%
      sub("^0*", "", .) %>%
      as.integer()
  )


# export for input into file loader
all_instruments %>%
  arrange(visit, id) %>%
  write_csv(
    path = paste0(
      "output/instruments/",
      gsub("[: ]", "-", Sys.time()), "_",
      "all_instruments.csv"
    )
  )



#------------------------------------------------------------------------------*
# Summaries ----
#------------------------------------------------------------------------------*

ecm_duplicates <- all_instruments %>%
  filter(
    instrument == "ecm"
  ) %>%
  select(id, visit, target, date_start, instrument_id) %>%
  group_by(id, visit, target) %>%
  mutate(
    n = paste0("ecm_", letters[seq(1, n())])
  ) %>%
  ungroup() %>%
  spread(key = n, value = instrument_id) %>%
  print()




# End of script

