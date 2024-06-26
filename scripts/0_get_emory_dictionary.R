
# Get Emory RedCap dictionary
gt_emory_dict_file <- list.files(
  path = "data/dictionaries", pattern = "MainSt.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

gt_emory_dictionary <- gt_emory_dict_file %>%
  pull(file) %>%
  read_csv(show_col_types = FALSE) %>%
  set_names(
    names(.) %>%
      make.names(unique = TRUE, allow_ = TRUE) %>%
      tolower() %>%
      gsub("[.]+", "_", .)
  ) %>%
  rownames_to_column() %>%
  extract(form_name, into = c("crf", "title"), regex = "^([^_]+)_(.*)") %>% 
  transmute(
    rowname, crf, variable = variable_field_name, field_type,
    title, text = field_label,
    config = choices_calculations_or_slider_labels,
    valid_min = text_validation_min, valid_max = text_validation_max,
    relevance = branching_logic_show_field_only_if_,
    required = required_field_
  )
