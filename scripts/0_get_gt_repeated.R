#------------------------------------------------------------------------------*
# Datos de contacto de participantes ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")


gt_repeated <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 53",
    "and event_id in (197, 227, 230)"
  )
) %>%
  mutate(
    crf = recode(
      event_id,
      `197` = "h54",
      `227` = "h51",
      `230` = "birth_call"
    )
  ) %>%
  group_by_at(
    vars(grep("^(?!value)", names(.), perl = TRUE))
  ) %>% print %>%
  mutate(
    n = n(),
    variable = if_else(
      condition = n > 1,
      true = paste0(field_name, "___", value),
      false = field_name
    )
  ) %>%
  ungroup() %>%
  mutate(
    field_name = factor(variable, levels = unique(variable)),
    value = if_else(
      condition = grepl("___", field_name),
      true = "1",
      false = value
    )
  ) %>%
  filter(!duplicated(.)) %>%
  select(crf, record, field_name, value) %>%
  spread(field_name, value) %>%
  print()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
