#------------------------------------------------------------------------------*
# Datos de contacto de participantes ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")


gt_rep_metadata <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_metadata",
    "where project_id = 53"
  )
) %>%
  as_tibble() %>%
  print()



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
  as_tibble() %>%
  left_join(
    gt_rep_metadata %>%
      select(project_id, field_order, field_name, element_type, form_name)
  ) %>%
  arrange(crf, field_order, value) %>%
  mutate(
    field_name = if_else(
      condition = element_type == "checkbox",
      true = paste0(field_name, "___", value),
      false = field_name
    ) %>%
      factor(levels = unique(.)),
    value = if_else(
      condition = element_type == "checkbox",
      true = "1",
      false = value
    )
  ) %>%
  select(crf, record, field_name, value) %>%
  group_by(crf, record, field_name) %>%
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  spread(field_name, value) %>%
  select(record, id, record_id, everything()) %>%
  print()



# Disconnect from database
DBI::dbDisconnect(conn = data_base)

rm(gt_rep_metadata, data_base)
