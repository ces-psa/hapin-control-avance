library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")


rc_metadata <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_metadata"
  )
) %>%
  as_tibble() %>%
  left_join(
    DBI::dbGetQuery(
      conn = data_base,
      statement = paste(
        "select project_id, project_name, app_title from redcap_projects"
      )
    )
  ) %>%
  print() %>%
  count(
    project_id, project_name, app_title, form_name
  ) %>% 
  print()

rc_metadata %>% write_csv("output/projectos.csv")
