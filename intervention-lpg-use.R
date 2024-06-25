#------------------------------------------------------------------------------*
# Record LPG use
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Configure analysis environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")


# Set plot theme
theme_set(
  theme_bw() +
    theme(
      title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "plain")
    )
)




#------------------------------------------------------------------------------*
# Get manual log data ----
#------------------------------------------------------------------------------*


# get emory data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")




# gas delivery team records

intervention_file <- "data/exports/CONTROL DE ESTUFAS Y GLP 400.xlsx"

inter_sheets <- readxl::excel_sheets(path = intervention_file)

inter_record <- inter_sheets %>%
  grep(pattern = "codigo", x = ., ignore.case = TRUE, value = TRUE) %>%
  set_names(.) %>%
  map(
    ~ readxl::read_excel(
      path = intervention_file, sheet = .x, skip = 5
    ),
    .id = "hoja"
  ) %>%
  map( ~ set_names(.x, gsub(" +", "_", names(.x)))) %>%
  print()






#------------------------------------------------------------------------------*
# LPG deliveries data ----
#------------------------------------------------------------------------------*


# harmonize column names
lpg_columns <- inter_record %>%
  map(names) %>%
  map_df(~ tibble(name = .x), .id = "tab") %>%
  mutate(
    type = case_when(
      grepl("entrega_estufa", name, ignore.case = TRUE) ~ "other",
      grepl("consumo", name, ignore.case = TRUE) ~ "request",
      grepl("cilindro|entrega|refil", name, ignore.case = TRUE) ~ "delivery",
      TRUE ~ "other"
    )
  ) %>%
  # assume column order implies chronology
  group_by(tab, type) %>%
  mutate(
    correlative = seq(from = 1, to = n())
  ) %>%
  ungroup() %>%
  mutate(
    new_name = if_else(
      condition = type %in% c("delivery", "request"),
      true = paste(type, correlative, sep = "_"),
      false = name %>%
        iconv(to = "ASCII//TRANSLIT") %>%
        tolower() %>%
        gsub("[^a-z0-9 _]", "", .) %>%
        gsub(" +", "_", .)
    )
  ) %>%
  filter(
    type != "other" | correlative < 9
  ) %>%
  print(n = Inf)

lpg_columns %>%
  pull(new_name) %>%
  unique()


lpg_records <- inter_record %>%
  map(
    ~ {
      .columns <- lpg_columns %>%
        filter(name %in% names(.x)) %>%
        filter(!duplicated(new_name)) %>%
        with(set_names(name, new_name))
      
      select(.x, !!! .columns) %>%
        mutate_all(as.character)
    }
  ) %>%
  bind_rows() %>%
  print()



# no    id    entrega     fecha
# 13    33022 delivery_1  17/08/2018/
# 19    33039 delivery_8  26/10/82018
# 20    33040           5 2019-09-24


excel_ref_date <- as.Date("1899-12-30")


lpg_data <- lpg_records %>%
  select(no, id = i_d, matches("delivery|request")) %>%
  # propagate id variables
  mutate_at(
    vars(no, id),
    funs(zoo::na.locf(., na.rm = FALSE))
  )


delivery_annotations <- lpg_data %>%
  filter(is.na(delivery_1)) %>%
  select(-no,) %>%
  gather(
    key = column, value = annotation,
    matches("delivery|request"),
    na.rm = TRUE
  ) %>%
  mutate(
    cyl_size = if_else(
      condition = grepl("cilindr", annotation) &
        grepl("lbs", annotation, ignore.case = TRUE),
      true = as.numeric(gsub("[^0-9]", "", annotation)),
      false = NA_real_
    )
  ) %>%
  print()


# show non dates
lpg_data %>%
  filter(!is.na(delivery_1)) %>%
  gather(
    key = column, value = date,
    matches("delivery|request"),
    na.rm = TRUE
  ) %>%
  filter(is.na(lubridate::ymd(date))) %>%
  filter(is.na(as.integer(date)))



lpg_actions <- lpg_data %>%
  filter(!is.na(delivery_1)) %>%
  gather(
    key = column, value = value,
    matches("delivery|request"),
    na.rm = TRUE
  ) %>%
  # fix excel dates
  mutate(
    value = recode(
      value,
      "20/11/21018" = "2018-11-20",
      "07/11/20185" = "2018-11-07",
      "256/11/2018" = "2018-11-25",
      "26/10/82018" = "2018-10-26"
    ),
    date = lubridate::ymd(value),
    date = case_when(
        !is.na(date) ~ date,
        is.na(date) &
          !is.na(as.integer(value)) ~ as.integer(value) + excel_ref_date
      ),
    date = case_when(
      id == "33087" & date == "2019-11-19" ~ as.Date("2018-11-19"),
      id == "33040" & date == "2019-09-24" ~ as.Date("2018-09-24"),
      id == "35050" & date == "2018-01-03" ~ as.Date("2019-01-03"),
      id == "33005" & date == "2016-01-16" ~ as.Date("2019-01-16"),
      
      id == "33114" & grepl("2018-01", as.character(date)) ~ as.Date(
        sub("2018-01", "2018-11", as.character(date))
      ),
      id == "33063" & grepl("2015", as.character(date)) ~ as.Date(
        sub("2015", "2018", as.character(date))
      ),
      id == "33005" & grepl("2016", as.character(date)) ~ as.Date(
        sub("2016", "2019", as.character(date))
      ),
      id == "33020" & grepl("2017", as.character(date)) ~ as.Date(
        sub("2017", "2018", as.character(date))
      ),
      TRUE ~ date
    )
  ) %>%
  left_join(
    delivery_annotations
  ) %>%
  separate(
    col = column, into = c("event", "correlative"),
    convert = TRUE
  ) %>%
  mutate(
    cyl_size = case_when(
      is.na(cyl_size) & event == "delivery" ~ 25,
      TRUE ~ cyl_size
    ),
    event = factor(event, levels = c("delivery", "request"))
  ) %>%
  # spread(key = event, value = date) %>%
  arrange(id, correlative) %>%
  select(-value, -no) %>%
  print()

View(lpg_deliveries)
lpg_deliveries <- lpg_actions %>%
  filter(event == "delivery") %>%
  select(-event, -annotation, delivery_date = date) %>%
  left_join(
    lpg_actions %>%
      filter(event == "request") %>%
      mutate(
        correlative = correlative + 1L
      ) %>%
      select(-event, -cyl_size, -annotation, request_date = date),
    by = c("id", "correlative")
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s6_date) | !is.na(h50_date)) %>%
      select(
        id,
        randomization_date = s6_date,
        arm = s6_arm,
        stove_install_date = h50_date
      )
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s1_date)) %>%
      select(
        id = s4_main_id,
        community = s1_community_name
      )
  ) %>%
  mutate(
    # ajustar entrega en instalación porque son 2 cilindros
    cyl_size = case_when(
      stove_install_date == delivery_date | correlative == 1 ~ 2 * cyl_size,
      TRUE ~ cyl_size
    ),
    # qc
    check = case_when(
      correlative != 1 & is.na(request_date) ~ "falta fecha solicitud",
      is.na(randomization_date) | is.na(arm) | arm != 1 ~ "error en s6",
      is.na(stove_install_date) | 
        (correlative == 1 & stove_install_date != delivery_date) ~ "comparar con h50"
    )
  ) %>%
  group_by(id) %>%
  mutate(
    most_recent = correlative == max(correlative)
  ) %>%
  ungroup() %>%
  select(
    community,correlative, id, randomization_date, arm, stove_install_date,
    request_date, delivery_date, cyl_size, most_recent, check
  ) %>%
  arrange(id, correlative) %>%
  print()




#------------------------------------------------------------------------------*
# Evaluate delivery data ----
#------------------------------------------------------------------------------*



hh_lpg_use <- lpg_deliveries %>%
  group_by(community  ,id, randomization_date, arm, stove_install_date) %>%
  mutate(
    days_since_last = as.numeric(
      delivery_date - lag(delivery_date), units = "days"
    )
  ) %>%
  summarize(
    deliveries = n(),
    total_days = as.numeric(
      max(delivery_date) - min(delivery_date), units = "days"
    ),
    avg_days_since_last = mean(days_since_last, na.rm = TRUE),
    # total de gas que se ha entregado
    total_gas_delivered = sum(cyl_size ),
    # total menos el último cilindro, porque digamos que no lo han usado
    total_gas_used = total_gas_delivered - cyl_size[most_recent],
    cylinders = data_frame(cyl_size ) %>%
      count(cyl_size ) %>%
      mutate(
        cyl_size = recode(
          as.character(cyl_size ),
          "50" = "2 x 25"
        )
      ) %>%
      glue::glue_data("{cyl_size }:{n}") %>%
      paste(collapse = ", "),
    last_delivery = delivery_date[most_recent],
    next_expected = last_delivery + as.integer(floor(avg_days_since_last))
  ) %>%
  ungroup() %>%
  print(n = Inf)
##consumo por comunidad  
hh_lpg_use %>% count(community, wt=total_gas_used) %>% print()

hh_lpg_use %>% writexl::write_xlsx(
    path = paste0("output/reporte_lpg_", Sys.Date(), ".xlsx")
  )

hh_lpg_use %>% summarize(hh_lpg_use$community) %>% print()


hh_lpg_use %>%
  filter(total_days > as.numeric(Sys.Date() - as.Date("2018-07-30"), units = "days")) %>%
  select(id, total_days) %>%
  left_join(lpg_deliveries) %>%
  group_by(id) %>%
  filter(delivery_date %in% range(delivery_date)) %>% print()


# 
# 
# # lpg use per capita ---
# lpg_per_capita <- lpg_time %>%
#   filter(!is.na(days_since_last)) %>%
#   group_by(id) %>%
#   summarize(
#     deliveries = n(),
#     lpg_kg = deliveries * 25 / 2.20462,
#     lpg_days = sum(days_since_last)
#   ) %>%
#   left_join(
#     # Get data for people living in the house, besides the pw
#     gt_emory_data %>%
#       filter(!is.na(m10_date)) %>%
#       select(
#         id, visit,
#         matches("^m10_(age|dob|gender)")
#       ) %>%
#       mutate_at(
#         vars(matches("m10_")),
#         funs(as.character)
#       ) %>%
#       gather(key = column, value = value, -id, -visit, na.rm = TRUE) %>%
#       mutate(
#         column = column %>%
#           gsub(pattern = "([^0-9])$", replacement = "\\1_1", .)
#       ) %>%
#       separate(
#         col = column, into = c(NA, "variable", "count"),
#         convert = TRUE
#       ) %>%
#       spread(variable, value, convert = TRUE) %>%
#       group_by(id, visit) %>%
#       summarize(
#         people = n() + 1, # add the pregnant woman
#         min_age = min(age, na.rm = TRUE),
#         avg_age = mean(age, na.rm = TRUE),
#         max_age = max(age, na.rm = TRUE)
#       ) %>%
#       ungroup()
#   ) %>%
#   select(-id, -visit) %>%
#   print()
# 
# 
# 
# lpg_per_capita %>%
#   mutate(
#     annual = lpg_kg / lpg_days * 365.25 / people
#   ) %>% {
#     lpg_plot <- ggplot(.) +
#       geom_jitter(
#         aes(
#           x = people, y = annual,
#           color = deliveries / lpg_days * (365.25 / 12)
#         )
#       ) +
#       labs(
#         x = "People living in the house",
#         y = "Annual LPG consumption per capita (kg)",
#         color = "Tank deliveries\nper month"
#       ) +
#       scale_color_viridis_c() +
#       theme(
#         legend.position = c(1, 1),
#         legend.justification = c(1.01, 1.01),
#         legend.background = element_blank()
#       )
#     
#     plot(lpg_plot)
#     .
#   } %>%
#   summarize(
#     average_per_capita = mean(annual),
#     weighted_by_recip_hhsize = weighted.mean(annual, 1/people),
#     weighted_by_hhsize = weighted.mean(annual, people),
#     weighted_by_deliveries = 
#       weighted.mean(
#         annual,
#         deliveries / (lpg_days * (365.25/12))
#       )
#   ) %>%
#   gather() %>%
#   mutate(
#     description = "."
#   )
# 
# 
# writexl::write_xlsx(lpg_per_capita, path = "output/lpg_consumption.xlsx")
# ggsave(
#   filename = "output/lpg-annual-per-capita.pdf", plot = last_plot()
# )
# 

