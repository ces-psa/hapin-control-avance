
source(file = "scripts/0_get_gt_repeated.R", encoding = "UTF-8")
source(file = "scripts/0_get_emory_repeated.R", encoding = "UTF-8")



if(
  as.Date(gt_emory_repeat_file$export_time) <
  as.Date(gt_emory_file$export_time)
) stop("Debe actualizar los datos exportados de \"HAPIN-Guatemala-Repeated CRFs\"")

days_diff <- function(date1, date2){
  as.numeric(date1 - date2, units = "days")
}


# dots download ----
h54 <- gt_repeated %>%
  filter(crf == "h54") %>%
  select(id = record_id, record, matches("h54_")) %>%
  rename(record_id = record) %>%
  bind_rows(
    gt_emory_data %>%
      select(id, matches("h54_")) %>%
      mutate_all(as.character) %>%
      filter(!is.na(h54_date))
  ) %>%
  filter(!is.na(id)) %>%
  rownames_to_column() %>%
  gather(column, value, -id, -record_id, -rowname, na.rm = TRUE) %>%
  mutate(
    column = if_else(
      condition = !grepl("_v[0-9]+$", column),
      true = paste0(column, "_v1"),
      false = column
    )
  ) %>%
  extract(
    column, into = c("crf", "variable", "rep"),
    regex = "(^[^_]+)_(.+)_v([0-9]+)", convert = TRUE
  ) %>%
  spread(variable, value) %>%
  mutate(date = as.Date(date)) %>%
  print()


# gas delivery ----
h51_emory <- gt_emory_repeat_data %>%
  filter(grepl("h51_gas", redcap_event_name))


h51_data <- h51_emory


h54_mes <- h54 %>%
  filter(!is.na(date)) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s6_date) | visit == "baseline") %>%
      select(id, brazo = s6_arm)
  ) %>%
  mutate(
    year = date %>%
      lubridate::year(),
    month = date %>%
      lubridate::month(label = TRUE, abbr = FALSE)
  ) %>%
  group_by(brazo, id, year, month) %>%
  summarize(
    registros_h54 = n(),
    usos_estufa = sum(stove_use == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  print()


h53_mes <- gt_emory_data %>%
  select(
    visit, id, matches("h53_")
  ) %>%
  filter(!is.na(h53_date)) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s6_date)) %>%
      select(id, brazo = s6_arm)
  ) %>%
  mutate(
    year = h53_date %>%
      lubridate::year(),
    month = h53_date %>%
      lubridate::month(label = TRUE, abbr = FALSE)
  ) %>%
  group_by(brazo, id, year, month) %>%
  summarize(
    visitas_h53 = n()
  ) %>%
  ungroup() %>%
  print()
  

uso_casas <- h54_mes %>%
  left_join(h53_mes) %>%
  mutate(
    visitas_h53 = if_else(
      condition = is.na(visitas_h53),
      true = 0L,
      false = visitas_h53
    )
  ) %>%
  print()
  


#------------------------------------------------------------------------------*
# Resumir uso de estufas y refuerzo ----
#------------------------------------------------------------------------------*

# hubo uso en casa intervencion, pero no visita de refuerzo
uso_casas %>%
  filter(
    usos_estufa > 0, visitas_h53 < 1 | is.na(visitas_h53),
    brazo != 0
  )


# resumen por mes de uso y refuerzo
uso_casas %>%
  group_by(brazo, year, month) %>%
  summarize(
    casas = n(),
    sin_uso_con_refuerzo = sum(usos_estufa == 0 & visitas_h53 > 0, na.rm = TRUE),
    con_uso_y_refuerzo = sum(usos_estufa > 0 & visitas_h53 > 0, na.rm = TRUE),
    con_uso_sin_refuerzo = sum(usos_estufa > 0 & visitas_h53 < 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    brazo = recode(
      brazo,
      `0` = "Control",
      `1` = "Intervención",
      .missing = "No aleatorizada"
    ) %>%
      if_else(
        condition = duplicated(.),
        true = "",
        false = .
      )
  ) %>%
  knitr::kable()


uso_casas %>%
  filter(brazo == 1) %>%
  filter(usos_estufa > 0, visitas_h53 == 0)







#------------------------------------------------------------------------------*
# Dot download use reports ----
#------------------------------------------------------------------------------*


dot_use <- gt_emory_data %>%
  select(id, matches("h40_(date|use_num|(stove[0-9](_v[0-9])?$))")) %>%
  mutate_all(as.character) %>%
  group_by(id) %>%
  mutate(corr = seq(1, n())) %>%
  ungroup() %>%
  gather(key = column, value = value, -id, -corr, na.rm = TRUE) %>%
  mutate(
    column = if_else(
      condition = !grepl("_v[0-9]+$", column),
      true = paste0(column, "_v1"),
      false = column
    )
  ) %>%
  extract(
    column, into = c("crf", "variable", "rep"),
    regex = "(^[^_]+)_(.+)_v([0-9]+)", convert = TRUE
  ) %>%
  spread(variable, value) %>%
  gather(column, value, matches("(stove|use_)"), na.rm = TRUE) %>%
  mutate(
    dot_n = gsub("(use_num|stove)([0-9]*)", "\\2", column),
    column = gsub("[0-9]*", "", column)
  ) %>%
  spread(column, value, convert = TRUE) %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    stove = recode(
      stove,
      "6" = "gas",
      "7" = "electric",
      "555" = "other",
      .default = "biomass"
    )
  ) %>%
  arrange(id, date)


dot_month <- dot_use %>%
  group_by(id, year, month)







#------------------------------------------------------------------------------*
# Date events ----
#------------------------------------------------------------------------------*

date_range <- c(
  dot_use$date,
  gt_emory_data$h54_date,
  gt_emory_data$h50_date,
  h54$date,
  Sys.Date()
) %>%
  range(na.rm = TRUE)

all_dates <- seq.Date(from = date_range[1], to = date_range[2], by = "1 day")



# h54 - reported traditional stove use ----
h54_day <- h54 %>%
  filter(!is.na(date)) %>%
  group_by(id = record_id, date) %>%
  summarize(
    registros_h54 = n(),
    usos_estufa = sum(stove_use == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  print()


# h53 - reported behavioural intervention ----
h53_day <- gt_emory_data %>%
  select(
    visit, id, matches("h53_")
  ) %>%
  filter(!is.na(h53_date)) %>%
  group_by(id, date = h53_date) %>%
  summarize(
    visitas_h53 = n()
  ) %>%
  ungroup() %>%
  print()

# h51 - gas delivery ----
h51_day <- h51_data %>%
  select(id, date = h51_date)


# all study visits ----
emory_crfs <- gt_emory_data %>%
  filter(visit != "tamizaje") %>%
  select(id, visit, matches("^[^_]+_(date|by)$")) %>%
  mutate_all(as.character) %>%
  gather(key = column, value = value, -id, -visit, na.rm = TRUE) %>%
  separate(column, into = c("crf", "variable")) %>%
  spread(variable, value) %>%
  filter(complete.cases(.)) %>%
  mutate(date = as.Date(date))

repeat_crfs <- list(
  h51_emory,
  rename(h54, id = record_id)
) %>%
  bind_rows() %>%
  select(id, visit, matches("^[^_]+_(date|by)$")) %>%
  mutate_all(as.character) %>%
  group_by(id) %>%
  mutate(
    rep = seq(1, n())
  ) %>%
  ungroup() %>%
  gather(key = column, value = value, -id, -visit, -rep, na.rm = TRUE) %>%
  separate(column, into = c("crf", "variable")) %>%
  spread(variable, value) %>%
  filter(complete.cases(.)) %>%
  mutate(date = as.Date(date))

all_crfs <- list(
  emory_crfs,
  repeat_crfs,
  h54 %>%
    select(id = record_id, crf, by, date)
) %>%
  bind_rows()


house_visited <- all_crfs %>%
  group_by(id, date) %>%
  summarize(
    people_visited = by %>% unique() %>% length()
  )


# for all enrolled households
intervention_events <- gt_emory_data %>%
  filter(
    !is.na(m11_date) | !is.na(h41_date) | !is.na(m10_date), visit == "baseline"
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s4_date), !is.na(s4_main_id)) %>%
      select(id = s4_main_id, community = s1_community_name) %>%
      mutate_all(as.character)
  ) %>%
  select(
    id, arm = s6_arm, baseline_end = h42_date,
    rand_date = s6_date, stove_date = h50_date,
    left_pw = e3_date, left_oaw = e3_date_o, left_c = e3_date_c, community
  ) %>%
  # for every date so far during the study
  crossing(date = all_dates) %>%
  filter(date >= stove_date) %>%
  # all known visits
  left_join(house_visited) %>%
  # filled h54
  left_join(h54_day) %>%
  # record aparent use from dot graph during download
  left_join(
    dot_use %>%
      filter(stove %in% c("biomass", "other")) %>%
      select(id, date, use_num, stove) %>%
      group_by(id, date, stove) %>%
      summarize(
        dot_graph_stove_use = sum(use_num, na.rm = TRUE)
      ) %>%
      ungroup()
  ) %>%
  # filled h53
  left_join(h53_day) %>%
  # any recorded
  filter(
    !is.na(people_visited) |
    !is.na(dot_graph_stove_use) |
    !is.na(registros_h54) |
    !is.na(usos_estufa) |
    !is.na(visitas_h53) |
      date == Sys.Date()
  ) %>%
  print()

intervention_events %>%
  filter(id == "35016")



intervention_events %>%
  mutate(
    period = if_else(
      condition = date > as.Date("2018-10-01"),
      true = "october >",
      false = "pre october"
    ),
    used_h54 = !is.na(registros_h54)
  ) %>%
  group_by(arm, period) %>%
  summarize(
    used_h54 = paste(round(mean(used_h54) * 100), "%")
  ) %>%
  ungroup() %>%
  spread(period, used_h54)


# Flags
# 
# any use recorded in h40 dots (looking at the Geocene app graph)
# h54 observed use 




#' stove use... days to <- not available unless look at dots data from geocene
#' notice_use: flags from above, notice stove use... days to h53
#' h53
#' days: mediana y rango
#' h53 flag: visit > 7 days after notice_use
#' 
#' prioritize by number of reports
#' prioritize by recency

#' report with IDs to Anaite, Rosy, Sayury

# Additional
#
# h51 - expected gas use?
# h52 -- delays for repairs
# h44 missing from p1, then on p2


intervention_events %>%
  group_by(arm, id) %>%
  mutate(
    observed = any(usos_estufa > 0),
    dots = any(dot_graph_stove_use > 0),
    h53 = any(!is.na(visitas_h53) & visitas_h53 > 0)
  ) %>%
  ungroup() %>%
  count(arm, id, observed, dots, h53) %>%
  filter(arm == 1) %>%
  arrange(!(observed | dots), h53, id)



all_use_periods <- intervention_events %>%
  mutate(
    observed_use = usos_estufa > 0 & !is.na(usos_estufa),
    dots_use = dot_graph_stove_use > 0 & !is.na(dot_graph_stove_use),
    any_use = observed_use | dots_use,
    h53_visit = visitas_h53 > 0 & !is.na(visitas_h53)
  ) %>%
  select(
    date, id, rand_date, arm, stove_date,
    people_visited, observed_use, stove, dots_use, any_use, h53_visit,
    stove_date, community
  ) %>%
  arrange(id, date) %>%
  # tag use periods
  group_by(id) %>%
  mutate(
    use_period = any_use %>%
      magrittr::or(
        lag(h53_visit, default = FALSE)
      ) %>%
      stats::filter(filter = 1, method = "recursive") %>%
      as.numeric()
  ) %>%
  group_by(arm, id, community, stove_date, use_period, stove) %>%
  summarize(
    date_start = min(date, na.rm = TRUE),
    observed_use = any(observed_use, na.rm = TRUE),
    dots_use = any(dots_use, na.rm = TRUE),
    any_use = any(any_use, na.rm = TRUE),
    any_h53 = any(h53_visit, na.rm = TRUE),
    h53_date = min(date[h53_visit], na.rm = TRUE)
  ) %>%
  arrange(arm, id, date_start) %>%
  mutate(
    date_end = lead(date_start - 1, default = Sys.Date()),
    total_days = days_diff(date_end, date_start),
    days_to_h53 = if_else(
      condition = any_use,
      true = days_diff(
        date1 = pmin(
          date_end, h53_date,
          na.rm = TRUE
        ),
        date2 = date_start
      ) + 1,
      false = NA_real_
    ),
    flag_use = arm == 1 & any_use,
    flag_h53 = arm == 1 & any_use & (!any_h53 | days_to_h53 > 7)
  ) %>%
  arrange(desc(arm), id, date_start) %>%
  select(
    arm, id, date_start, date_end, total_days,
    observed_use, dots_use, stove, any_use,
    any_h53, h53_date, days_to_h53, flag_use, flag_h53,
    stove_date, community
  ) %>%
  ungroup()




grouped_use_periods <- all_use_periods %>%
  # filter(id == "33006") %>%
  group_by(id) %>%
  mutate(
    period_step = case_when(
      !any_use & lag(any_h53, default = FALSE) ~ 1L,
      !any_use ~ 0L,
      any_use & !lag(any_use, default = FALSE) ~ 1L,
      any_use & lag(any_h53, default = TRUE) ~ 1L,
      TRUE ~ 0L
    ),
    
    period = cumsum(period_step)
  ) %>%
  group_by(
    arm, id, period,
    stove_date, community, stove
  ) %>%
  summarize(
    date_start = min(date_start),
    date_end = max(date_end),
    total_days = days_diff(date_end, date_start) + 1,
    observed_uses = sum(observed_use, na.rm = TRUE),
    dots_uses = sum(dots_use, na.rm = TRUE),
    total_uses = sum(any_use, na.rm = TRUE),
    total_h53 = sum(any_h53, na.rm = TRUE),
    days_no_h53 = sum(days_to_h53, na.rm = TRUE),
    h53_dates = h53_date %>%
      as.character() %>%
      na.omit() %>%
      as.Date() %>%
      paste(collapse = ", ")
  ) %>%
  ungroup() %>%
  select(-period) %>%
  print()


summary_uses <- grouped_use_periods %>%
  group_by(
    arm, id,
    stove_date, community, stove
  ) %>%
  mutate(
    any_use = any(total_uses > 0),
    sum_total_uses = sum(total_uses, na.rm = TRUE),
    sum_days_no_h53 = sum(days_no_h53, na.rm = TRUE)
  ) %>%
  arrange(sum_total_uses, sum_days_no_h53, id, date_start) %>%
  filter(arm == 1, any_use) %>%
  select(-any_use, -sum_total_uses, -sum_days_no_h53) %>%
  summarize(
    date_start = min(date_start),
    date_end = max(date_end),
    total_days = days_diff(date_end, date_start) + 1,
    observed_uses = sum(observed_uses, na.rm = TRUE),
    dots_uses = sum(dots_uses, na.rm = TRUE),
    total_uses = sum(total_uses, na.rm = TRUE),
    total_h53 = sum(total_h53, na.rm = TRUE),
    days_no_h53 = sum(days_no_h53, na.rm = TRUE),
    h53_dates = h53_dates %>%
      if_else(. == "", NA_character_, .) %>%
      na.omit() %>%
      paste(collapse = ", ")
  ) %>%
  print(n = 50)



# write out data

list(
  resumen_casa = summary_uses %>%
    filter(arm == 1) %>%
    arrange(desc(total_uses), desc(days_no_h53)) %>%
    rename(
      dias_sin_h53 = days_no_h53
    ),
  detalle_casas = grouped_use_periods %>%
    filter(arm == 1) %>%
    group_by(id) %>%
    do({
      bind_rows(
        .,
        tibble(id = "")
      )
    }) %>%
    ungroup() %>%
    rownames_to_column(var = "orden_original") %>%
    rename(
      dias_sin_h53 = days_no_h53
    )
) %>%
  writexl::write_xlsx(path = "output/uso-estufa-tradicional.xlsx")
