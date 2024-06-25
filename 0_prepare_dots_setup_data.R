# load use packages
library(package = "tidyverse")
library(package = "crosstalk")

###CARGA DE DATOS EMORY Y UVG
# Pre-screening information (s0)
source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Load helper functions
source(file = "scripts/zz_output.R")

# Emory RedCap dictionary

source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Stops knitting if lpg delivery data is not up to date
source(file = "scripts/uso-estufas.R", encoding = "UTF-8")

#fecha de la semana
fecha1<-'2019-01-21'
fecha2<-'2019-01-27'


# Collect the necessary data
dots_setup_rc <- gt_emory_data %>%
  mutate(irc = "guatemala") %>%
  select(
    irc, HHID = id, redcap_event_name,
    matches("h40_.*(visit|time|date|dot|area|stove)"),
    # remove because we are not using it but was included because it matched dot
    -matches("complete|problem|action|other|add")
  ) %>%
  mutate_all(as.character) %>%
  #----------------------------------------------------------------------------*
  # Reshape and subset the data to the variables that are needed
  #----------------------------------------------------------------------------*
  gather(
    key = column, value = value,
    matches("visit|time|date|dot|area|stove"),
    -matches("h40_date"),
    na.rm = TRUE
  ) %>%
  mutate(
    # explicit crf_copy on variable names
    column = if_else(
      condition = !grepl("_v[0-9]$", column),
      true = paste0(column, "_v1"),
      false = column
    ),
    # Fix non-standard variablenames
    column = gsub(
      pattern = "countinue",
      replacement = "continue",
      column
    )
  ) %>%
  # separate variable context
  extract(
    col = column,
    into = c("crf", "variable", "dot_correlative", "crf_copy"),
    regex = "(^[^_]+)_([^0-9]+)([0-9]+)?_v([0-9]+$)"
  ) %>%
  spread(key = variable, value = value) %>%
  # keep the correct date given the crf copy
  mutate(
    crf_date = if_else(
      condition = crf_copy == 1,
      true = h40_date,
      false = h40_date_v2
    )
  ) %>%
  arrange(HHID, crf_date) %>%
  # collect the data depending on the visit type
  select(
    irc, HHID, redcap_event_name, crf_date,
    crf_copy, dot_id = dot, visit,
    everything(), -crf, -h40_date, -h40_date_v2
  ) %>%
  gather(key = variable, value = value, matches("(ins|dl|stop)_(date|time)")) %>%
  separate(
    col = variable,
    into = c("step", "variable")
  ) %>%
  filter(!is.na(value)) %>%
  spread(key = variable, value = value) %>%
  rename(step_date = date) %>%
  arrange(HHID, dot_id, crf_date) %>%
  # Add arm
  left_join(
    gt_emory_data %>%
      filter(!is.na(s6_date) | !is.na(s6_arm) | !is.na(h50_date)) %>%
      select(HHID = id, arm = s6_arm, stove_installation = h50_date)
  ) %>%
  # Add community
  left_join(
    gt_emory_data %>%
      filter(!is.na(s1_date)) %>%
      select(HHID = s4_main_id, community = s1_community_name)
  ) %>%
  mutate_at(
    vars(matches("(date|stove_installation)")),
    funs(as.Date)
  ) %>%
  print()



# approximate month length
ml <- 365.25 / 12

# define all expected biweekly visits
all_expected_visits <- dots_setup_rc %>%
  group_by(HHID, arm) %>%
  summarize(
    dot_start = min(crf_date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # all dates every 15 days
    all_dates = map2(
      .x = dot_start, .y = arm,
      ~ {
        # end after 18 study months
        end_date <- .x + as.integer(ceiling(18*ml))
        total_days <- as.numeric(end_date - .x, unit = "days")
        
        if(.y == 1 | is.na(.y)){
          visit_freq <-  15
        } else {
          visit_freq <- 30
        }
        
        n_visits <- total_days %/% visit_freq
        
        
        next_date <- .x + (visit_freq * (n_visits + 1))
        
        all_dates <- seq.Date(from = .x, to = next_date, by = visit_freq)
        tibble(expected_date = as.character(all_dates))
      }
    )
  ) %>%
  unnest(all_dates) %>%
  mutate(
    expected_date = as.Date(expected_date)
  ) %>%
  print()


# compare actual vs expected

dots_completed_expected <- dots_setup_rc %>%
  group_by(HHID, crf_date, arm) %>%
  summarize(
    n_dots = n()
  ) %>%
  arrange(HHID, crf_date) %>%
  group_by(HHID) %>%
  mutate(
    freq = if_else(
      condition = arm == 1 | is.na(arm),
      true = 15L,
      false = 30L
    ),
    period = as.numeric(crf_date - min(crf_date), unit = "days") %/% first(freq)
  ) %>%
  ungroup() %>%
  full_join(
    all_expected_visits %>%
      group_by(HHID) %>%
      arrange(expected_date) %>%
      mutate(
        period = seq(0, n() - 1)
      ) %>%
      arrange(HHID, period) %>%
      ungroup() %>%
      select(-dot_start),
    by = c("HHID", "arm", "period")
  ) %>%
  arrange(HHID, period) %>%
  print()





# next dot dl based on most recent one
dots_setup_rc %>%
  group_by(HHID) %>%
  summarize(
    dot_start = min(crf_date, na.rm = TRUE),
    most_recent = max(crf_date),
    next_check = most_recent + lubridate::days(15),
    active_dots = dot_id[step != "stop" & crf_date == most_recent] %>% unique() %>% length(),
    which_dots = dot_id[step != "stop" & crf_date == most_recent] %>% unique() %>% paste(collapse = "; ")
  )



#------------------------------------------------------------------------------*
# Prepara dots setup data for analysis ----
#------------------------------------------------------------------------------*


# Implicitly label dot data recording events as "missions" (sets)
dots_setup_labeled <- dots_setup_rc %>%
  # Guatemala specific fixes
  mutate(
    dot_id = case_when(
      irc == "guatemala" & dot_id == "LBC" & crf_date == "2018-09-28" ~ "0119",
      TRUE ~ dot_id
    )
  ) %>%
  # Identify missions
  mutate(
    step = factor(step, levels = c("ins", "dl", "stop"))
  ) %>%
  arrange(irc, HHID, dot_id, crf_date, step) %>%
  group_by(irc, HHID, arm) %>%
  # Tag runs of records starting at "ins" and ending at "stop" %>%
  mutate(
    border = step == "ins",
    set = stats::filter(
      x = border, filter = 1, method = "recursive"
    )
  ) %>%
  # label sets without dowload events because we do not expect data
  group_by(irc, HHID, dot_id, set) %>%
  mutate(
    expect_data = any(step == "dl")
  ) %>%
  ungroup() %>%
  select(
    irc:visit, expect_data, everything()
  ) %>%
  print(n = Inf)

View(dots_setup_labeled)

# Get mission windows from RC data
dots_rc_missions <- dots_setup_labeled %>%
  group_by(irc, HHID, arm, dot_id, set, stove, stove_installation) %>%
  summarize(
    downloads = sum(expect_data),
    start_date = as.character(min(crf_date, na.rm = TRUE)),
    last_date = as.character(max(crf_date, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  arrange(irc, HHID, start_date) %>%
  print()


# Get the first date of dot setup for each IRC
dots_start <- dots_rc_missions %>%
  group_by(irc) %>%
  summarize(
    first_install_date = min(start_date, na.rm = TRUE)
  ) %>%
  ungroup()


# End of script
