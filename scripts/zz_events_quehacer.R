#------------------------------------------------------------------------------*
# Calculate event windows ----
#------------------------------------------------------------------------------*

# Register commonly used functions
days <- lubridate::days


# Reference windows for events
event_references <- tribble(
  ~event_name, ~start_days, ~end_days,
  "baseline",            0,      21*7,
  "p1",               24*7,      27*7,
  "p2",               32*7,      35*7,
  "parto",            -6*7,       999,
  "b1",            3*30 -3,   3*30 +3,
  "b2",            6*30 -4,   6*30 +4,
  "b3",            9*30 -4,   9*30 +4,
  "b4",           12*30 -4,  12*30 +4
)


# Set parameters for screening
monthly_screening <- 57
start_date <- as.Date("2018-07-19")
screening_duration <- ceiling(800 / monthly_screening / 12 * 365)


# Generate report considering next Monday
next_monday <- Sys.Date() %>%
  lubridate::ceiling_date(unit = "week", week_start = 1)




#------------------------------------------------------------------------------*
# Define all possible events ----
#------------------------------------------------------------------------------*

all_events <- inscritas %>%
  # Windows are mostly based on the expected date of delivery
  select(
    visit, screening_id, id,
    fpp, fpp_method, fur,
    hh_arm = s6_arm,
    # Add the community name
    community,
    # Leave these dates as reference for the field team
    screening_date, s2_date, enrollment_date, randomization_date = s6_date,
    m11 = m11_date, a21 = a21_date, h41 = h41_date, c30 = matches("c30_date"),
    pw = e3_date, owa = e3_date_o
  ) %>%
  filter(
    visit %in% c(
      "baseline", "p1", "p2", "b1", "b2", "b3", "b4", "salida"
    )
  ) %>%
  gather(
    key = variable, value = value,
    hh_arm, randomization_date, m11, a21, h41, pw, owa,
    na.rm = TRUE, factor_key = TRUE
  ) %>%
  unite(col = variable, visit, variable) %>%
  spread(variable, value) %>%
  rename(
    hh_arm = baseline_hh_arm, randomization_date = baseline_randomization_date
  ) %>%
  gather(variable, value, matches("(baseline|p1|p2|parto|b1|b2|b3|b4)_")) %>%
  extract(
    variable, into = c("event_name", "variable"),
    regex = "([^_]+)_(.+)"
  ) %>%
  spread(variable, value) %>%
  mutate(
    report_date = next_monday,
    # Tag given study arm
    hh_arm = recode_factor(
      hh_arm,
      `1` = "intervencion",
      `0` = "control",
      .missing = "no-aleatorizadas" 
    )
  ) %>%
  full_join(event_references) %>%
  filter(
    # Some participant still in the study
    (grepl("^33", id) & is.na(salida_pw)) | (grepl("^35", id) & (is.na(salida_pw) | is.na(salida_owa))),
    # The event has not happened
    (is.na(m11) & is.na(salida_pw)) |
      is.na(h41) |
      (grepl("^35", id) & is.na(a21) & is.na(salida_owa))
  ) %>%
    )
  ) %>%
  # Add event reference windows
  left_join(mutate(event_references, placeholder = 1)) %>%
  # Determine event relevance
  mutate(
    conception_date = fpp - days(280),
    # Calculate reference days
    current_days_pregnancy = as.integer(
      report_date - conception_date,
      units = "days"
    ),
    # Days since birth
    days_since_birth = as.integer(
      report_date - fpp,
      units = "days"
    ),
    event_week_start = report_date,
    event_week_end = event_week_start + days(5),
    event_relevant = case_when(
      # Baseline should start at the most 14 days after S2
      # And can be considered if s2 has already been filled
      event_name == "baseline" & (
        (s2_date < event_week_start) #&
        # (s2_date + days(1) >= event_week_start)
      ) ~ TRUE,
      # P1 should happen during gestational age between 24 and <27 weeks 
      event_name == "p1" & (
        current_days_pregnancy >= start_days
      ) ~ TRUE,
      # P2 should happen during gestational age between 32 and <35 weeks 
      event_name == "p2" & (
        current_days_pregnancy >= start_days
      ) ~ TRUE,
      # Birth expected starting 2 weeks before the expected delivery date
      event_name == "birth" & days_since_birth >= start_days ~ TRUE,
      event_name == "b1" & days_since_birth >= start_days ~ TRUE,
      event_name == "b2" & days_since_birth >= start_days ~ TRUE,
      event_name == "b3" & days_since_birth >= start_days ~ TRUE,
      event_name == "b4" & days_since_birth >= start_days ~ TRUE,
      TRUE ~ FALSE
    ),
    # ordered events
    event_name = factor(
      event_name,
      levels = c("baseline", "p1", "p2", "birth", "b1", "b2", "b3", "b4")
    ),
    fpp_method = recode(
      fpp_method,
      `1` = "FUR",
      `2` = "Ultrasonido"
    )
  ) %>%
  select(
    report_date,
    community,
    hh_arm,
    screening_id, id,
    screening_date, enrollment_date, randomization_date,
    event_name, event_relevant
    fpp, fpp_method, fur,
    current_days_pregnancy,
  )


# End of script
