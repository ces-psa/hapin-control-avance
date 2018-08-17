
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


# Generate report every Friday morning
report_days <- seq.Date(
  from = start_date,
  # Latest birth
  to = max(inscritas$fpp) +
    # plus one year
    12*30 + 4,
  by = "1 day"
)

report_days <- report_days[weekdays(report_days) %in% c("viernes", "friday")]




# Register commonly used functions
days <- lubridate::days

all_events <- inscritas %>%
  select(
    screening_id, id, fpp,
    screening_date, s2_date, enrollment_date, randomization_date = s6_date
  ) %>%
  mutate(
    placeholder = 1
  ) %>%
  full_join(
    data_frame(
      placeholder = 1,
      report_date = report_days
    )
  ) %>%
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
    event_week_start = report_date + days(3),
    event_week_end = event_week_start + days(4),
    event_relevant = case_when(
      # Baseline should start at the most 14 days after S2
      # And can be considered if s2 has already been filled
      event_name == "baseline" & (
        (s2_date < event_week_start) #&
        # (s2_date + days(1) >= event_week_start)
      ) ~ TRUE,
      # P1 should happen during gestational age between 24 and <27 weeks 
      event_name == "p1" & (
        (current_days_pregnancy + 3 >= start_days) &
          (current_days_pregnancy + 3 < end_days)
      ) ~ TRUE,
      # P2 should happen during gestational age between 32 and <35 weeks 
      event_name == "p2" & (
        (current_days_pregnancy + 3 >= start_days) &
          (current_days_pregnancy + 3 < end_days)
      ) ~ TRUE,
      # Birth expected starting 2 weeks before the expected delivery date
      event_name == "birth" & (
        (days_since_birth + 3 >= start_days) &
          (days_since_birth + 3 <= end_days)
      ) ~ TRUE,
      event_name == "b1" & (
        (days_since_birth + 3 >= start_days) &
          (days_since_birth + 3 <= end_days)
      ) ~ TRUE,
      event_name == "b2" & (
        (days_since_birth + 3 >= start_days) &
          (days_since_birth + 3 <= end_days)
      ) ~ TRUE,
      event_name == "b3" & (
        (days_since_birth + 3 >= start_days) &
          (days_since_birth + 3 <= end_days)
      ) ~ TRUE,
      event_name == "b4" & (
        (days_since_birth + 3 >= start_days) &
          (days_since_birth + 3 <= end_days)
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    # ordered events
    event_name = factor(
      event_name,
      levels = c("baseline", "p1", "p2", "birth", "b1", "b2", "b3", "b4")
    )
  ) %>%
  select(
    report_date,
    screening_id, id,
    screening_date, enrollment_date, randomization_date,
    fpp, current_days_pregnancy,
    event_name, event_relevant
  )

