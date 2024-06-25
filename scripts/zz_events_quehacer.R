#------------------------------------------------------------------------------*
# List all expected crfs and visits ----
#------------------------------------------------------------------------------*

# Currently filled crfs
source(file = "scripts/completed_crfs.R", encoding = "UTF-8")


# All expected crfs
all_crfs <- crf_id_dates %>%
  filter(
    crf %in% c(
      # screening
      "m17", "s4",
      # aleatorization and intervention
      "s6", "h50",
      # baseline, p1, p2, b1, b2, b3, b4
      "m11", "a21", "h41", "c33",
      # birth
      "c30",
      # monthly
      "c31",
      # study exit
      "e3", "e3o", "e3c"
    )
  ) %>%
  complete(nesting(study_id, screening_id), nesting(visit, crf)) %>%
  filter(
    (crf == "a21" & grepl("^35", study_id)) | crf != "a21"
  ) %>%
  mutate(
    visit = factor(visit, levels = c(levels(visit), "ninguno"))
  )


#------------------------------------------------------------------------------*
# Calculate event windows ----
#------------------------------------------------------------------------------*


# Generate report considering next Monday
next_monday <- Sys.Date() %>%
  lubridate::ceiling_date(unit = "week", week_start = 1)


# Month length
ml <- 365.25/12


# Reference windows for events
event_references <- tribble(
  ~visit,      ~start_days, ~end_days,
  "baseline",            0,      21*7,
  "p1",               24*7,      27*7,
  "p2",               32*7,      35*7,
  "parto",            -6*7,       999,
  "m1",           1*ml - 1,  1*ml + 1,
  "m2",           2*ml - 1,  2*ml + 1,
  "b1",           3*30 - 3,  3*30 + 3,
  "m4",           4*ml - 1,  4*ml + 1,
  "m5",           5*ml - 1,  5*ml + 1,
  "b2",           6*30 - 4,  6*30 + 4,
  "m7",           7*ml - 1,  7*ml + 1,
  "m8",           8*ml - 1,  8*ml + 1,
  "b3",           9*30 - 4,  9*30 + 4,
  "m10",         10*ml - 1, 10*ml + 1,
  "m11",         11*ml - 1, 11*ml + 1,
  "b4",          12*ml - 4, 12*ml + 4,
  "ninguno",             0,       Inf
) %>%
  mutate(
    visit = factor(visit, levels = levels(all_crfs$visit))
  )


# Generate report considering next Monday
next_monday <- Sys.Date() %>%
  lubridate::ceiling_date(unit = "week", week_start = 1)



losses <- gt_emory_data_arm1 %>%
  select(
    redcap_event_name, s4_id = s4_main_id, study_id = id) %>% full_join(gt_emory_data_arm2  %>% select(study_id=id, matches("^e[12]_"))
  ) %>%
  filter(!is.na(e1_date) | !is.na(e2_date)) %>%
  mutate(
    # get study id for Es recorded in the screening arm
    study_id = if_else(
      condition = grepl("elegibilidad", redcap_event_name),
      true = as.character(s4_id),
      false = study_id
    ),
    # Abortion
    loss_type = case_when(
      e1_participant == 1 & e1_title == 2 ~ "abortion",
      # e2 pregnant woman and fetal loss
      e2_participant == 1 & e2_title == 2 ~ "fetal_loss",
      # e2 child and death
      e2_participant == 3 & e2_title == 7 ~ "child_death",
      TRUE ~ "not-child-related"
    ),
    loss_date = case_when(
      loss_type == "abortion" ~ e1_start_date,
      loss_type == "fetal_loss" ~ e2_start_date,
      loss_type == "child_death" ~ e2_start_date
    )
  ) %>%
  count(study_id, loss_type, loss_date) %>%
  filter(loss_type != "not-child-related")



births <- gt_emory_data_arm2 %>%
  filter(!is.na(c30_date)) %>%
  select(
    study_id = id, fecha_parto = c30_date
  )



# All possible visits
all_visits <- all_crfs %>%
  # Keep main study visits
  filter(
    grepl("base|p[12]|m[124578]|b[1234]|parto", visit)
  ) %>%
  # tag with pregnancy information
  left_join(
    gt_emory_data_arm1 %>%
      filter(!is.na(s4_date) | !is.na(m17_date)) %>%
      select(
        study_id = s4_main_id, screening_id = id,
        community = s1_community_name,
        fpp = m17_ga, fpp_method = m17_ga_method, fur = m17_lmp_date,
        screening_date = m17_date, enrollment_date = s4_date
      ) %>%
      mutate(study_id = as.character(study_id))
  ) %>%
  # tag with birth information
  left_join(births) %>%
  # tag with hh information
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(s6_date) | !is.na(h50_date) | visit == "baseline") %>%
      select(
        study_id = id,
        randomization_date = s6_date, hh_arm = s6_arm, stove_date = h50_date
      )
  ) %>%
  # Add study exit information
  left_join(
    all_crfs %>%
      filter(!is.na(date)) %>%
      group_by(study_id) %>%
      summarize(
        salida_pw = "e3" %in% crf,
        salida_oaw = "e3o" %in% crf,
        salida_c = "e3c" %in% crf
      ) %>%
      select(study_id, salida_pw, salida_oaw, salida_c)
  ) %>%
  # Add child status information
  left_join(losses)




#------------------------------------------------------------------------------*
# Define all possible events ----
#------------------------------------------------------------------------------*
all_events <- all_visits %>%
  filter(
    # Some participant still in the study
    (grepl("^33", study_id) & !salida_pw) |
      (grepl("^35", study_id) & (!salida_pw | !salida_oaw)) |
      (!is.na(fecha_parto) & !salida_c)
  ) %>%
  filter(
    # CRF-event is relevant
    (crf == "a21" & grepl("^35", study_id)) |
      (crf == "m11") |
      (crf == "c31" & is.na(loss_type)) |
      (crf == "h41") |
      (crf == "c30" & is.na(loss_type))
  ) %>%
  left_join(event_references) %>%
  mutate(
    crf = recode(
      crf,
      m11 = "clinica_pw",
      a21 = "clinica_oaw",
      h41 = "exposicion",
      c31 = "clinica_c"
    )
  ) %>%
  arrange(study_id, visit, crf) %>%
  group_by(study_id, visit) %>%
  mutate(
    pending = paste(crf[is.na(date)], collapse = ", ")
  ) %>%
  ungroup() %>%
  spread(crf, date) %>%
  # Determine event relevance
  mutate(
    report_date = next_monday,
    conception_date = fpp - lubridate::days(280),
    birth_date = if_else(
      condition = is.na(fecha_parto),
      true = fpp,
      false = fecha_parto
    ),
    # Calculate reference days
    current_days_pregnancy = as.integer(
      report_date - conception_date,
      units = "days"
    ),
    # Days since birth
    days_since_birth = as.integer(
      report_date - birth_date,
      units = "days"
    ),
    event_week_start = report_date,
    event_week_end = event_week_start + lubridate::days(5),
    event_relevant = case_when(
      # Baseline should start at the most 14 days after S2
      # And can be considered if s2 has already been filled
      visit == "baseline" & (
        (enrollment_date < event_week_start) &
          # has not happened
          (
            (is.na(clinica_pw) & !salida_pw) |
              is.na(exposicion) |
              (grepl("^35", study_id) & is.na(clinica_oaw) & !salida_oaw)
          )
      ) ~ TRUE,
      # P1 and P2 should happen during certain gestational age range 
      visit %in% c("p1", "p2") & (
        current_days_pregnancy >= start_days &
          # has not happened
          (
            (is.na(clinica_pw) & !salida_pw) |
              is.na(exposicion) |
              (grepl("^35", study_id) & is.na(clinica_oaw) & !salida_oaw)
          )
      ) ~ TRUE,
      # Birth expected starting 2 weeks before the expected delivery date
      visit == "parto" & current_days_pregnancy >= (36*7) &
        (!salida_pw) &
        is.na(loss_type) &
        # has not happened
        (is.na(c30)) ~ TRUE,
      
      # Visits some time after birth
      grepl("b[1-4]|m[0-9]", visit) & (
        days_since_birth >= start_days &
          # has not happened
          (
            # visit to child missing
            (is.na(clinica_c) & is.na(loss_type)) |
              # exposure visit missing
              (visit %in% c("b1", "b2", "b4") & is.na(exposicion)) |
              # visit to adults missing
              (
                grepl("b[1-4]", visit) & (
                  is.na(clinica_pw) |
                    (grepl("^35", study_id) & is.na(clinica_oaw))
                )
              )
          )
      ) ~ TRUE,
      # all ids in que hacer
      visit == "ninguno" ~ TRUE,
      TRUE ~ FALSE
    ),
    valid_days = case_when(
      visit %in% c("baseline", "p1", "p2") ~ end_days - current_days_pregnancy,
      visit == "parto" ~ 280 - current_days_pregnancy,
      grepl("b[1-4]|m[0-9]", visit) ~ end_days - days_since_birth,
      TRUE ~ 0
    ),
    fpp_method = recode(
      fpp_method,
      `1` = "FUR",
      `2` = "Ultrasonido"
    ),
    # Tag given study arm
    hh_arm = recode_factor(
      hh_arm,
      `1` = "intervencion",
      `0` = "control",
      .missing = "no-aleatorizadas" 
    ),
    # tag losses
    loss_type = case_when(
      loss_type == "abortion" ~ "Aborto",
      loss_type == "fetal_loss" ~ "Pérdida fetal",
      loss_type == "child_death" ~ "Muerte infantil",
      is.na(loss_type) ~ "",
      TRUE ~ loss_type
    )
  ) %>%
  group_by(study_id, visit) %>%
  mutate(
    event_name = case_when(
      # missing clinics
      visit %in% c("baseline", "p1", "p2") &
        !is.na(exposicion) & is.na(clinica_pw) & !salida_pw ~
        paste(visit, "clinica embarazada"),
      visit %in% c("baseline", "p1", "p2") &
        !is.na(exposicion) & is.na(clinica_oaw) & !salida_oaw &
        grepl("^35", study_id) ~
        paste(visit, "clinica otra adulta"),
      # missing exposure
      visit %in% c("baseline", "p1", "p2") &
        is.na(exposicion) &
        (!salida_pw | !salida_oaw | !salida_c) ~
        paste(visit, "exposure"),
      # missing randomization
      visit %in% c("baseline") &
        hh_arm == "no-aleatorizadas" &
        (is.na(salida_pw) | is.na(salida_oaw)) ~
        paste("aleatorizacion"),
      # missing stove installation
      visit %in% c("baseline", "p1", "p2") &
        hh_arm == "intervencion" & is.na(stove_date) &
        (is.na(salida_pw) | is.na(salida_oaw)) ~
        paste("instalación"),
      TRUE ~ as.character(visit)
    )
  ) %>%
  ungroup() %>%
  select(
    report_date,
    study_id, visit,
    screening_id, community,
    hh_arm,
    screening_date, enrollment_date, randomization_date,
    fpp, fpp_method, fur,
    current_days_pregnancy,
    event_name, event_relevant,
    valid_days,
    pending, salida_pw, salida_oaw, salida_c,
    everything()
  )


# End of script
