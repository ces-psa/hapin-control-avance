#------------------------------------------------------------------------------*
# Separate data topics ----
#------------------------------------------------------------------------------*

# Mujeres tamizadas elegibles pero no inscritas
elegibles <- gt_emory_data %>%
  mutate(
    # eligibility
    eligible = (
      # is eligible
      s1_pregnant == 1 & s1_age_s == 1 & s1_area_s == 1 & s1_biomass == 1 &
        s1_smoke != 1 & s1_move != 1 &  s1_lpg != 1
    ) & (
      # is not ineligible
      !s2_fetus %in% c(0, 2) & !s2_gest %in% c(0, 2) &
        !s2_onefetus %in% c(0, 2) & !s2_participate %in% c(0, 2) &
        s1_ultra != 0
    ),
    # is enrolled
    enrolled = (!is.na(s4_date) & !s4_consent %in% c(0, 2) & !is.na(s4_main_id)),
    # should be enrolled but is not
    pending = eligible & !enrolled &
      (is.na(s1_date) | is.na(s2_date) | is.na(s4_date))
  ) %>%
  filter(
    # from screening
    redcap_event_name == "elegibilidad_arm_1"
  ) %>%
  select(
    redcap_event_name, visit, id,
    eligible, enrolled, pending,
    matches("^(s1|s2|m17|s3|s4)_")
  )


# Tabla de referencia IDs tamizaje y estudio principal
id_screening_enrolled <- gt_emory_data %>%
  filter(redcap_event_name == "elegibilidad_arm_1") %>%
  select(
    id,
    community = s1_community_name,
    s4_main_id
  )


# Mujeres inscritas en el estudio
inscritas <- gt_emory_data %>%
  # Only keep those that are enrolled
  filter(
    id %in% {
      id_screening_enrolled %>%
        filter(!is.na(s4_main_id)) %>%
        pull(s4_main_id) %>%
        as.character()
    }
  ) %>%
  # Gather variable-value pairs
  mutate_at(
    vars(matches("_date")),
    funs(as.character)
  ) %>%
  # Add dates for started H41s
  left_join(
    gt_exposure_start %>%
      filter(gth4x_crf == "h41") %>%
      select(visit, id = record_id, gth4x_date)
  ) %>%
  mutate(
    h41_date = if_else(
      condition = !is.na(gth4x_date),
      true = as.character(as.POSIXct(gth4x_date)),
      false = as.character(as.POSIXct(h41_date))
    )
  ) %>%
  select(-gth4x_date) %>%
  gather(
    key = variable, value = value,
    -id, -visit,
    factor_key = TRUE
  ) %>%
  filter(!is.na(value)) %>%
  # And tag with the visit name (to have only one row per patient)
  unite(variable, visit, variable, sep = ".") %>%
  # Recover one row per patient
  spread(variable, value) %>%
  # Add screening ids
  left_join(
    select(
      id_screening_enrolled,
      community,
      screening_id = id, id = s4_main_id
    ) %>%
      mutate(id = as.character(id))
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(redcap_event_name == "elegibilidad_arm_1", !is.na(m17_ga)) %>%
      select(
        screening_id = id,
        fpp = m17_ga, fpp_method = m17_ga_method, fur = m17_lmp_date,
        screening_date = s1_date, enrollment_date = s4_date,
        s2_date
      )
  ) %>%
  # Regenerate RedCap structure
  gather(variable, value, matches("[.]")) %>%
  filter(!is.na(value)) %>%
  separate(variable, into = c("visit", "variable"), sep = "[.]") %>%
  mutate(
    visit = factor(visit, levels = levels(gt_emory_data$visit))
  ) %>%
  spread(variable, value) %>%
  select(
    redcap_event_name, visit,
    screening_id, id, fpp, fpp_method, fur, s2_date,
    everything()
  )


tamizadas <- elegibles %>%
  filter(
    ! eligible
  ) %>%
  select(id, redcap_event_name, visit, matches("^(s1|s2|m17|s3|s4)_"))

