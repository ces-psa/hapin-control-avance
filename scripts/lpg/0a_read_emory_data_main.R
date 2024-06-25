#------------------------------------------------------------------------------*
# Read in data from Emory RedCap main trial proyect
#------------------------------------------------------------------------------*


# load used package
library(package = "tidyverse")


#------------------------------------------------------------------------------*
# Get data from Emory RedCap exports ----
#------------------------------------------------------------------------------*

gt_emory_file <- list.files(
  "data/exports", pattern = "Arm[123]", full.names = TRUE
) %>%
  tibble(file = .) %>%
  extract(
    file, into = c("arm", "export_time"),
    regex = ".+Arm([123])_DATA_([-0-9_]+).csv",
    remove = FALSE
  ) %>%
  mutate(export_time = lubridate::ymd_hm(export_time)) %>%
  group_by(arm) %>%
  slice(which.max(export_time)) %>%
  ungroup() %>%
  print()




gt_emory_list <- gt_emory_file %>%
  # get file named after its arm
  {set_names(.$file, paste0("arm", .$arm))} %>%
  # readr::read_csv es conveniente y muy bueno para adivinar tipos, pero lento
  # map(read_csv, col_types = cols(.default = col_character())) %>%
  # fread es mucho más rápido, y útil en este caso que queremos todo como chars
  map(
    data.table::fread, sep = ",", header = TRUE, colClasses = "character",
    na.strings = c("", "NA")
  ) %>%
  map(
    ~ {
      arm_data <- .x %>%
        filter(!id %in% c("99999")) %>%
        mutate(
          # Default change for all "monthly" visits
          visit = gsub(
            pattern = "m([0-9]+).+.",
            replacement = "mensual\\1",
            redcap_event_name
          ),
          # Assign routine names to the event-visit combinations
          visit = recode(
            redcap_event_name,
            elegibilidad_arm_1 = "tamizaje",
            linea_de_base_arm_2 = "baseline",
            blp1_arm_1 = "blp1",
            p1_arm_2 = "p1",
            p1p2_arm_1 = "p1p2",
            p2_arm_2 = "p2",
            birth_arm_2 = "parto",
            p2b1_arm_1 = "p2b1",
            mes1_arm_2 = "m1",
            mes_2_arm_2 = "m2",
            b1_arm_2 = "b1",
            b1b2_arm_1 = "b1b2",
            mes_4_arm_2 = "m4",
            mes_5_arm_2 = "m5",
            b2_arm_2 = "b2",
            b2b3_arm_1 = "b2b3",
            mes_7_arm_2 = "m7",
            mes_8_arm_2 = "m8",
            b3_arm_2 = "b3",
            b3b4_arm_1 = "b3b4",
            mes_10_arm_2 = "m10",
            mes_11_arm_2 = "m11",
            b4_arm_2 = "b4",
            salida_del_estudio_arm_2 = "salida",
            segun_sea_necesari_arm_2 = "libres1",
            segun_sea_necesari_arm_2b = "libres2",
            segun_sea_necesari_arm_2c = "libres3",
            segun_sea_necesari_arm_2d = "libres4",
            segun_sea_necesari_arm_2e = "libres5",
            .default = visit
          ) %>%
            factor(
              levels = c(
                "tamizaje", "baseline", "p1", "p2",
                "parto",
                "m1", "m2", "b1", "m4", "m5", "b2",
                "m7", "m8", "b3", "m10", "m11", "b4",
                "salida", 
                "libres1", "libres2", "libres3", "libres4", "libres5",
                "mensual1", "mensual2", "mensual3", "mensual4", "mensual5",
                "mensual6",  "mensual7", "mensual8", "mensual9", "mensual10",
                "mensual11", "mensual12", "mensual13", "mensual14", "mensual15",
                "mensual16", "mensual17", "mensual18"
              )
            )
        ) %>%
        select(redcap_event_name, visit, id, everything()) %>%
        # fix types
        mutate_at(
          vars(matches("date"), matches("dob"), matches("m17_ga$")),
          list(as.Date)
        ) %>%
        mutate_at(
          vars(matches("m17_hr_bpm")),
          list(~ as.numeric(.))
        ) %>%
        as_tibble()
      
      arm_data %>%
        count(redcap_event_name) %>%
        slice(1) %>%
        knitr::kable()
      
      if(
        "s4_main_id" %in% names(arm_data) &
        any(grepl("elegibilidad", arm_data$redcap_event_name))
      ){
          arm_data <- arm_data %>%
            mutate(
              screening_id = id,
              id = s4_main_id
            ) %>%
            select(redcap_event_name, visit, screening_id, id, everything())
          
          # create reference for screening ids
          arm_data %>%
            filter(!is.na(id)) %>%
            count(screening_id, id) %>%
            select(-n) %>%
            assign("sc_ids", value = ., envir = .GlobalEnv)
      } else {
        arm_data <- arm_data %>%
          left_join(sc_ids) %>%
          select(redcap_event_name, visit, screening_id, id, everything())
      }
      
      arm_data
    }
  )



# separado en varios para optimizar data.frame
walk2(names(gt_emory_list), gt_emory_list, assign, envir = .GlobalEnv)




# End of script
