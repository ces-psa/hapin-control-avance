#------------------------------------------------------------------------------*
# Check which crfs have already been filled
#------------------------------------------------------------------------------*


# Use date variable as a proxy for filling a CRF ----
crf_id_dates <- gt_emory_data %>%
  filter(visit == "tamizaje") %>%
  select(
    study_id = s4_main_id, screening_id = id, visit, matches("^[^_]+_date$")
  ) %>%
  mutate_all(as.character) %>%
  gather(
    key = crf, value = date, matches("date"),
    na.rm = TRUE
  ) %>%
  bind_rows(
    {
      screening_data <- .
      
      gt_emory_data %>%
        filter(visit != "tamizaje") %>%
        select(
          study_id = id, visit, matches("^[^_]+_date(_[oc])?$")
        ) %>%
        mutate_all(as.character) %>%
        gather(
          key = crf, value = date, matches("date"),
          na.rm = TRUE
        ) %>%
        left_join(
          screening_data %>%
            select(screening_id, study_id) %>%
            filter(!duplicated(.))
        )
    }
  ) %>%
  mutate(
    crf = crf %>%
      gsub(pattern = "_date_?", replacement = "", .) %>%
      factor(levels = unique(.)),
    visit = factor(visit, levels = unique(visit))
  ) %>%
  arrange(study_id, visit, crf)




crf_filled <- function(crf, ...){
  condition <- quos(...)
  
  found <- crf_id_dates %>%
    filter(!!!condition)
  
  return(crf %in% found$crf)
}
