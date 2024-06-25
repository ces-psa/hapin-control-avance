gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id"=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>%mutate(type = if_else(
  condition = grepl("^35[0-9]{3}", id),
  true = "oaw",
  false = "pw"
)
) %>% group_by(type) %>% summarize(contador=n())
