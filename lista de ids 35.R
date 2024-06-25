# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

  gt_emory_data %>% select(id, s4_date, house_id=s4_main_id) %>% filter(!is.na(house_id)) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", house_id),
        true = "oaw",
        false = "pw"
      )
      ) %>% filter(type=="oaw") %>% write_csv("output/listade35.csv")
