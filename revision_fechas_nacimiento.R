# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")
# Participant information (z10)
source(file = "scripts/0_get_e3.R", encoding = "UTF-8")


# Participant information (z10)
source(file = "scripts/0_get_hapinII.R", encoding = "UTF-8")


#revisar fechas de nacimiento en z10

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% left_join(
  z10_dob %>% select(id=id_estudio, record_id, fecha_nacimiento_bb)  
) %>% mutate(
  corregir_z10=if_else(
    as.Date(c30_dob)!=as.Date(fecha_nacimiento_bb),"Si","No"
  )
) %>% filter(corregir_z10=="Si") %>% writexl::write_xlsx("output/corregir_z10_dob.xlsx")
 
