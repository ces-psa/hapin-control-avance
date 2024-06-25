# load use packages
library(package = "tidyverse")
library(package = "crosstalk")
# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
s0_z10<-read_csv("data//exports//exportz10.csv")
c_30<-read_csv("data//exports//c30.csv")

gt_emory_data %>%
  select(id, visit, matches("^[^_]+_(date|by)$")) %>%
  mutate_all(as.character) %>%
  gather(crf, value, -id, -visit, na.rm = TRUE) %>%
  separate(crf, into = c("crf", "variable")) %>%
  spread(variable, value) %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    date = paste(year, month),
    by = by %>%
      tolower() %>%
      iconv(to="ASCII//TRANSLIT")%>%
      # hacer limpieza de iniciales
      recode(
        "wsc6" = "wsc",
        "wss" = "wsc",
        "lna" = "lma",
        "alb" = "abl",
        "rmi"= "rma",
        "eel"= "elm",
        "wdc"="wsc",
        "lba"="lma",
        "lfn"="lfb",
        "wnds"="wns",

        "lms"="lma"
      )
  ) %>%
  # quitar despues de limpieza
  #count(by, sort = TRUE) %>% 
    count(visit, crf, date, by) %>%
      spread(key = date, value = n) %>%write.csv("frecuencias_iniciales")
      print(n = Inf)


