library(tidyverse)
library(dplyr)
library(lubridate)
library(xlsx)
library(ggplot2)

data<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/ecm_b6.rds")
#data_lab <- readRDS(file = "C:/GitHub_repo/qc_exposure_hapin/rds/b6_mass_deposition.rds")
data_lab_bc <- readxl::read_xlsx("C:/Users/aramirez/Desktop/Sootscan/3 BC Calculations/HAPIN BC results/BC_HAPIN_20230831.xlsx")

prepesaje_15<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/hapin_prepesaje_15.rds")
pospesaje_15<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/hapin_pospesaje_15.rds")

#promedio prepesaje
pre_peso<-prepesaje_15 %>% filter(is.na(dup_ave)) %>% filter(is.na(dup_prompt)) %>% mutate(
  id_filtro=toupper(filter_id_exactly_as_on_label),
  flag_blank_lab=substr(id_filtro,1,2)
) %>% filter(flag_blank_lab=="3V")%>% select(filter_id, promedio_pre=mean_weight_mg)

#promedio pospesaje
pos_peso<-pospesaje_15 %>% filter(is.na(dup_ave)) %>% filter(is.na(dup_prompt)) %>% mutate(
  id_filtro=toupper(filter_id_exactly_as_on_label),
  flag_blank_lab=substr(id_filtro,1,2)
) %>% filter(flag_blank_lab=="3V")%>% select(filter_id, promedio_pos=mean_weight_mg)

data_lab<-pre_peso %>% left_join(
  pos_peso
) %>% mutate(
  mass_deposition=as.numeric(promedio_pos) - as.numeric(promedio_pre)
) 

data_selected <- data %>% mutate(
  ID_Hogar = str_extract(ORIGEN, "\\d{5}")
) %>% select(-ORIGEN) %>% 
# %>% filter(ID_Hogar %in% c("33050",
#                                                  "33051", 
#                                                  "36628", 
#                                                  "33553", 
#                                                  "33628",
#                                                  "33090", 
#                                                  "35040",
#                                                  "33585",
#                                                  "35112",
#                                                  "36113",
#                                                  "33011",
#                                                  "33289")) %>% 
  select(ID_Hogar,Date, Time, 
 `RH-Corrected Nephelometer`,
 `header_Nephelometer-Offset`,
 RH,
 Flow,
 Temp,
 `X-axis`,
 `Z-axis`,
 `header_Filter ID`, 
 `header_Device Serial`,
 ShutDownReason ,
 `header_Nephelometer-Offset`,
 `header_Nephelometer-Slope`, 
) 

generar_secuencia <- function(data) {
  fecha_inicio <- min(data$Date_complete)
  fecha_fin <- max(data$Date_complete)
  incremento <- 60  # Segundos

  secuencia_fechas <- seq(from = fecha_inicio, to = fecha_fin, by = incremento)
  return(data.frame(Date_complete_union = secuencia_fechas))
}

complete_data <- data_selected %>%
  mutate(
    Date_Format = dmy(Date),
    Time_Format = hms(Time),
    Year = year(Date_Format),
    Month = month(Date_Format), 
    day = day(Date_Format), 
    Hour = hour(Time_Format),
    Minute = minute(Time_Format),
    Segundos = "01",
  ) %>% mutate(
    Date_complete = as.POSIXct(paste(Date, paste(Hour, Minute, "0", sep = ":")), format = "%d/%m/%Y %H:%M:%S")
  ) %>% group_by(`header_Filter ID`) %>%
    do(generar_secuencia(.)) %>% 
    left_join(
      data_selected %>% mutate(
        Date_Format = dmy(Date),
        Time_Format = hms(Time),
        Date_complete_union = as.POSIXct(paste(Date, 
                                               paste(hour(Time_Format), minute(Time_Format), "0", sep = ":")), format = "%d/%m/%Y %H:%M:%S")
      ) %>% 
      arrange(
        Date_Format, Time_Format
    )
  )

data_by_minute <- complete_data %>% mutate(
    Date_Format = dmy(Date),
    Time_Format = hms(Time),
    `RH-Corrected Nephelometer` = as.numeric(`RH-Corrected Nephelometer`),
    Temp = as.numeric(Temp)
  ) %>% mutate(
    Year = year(Date_complete_union),
    Month = month(Date_complete_union), 
    day = day(Date_complete_union), 
    Hour = hour(Date_complete_union),
    Minute = minute(Date_complete_union)
  ) %>% group_by(
    Year, Month, day, Hour, Minute,`header_Filter ID`
  ) %>% mutate(
    PMedidas = sum(!is.na(`RH-Corrected Nephelometer`)),
    Pmean = mean(`RH-Corrected Nephelometer`, na.rm = T),
    Pmax = max(`RH-Corrected Nephelometer`, na.rm = T),
    Pmin = min(`RH-Corrected Nephelometer`, na.rm = T),
    TMedidas = sum(!is.na(Temp)),
    Tmean = mean(Temp, na.rm = T),
    Tmin =  min(Temp, na.rm = T),
    Tmax =  max(Temp, na.rm = T)
  ) %>% ungroup() %>% group_by(
    `header_Filter ID` 
  ) %>% mutate(
    PGFMedidas = sum(!is.na(Pmean)),
    PGFmean = mean(as.numeric(Pmean), na.rm = T),
    PGFmax = max(Pmean, na.rm = T),
    PGFmin = min(Pmean, na.rm = T),
    
    TempMedidas = sum(!is.na(Tmean)),
    Tempmean = mean(as.numeric(Tmean), na.rm = T),
    Tempmax = max(Tmean, na.rm = T),
    Tempmin = min(Tmean, na.rm = T)
  ) %>% ungroup() %>% select(
    ID_Hogar,
    Date_complete_union,
    `header_Filter ID`,
    `RH-Corrected Nephelometer`,
    PMedidas,
    Pmean,
    Pmax,
    Pmin,
    TMedidas,
    Tmean,
    Tmin,
    Tmax,
    PGFMedidas,
    PGFmean,
    PGFmax,
    PGFmin,
    TempMedidas,
    Tempmean,
    Tempmax,
    Tempmin, 
    `header_Nephelometer-Offset`,
    `header_Nephelometer-Slope`
  ) %>% unique()

data_by_filter <- data_by_minute %>% select(
ID_Hogar,
    `header_Filter ID`,
  PGFMedidas,
  PGFmean,
  PGFmax,
  PGFmin,
  TempMedidas,
  Tempmean,
  Tempmax,
  Tempmin,
  `header_Nephelometer-Offset`,
  `header_Nephelometer-Slope`, 
) %>% filter(
  !is.na(`header_Nephelometer-Offset`)
)%>% unique() %>% select(
  "HHID" = ID_Hogar,
  "filter_id" = `header_Filter ID`,
  "PM25 Measures" = PGFMedidas,
  "PM25 Mean" = PGFmean,
  "PM25 Max" = PGFmax,
  "PM25 Min" = PGFmin,
  "Temp Measures" = TempMedidas,
  "Temp Mean" = Tempmean,
  "Temp Max" = Tempmax,
  "Temp Min" = Tempmin,
  `header_Nephelometer-Offset`,
  `header_Nephelometer-Slope`
) %>% left_join(
  data_lab_bc %>% select(
     filter_id ,
    bc_deposition_ug
  )
) %>% left_join(
  data_lab %>% select(
    filter_id ,
    mass_deposition
  )
)
   
data_by_filter %>% writexl::write_xlsx("output/b6_by_filter_completexlsx")

data_selected %>% select(ID_Hogar, `header_Nephelometer-Offset`) %>% distinct() %>% filter(
 as.numeric(`header_Nephelometer-Offset`)>100
  )
