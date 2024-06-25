      
# install.package(ggplot2) # para instalar ggplot2
library(scales)
library(ggplot2)


#saccar los b4 pendientes
dt_b4_pendientes<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
) %>% select(id_tamizaje, id, fecha_nacimiento) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    (!is.na(h41_date) | !is.na(c35_date) | !is.na(m11_date) | !is.na(a24a_date)) & visit=="b4"
  ) %>% select(id)
) %>% 
  mutate(
  meses_12=fecha_nacimiento +lubridate::days(365),
  semana_date=lubridate::floor_date(meses_12, unit = "weeks", week_start = 1),
  grupo="b4_pendiente"
) %>% select(id, semana_date, grupo)

#b5 pendientes

dt_b5_pendientes<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% 
  mutate(
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
) %>% filter(!is.na(c30_dob)) %>% select(id_tamizaje, id, fecha_nacimiento) %>%  mutate(
  ventana=as.Date(fecha_nacimiento) + lubridate::days(350),
  meses_12=as.Date(fecha_nacimiento) + lubridate::days(round(365)),
  meses_24=as.Date(fecha_nacimiento) + lubridate::days(round(730)),
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% left_join(
  gt_emory_data_arm2 %>% filter(
    (!is.na(h41_date) | !is.na(c35_date) | !is.na(m11_date) | !is.na(a24a_date)) & visit=="b4"
  ) %>% transmute(id, b4_realizado="Si" )
) %>% mutate(
  semana_12_meses=lubridate::floor_date(meses_12, unit = "weeks", week_start = 1),
  semana=lubridate::floor_date(meses_24, unit = "weeks", week_start = 1),
  semana_al=lubridate::floor_date(meses_24+7, unit = "weeks", week_start = 7),
  semana_concat=paste0("del: ",semana, " al ",semana_al),
  grupo="b5_pendientes"
) %>% arrange(meses_24) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date)) %>%  select(id) 
) %>%  select(id, semana_date=semana, grupo) %>%  print()

#b5 realizados                             
dt_b5_realizado<-gt_hapin_II_data %>% filter(!is.na(c35_date)) %>%  select(id) %>% left_join(
  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% mutate(
    fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
  ) %>% filter(!is.na(c30_dob)) %>% select(id_tamizaje, id, fecha_nacimiento) %>%  mutate(
    ventana=as.Date(fecha_nacimiento) + lubridate::days(350),
    meses_12=as.Date(fecha_nacimiento) + lubridate::days(round(365)),
    meses_24=as.Date(fecha_nacimiento) + lubridate::days(round(730)),
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
  ) %>% mutate(
    descartar=case_when(
      is.na(e3_date_exit_c) ~ "No",
      as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
      TRUE ~ "No"
    )
  ) %>% filter(descartar=="No") %>% left_join(
    gt_emory_data_arm2 %>% filter(
      (!is.na(h41_date) | !is.na(c35_date) | !is.na(m11_date) | !is.na(a24a_date)) & visit=="b4"
    ) %>% transmute(id, b4_realizado="Si" )
  ) %>% mutate(
    semana_12_meses=lubridate::floor_date(meses_12, unit = "weeks", week_start = 1),
    semana=lubridate::floor_date(meses_24, unit = "weeks", week_start = 1),
    semana_al=lubridate::floor_date(meses_24+7, unit = "weeks", week_start = 7),
    semana_concat=paste0("del: ",semana, " al ",semana_al),
    grupo="b5_realizado"
  ) %>% select(id, semana_date=semana, grupo) %>%  print()
  
)


#b5 rechazados 
gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id,s4_ocon_date, 
                                                        s4_consent_c, s4_consent) %>% mutate(
                                                          consentio_adulta=if_else(
                                                            is.na(s4_ocon_date),"0","1"
                                                          )
                                                        ) %>% left_join(
                                                          listado_bebes 
                                                        ) %>% filter(
                                                          is.na(`12_meses`)
                                                        ) %>% left_join(
                                              gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% 
                                                select(id, c35_date)
                                                        ) %>% select(id, s4_ocon_date, s4_consent,
                                                                     s4_consent_c, c35_date)



dt_b5_rechazado<-gt_hapin_II_data %>% filter(!is.na(s4_date)) %>%  select(id,s4_ocon_date, 
                                                                          s4_consent_c, s4_consent) %>% mutate(
                                                                            consentio_adulta=if_else(
                                                                              is.na(s4_ocon_date),"0","1"
                                                                            )
                                                                          ) %>% mutate(
                                                                            tipo=if_else(
                                                                              grepl("^35", id),
                                                                              "hogar_35",
                                                                              "hogar_33"
                                                                            )
                                                                          ) %>% left_join(
                                                                            consentimientos_sangre_seca %>% 
                                                                              transmute(id,sangre_seca="1")
                                                                          ) %>% 
  group_by(tipo,sangre_seca,s4_consent, s4_consent_c, consentio_adulta) %>% 
  filter(s4_consent=="0" & s4_consent_c=="1")  %>% 
  left_join(
  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% mutate(
    fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
  ) %>% filter(!is.na(c30_dob)) %>% select(id_tamizaje, id, fecha_nacimiento) %>%  mutate(
    ventana=as.Date(fecha_nacimiento) + lubridate::days(350),
    meses_12=as.Date(fecha_nacimiento) + lubridate::days(round(365)),
    meses_24=as.Date(fecha_nacimiento) + lubridate::days(round(730)),
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
  ) %>% mutate(
    descartar=case_when(
      is.na(e3_date_exit_c) ~ "No",
      as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
      TRUE ~ "No"
    )
  ) %>% filter(descartar=="No") %>% left_join(
    gt_emory_data_arm2 %>% filter(
      (!is.na(h41_date) | !is.na(c35_date) | !is.na(m11_date) | !is.na(a24a_date)) & visit=="b4"
    ) %>% transmute(id, b4_realizado="Si" )
  ) %>% mutate(
    semana_12_meses=lubridate::floor_date(meses_12, unit = "weeks", week_start = 1),
    semana=lubridate::floor_date(meses_24, unit = "weeks", week_start = 1),
    semana_al=lubridate::floor_date(meses_24+7, unit = "weeks", week_start = 7),
    semana_concat=paste0("del: ",semana, " al ",semana_al),
    grupo="b5_realizado"
  ) %>% select(id, semana_date=semana, grupo) %>%  print()
  
)

# unificar b4 y b5 pendientes
dt_grafica<-dt_b4_pendientes %>% bind_rows(
  dt_b5_realizado,
  dt_b5_pendientes
  
) %>% select(id, semana_date, Group=grupo)

dt_grafica %>% group_by(semana_date,Group) %>% count() %>% writexl::write_xlsx("output/dt_graficas.xlsx")


##ELABORAR GRAFICA
ggplot(dt_grafica,aes(x=semana_date, group=Group, fill=Group, colour=Group))+
  stat_bin( binwidth=2, alpha=0.5,
            position="stack") + theme_bw()+
  xlab("Weeks")+
  ylab("Visits")+
  scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d %m %Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(breaks=seq(1,30,2))



#lista de bebes candidatos
listado_bebes<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`, Celular_papa_nino=`Celular esposo`, Celular_otro_miembro)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% 
  print()

#ejemplo de histogramas superpuestos
dat <- data.frame(xx = c(runif(100,20,50),
                         runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(dat,aes(x=xx)) + geom_histogram(data=subset(dat,yy == 'a'),fill = "red", alpha = 0.2) + 
      geom_histogram(data=subset(dat,yy == 'b'),fill = "blue", alpha = 0.2) + 
  geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)




#ejemplo
library("ggplot2")
library("scales")

dates <- read.csv("http://pastebin.com/raw.php?i=sDzXKFxJ", sep=",", header=T)
dates$Date <- as.Date(dates$Date)

# convert the Date to its numeric equivalent
# Note that Dates are stored as number of days internally,
# hence it is easy to convert back and forth mentally
dates$num <- as.numeric(dates$Date)

bin <- 60 # used for aggregating the data and aligning the labels

p <- ggplot(dates, aes(num, ..count..))
p <- p + geom_histogram(binwidth = bin, colour="white")

# The numeric data is treated as a date,
# breaks are set to an interval equal to the binwidth,
# and a set of labels is generated and adjusted in order to align with bars
p <- p + scale_x_date(breaks = seq(min(dates$num)-20, # change -20 term to taste
                                   max(dates$num), 
                                   bin),
                      labels = date_format("%Y-%b"),
                      limits = c(as.Date("2009-01-01"), 
                                 as.Date("2011-12-01")))

# from here, format at ease
p <- p + theme_bw() + xlab(NULL) + opts(axis.text.x  = theme_text(angle=45,
                                                                  hjust = 1,
                                                                  vjust = 1))
p


dates$Date <- as.Date(dates$Date)
ggplot(dates, aes(x=Date)) + geom_histogram(binwidth=30, colour="white") +
  scale_x_date(labels = date_format("%Y-%b"),
               breaks = seq(min(dates$Date)-5, max(dates$Date)+5, 30),
               limits = c(as.Date("2008-05-01"), as.Date("2012-04-01"))) +
  ylab("Frequency") + xlab("Year and Month") +
  theme_bw() + opts(axis.text.x = theme_text(angle=90))

hist(.leap.seconds, "years", freq = TRUE)
hist(.leap.seconds,
     seq(ISOdate(1970, 1, 1), ISOdate(2020, 1, 1), "5 years"))
rug(.leap.seconds, lwd=2)

## 100 random dates in a 10-week period
random.dates <- as.Date("2001/1/1") + 70*stats::runif(100)
hist(random.dates, "weeks", format = "%d %b")

#otro ejemplo
dates <- seq(as.Date("2011-10-01"), length.out=60, by="+1 day")

set.seed(1)
dat <- data.frame(
  suburb <- rep(LETTERS[24:26], times=c(100, 200, 300)),
  Date_of_Onset <- c(
    sample(dates-30, 100, replace=TRUE),
    sample(dates,    200, replace=TRUE),
    sample(dates+30, 300, replace=TRUE)
  )
)

library(scales)
library(ggplot2)
ggplot(dat, aes(x=Date_of_Onset, fill=suburb)) + 
  stat_bin(binwidth=1, position="identity") + 
  scale_x_date(breaks=date_breaks(width="1 month"))


# #otro ejemplo usando fechas
# library(scales)
# ggplot(dt_general,aes(x=semana_12_meses, group=semana_12_meses, fill=b4_realizado, colour=b4_realizado))+
#   stat_bin( binwidth=2, alpha=0.2,
#            position="stack") + theme_bw()+
#   xlab("Weeks")+
#   ylab("Visits")+
#   scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d %m %Y")) +
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   
# 
#   
# dt_general %>% filter(is.na(b4_realizado))

