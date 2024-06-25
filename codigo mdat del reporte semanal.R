
## MDAT {.tabset}
### B6 (36 MESES)
#### REPORTE
```{r mdat}
#datos de c85 export de redcap
mdat<-gt_hapin_II_data %>%  filter(!c85_date %in% c("")) %>% filter(!is.na(c85_date)) %>% 
  select(id, c85_date, c85_complete, c85_section___1, c85_section___2, c85_section___3, c85_section___4) %>% 
  mutate(weekday = weekdays(as.Date(c85_date))) 


#agregar 
mdat$week <- strftime(mdat$c85_date, format = "%V")
mdat$month <- strftime(mdat$c85_date, format = "%m")
mdat$year <- strftime(mdat$c85_date, format = "%Y")

mdat <- mdat %>% 
  group_by(month) %>% 
  mutate(c85_complete = factor(c85_complete, levels = c(1,0), labels=c("Completo", "No Completo")),
         c85_section___1 = factor(c85_section___1, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___2 = factor(c85_section___2, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___3 = factor(c85_section___3, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___4 = factor(c85_section___4, levels = c(1,0), labels=c("No completo", "Completo"))) %>% 
  mutate(month=as.integer(month)) %>% 
  mutate(
    Mes= factor(month, levels = c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12), labels=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
  ) %>% 
  select(id,  year, month, Mes, week, weekday, everything())
#agregar realizados
mdat <- mdat %>% 
  mutate(Realizados=as.numeric("1"))

#total mdat abordados
total <- mdat %>% 
  group_by(year, month, Mes) %>% 
  arrange(month) %>% 
  count() %>% 
  ungroup() %>% 
  select(year, Mes, n) %>% 
  rename(Total=n)


#completos y no completos
total <- mdat %>% 
  select(year, month, Mes, c85_complete) %>% 
  group_by(year, Mes, c85_complete) %>% 
  count() %>% 
  left_join(total, by = c("year", "Mes")) %>% 
  pivot_wider(
    names_from = c85_complete,
    values_from = n) %>% 
  select("Año"=year, Mes, Completo, "No Completo", Total) 

totales <- total %>% 
  ungroup() %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

total %>% 
  bind_rows(totales) %>%
  mutate(Mes = if_else(is.na(Mes), "Total", as.character(Mes))) %>% 
  mutate(Año = if_else(is.na(Año), " ", as.character(Año))) %>% 
  kable(caption="<center>Avance Mensual <br>
        Evaluaciones MDAT</center>", align="l") %>% 
  kable_classic(full_width=F, html_font="Cambria")


#incompletos
#mdat no completos
nc_mdat <- mdat %>% 
  group_by(year,month, Mes) %>% 
  select(year, month, Mes, c85_complete) %>% 
  filter(c85_complete %in% c("No Completo")) %>% 
  arrange(year,month) %>% 
  mutate(n=1) %>% 
  select(year,month, Mes, n) %>% 
  count() %>% 
  ungroup() %>% 
  select(year,Mes, n) %>% 
  rename(MDAT_no_completos=n)

#Motor grueso incompletos
nc_mg <- mdat %>% 
  group_by(year,month, Mes) %>% 
  select(year,month, Mes, c85_section___1) %>% 
  rename(motor_grueso=c85_section___1) %>% 
  filter(motor_grueso %in% c("No completo")) %>% 
  arrange(year,month) %>% 
  mutate(n=1) %>% 
  select(year,month, Mes, n) %>% 
  count() %>% 
  ungroup() %>% 
  select(year, Mes, n) %>% 
  rename(incompleto_motor_grueso=n)

#motor fino incompletos
nc_mf <- mdat %>% 
  group_by(year, month, Mes) %>% 
  select(year, month, Mes, c85_section___2) %>% 
  rename(motor_fino=c85_section___2) %>% 
  filter(motor_fino %in% c("No completo")) %>% 
  arrange(year,month) %>% 
  mutate(n=1) %>% 
  select(year,month, Mes, n) %>% 
  count() %>% 
  ungroup() %>% 
  select(year,Mes, n) %>% 
  rename(incompleto_motor_fino=n)

#lenguaje incompletos
nc_l <- mdat %>% 
  group_by(year,month, Mes) %>% 
  select(year,month, Mes, c85_section___3) %>% 
  rename(lenguaje=c85_section___3) %>% 
  filter(lenguaje %in% c("No completo")) %>% 
  arrange(year,month) %>% 
  mutate(n=1) %>% 
  select(year, month, Mes, n) %>% 
  count() %>% 
  ungroup() %>% 
  select(year, Mes, n) %>% 
  rename(incompleto_lenguaje=n)

#social incompletos
nc_s <- mdat %>% 
  group_by(year,month, Mes) %>% 
  select(year, month, Mes, c85_section___4) %>% 
  rename(social=c85_section___4) %>% 
  filter(social %in% c("No completo")) %>% 
  arrange(year, month) %>% 
  mutate(n=1) %>% 
  select(year, month, Mes, n) %>% 
  count() %>% 
  ungroup() %>% 
  select(year, Mes, n) %>% 
  rename(incompleto_social=n)

#integrar los no completos
nc_mdat <- nc_mdat %>% 
  left_join(nc_mg) %>% 
  left_join(nc_mf) %>% 
  left_join(nc_l) %>% 
  left_join(nc_s) 

#quitar dataset no utilizados
remove(nc_mg, nc_mf, nc_l, nc_s)

nc_mdat %>% 
  select("Año"=year,Mes, "No completados"=MDAT_no_completos, "Motor grueso"=incompleto_motor_grueso, "Motor fino"=incompleto_motor_fino, "Lenguaje"=incompleto_lenguaje, "Social"=incompleto_social) %>% 
  kable(caption="<center>Evaluaciones MDAT <br>
        no completadas</center>",align="l") %>% 
  kable_classic(full_width=F, html_font="Cambria")

```


```{r mdata_mes}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
#datos de c85 export de redcap
n_mdat<-gt_hapin_II_data %>%  filter(!c85_date %in% c("")) %>% filter(!is.na(c85_date)) %>% 
  select(id, c85_date, c85_by, c85_complete, c85_section___1, c85_section___2, c85_section___3, c85_section___4) %>% 
  mutate(weekday = weekdays(as.Date(c85_date))) %>% mutate(
    year=lubridate::year(c85_date),
    month=lubridate::month(c85_date),
    Mes=format(as.Date(c85_date), "%B")
  )

#agrupar por iniciales
comp <- n_mdat %>% arrange(c85_date) %>% 
  select(year, month, Mes, c85_by, c85_complete) %>% 
  group_by(year, month, Mes, c85_by, c85_complete) %>% 
  count(c85_complete) %>% 
  ungroup()

total <- n_mdat %>% 
  group_by(year, month, Mes) %>% 
  count() %>% 
  select(year, Mes, denominador=n)

comp_mdat <- comp %>% 
  left_join(total) %>%
  mutate(porcentaje = percent(as.numeric(n)/as.numeric(denominador), digits = 1))

remove(comp, total)

motor_grueso<-n_mdat %>% filter(c85_complete=="0") %>% group_by(year, month, Mes, c85_by, Motor_grueso=c85_section___1) %>% 
  count() %>% filter(Motor_grueso=="1") %>% ungroup() %>% select(year, month, Mes, iniciales=c85_by, incomp_motor_grueso=n) 

motor_grueso<-motor_grueso %>% group_by(Mes) %>% mutate(denominador_mg=sum(incomp_motor_grueso))

motor_fino<-n_mdat %>% filter(c85_complete=="0") %>% group_by(year, month, Mes, c85_by, Motor_fino=c85_section___2) %>% 
  count() %>% filter(Motor_fino=="1") %>% ungroup() %>% select(year, month, Mes, iniciales=c85_by, incomp_motor_fino=n) 

motor_fino <- motor_fino %>% group_by(Mes) %>% mutate(denominador_mf=sum(incomp_motor_fino))

lenguaje<-n_mdat %>% filter(c85_complete=="0") %>% group_by(year, month, Mes, c85_by, lenguaje=c85_section___3) %>% 
  count() %>% filter(lenguaje=="1") %>% ungroup() %>% select(year, month, Mes, iniciales=c85_by, incomp_lenguaje=n) 

lenguaje<-lenguaje %>% group_by(Mes) %>% mutate(denominador_leng=sum(incomp_lenguaje))

social<-n_mdat %>% filter(c85_complete=="0") %>% group_by(year, month, Mes, c85_by, social=c85_section___4) %>% 
  count() %>% filter(social=="1") %>% ungroup() %>% select(year, month, Mes, iniciales=c85_by, incomp_social=n) 

social<- social %>% group_by(Mes) %>%  mutate(denominador_soc=sum(incomp_social))

incomp_secciones<-n_mdat %>% filter(c85_complete=="0") %>% group_by(year, month, Mes, c85_by) %>% count() %>% select(year, month, Mes, iniciales=c85_by)  %>% left_join(
  motor_grueso
)%>% left_join(
  motor_fino
) %>% left_join(
  lenguaje
) %>% left_join(
  social
) %>% ungroup() %>% mutate(
  p_motor_grueso= percent(as.numeric(incomp_motor_grueso) / as.numeric(denominador_mg), digits = 1),
  p_motor_fino=percent(as.numeric(incomp_motor_fino)/ as.numeric(denominador_mf), digits = 1),
  p_lenguaje=percent(as.numeric(incomp_lenguaje)/ as.numeric(denominador_leng), digits = 1),
  p_social=percent(as.numeric(incomp_social)/ as.numeric(denominador_soc), digits = 1),
)

dt_secciones_incompletas<- incomp_secciones %>%  transmute(
  year=as.character(year), Mes, iniciales, 
  motor_grueso=paste0(incomp_motor_grueso,"(", denominador_mg,", ",p_motor_grueso,")"),
  motor_fino=paste0(incomp_motor_fino,"(", denominador_mf,", ",p_motor_fino,")"),
  lenguaje=paste0(incomp_lenguaje,"(", denominador_leng,", ",p_lenguaje,")"),
  social=paste0(incomp_social,"(", denominador_soc,", ",p_social,")"),
) 

dt_secciones_incompletas<-dt_secciones_incompletas %>% mutate(
  motor_grueso= case_when(
    motor_grueso=="NA(NA, NA)" ~ "0(0, 0)",
    motor_grueso!="NA(NA, NA)" ~ motor_grueso
  ),
  motor_fino= case_when(
    motor_fino=="NA(NA, NA)" ~ "0(0, 0)",
    motor_fino!="NA(NA, NA)" ~ motor_fino
  ),
  lenguaje= case_when(
    lenguaje=="NA(NA, NA)" ~ "0(0, 0)",
    lenguaje!="NA(NA, NA)" ~ lenguaje
  ),
  social= case_when(
    social=="NA(NA, NA)" ~ "0(0, 0)",
    social!="NA(NA, NA)" ~ social
  )
)

DT::datatable(dt_secciones_incompletas, class = 'cell-border stripe', rownames = F, filter = 'top')
# %>% kable(caption = "<center>SECCIONES INCOMPLETAS <br>
#                  POR INICIALES / MES </center>", 
#        align = "l") %>% 
#    kable_classic(full_width=F, html_font = "Cambria")

```


~~~~
  *MDAT al "`r as.Date(f_week2) %>% format(.,"%A %d %B  %Y")` "*
  ~~~~
  
  #### GRAFICAS
  ```{r mdatgraficas}

#datos de c85 export de redcap
mdat<-gt_hapin_II_data %>%  filter(!c85_date %in% c("")) %>% filter(!is.na(c85_date)) %>% 
  select(id, c85_date, c85_complete, c85_section___1, c85_section___2, c85_section___3, c85_section___4) %>% 
  mutate(weekday = weekdays(as.Date(c85_date))) 


#agregar 
mdat$week <- strftime(mdat$c85_date, format = "%V")
mdat$month <- strftime(mdat$c85_date, format = "%m")
mdat$year <- strftime(mdat$c85_date, format = "%Y")

mdat <- mdat %>% 
  group_by(month) %>% 
  mutate(c85_complete = factor(c85_complete, levels = c(1,0), labels=c("Completo", "No Completo")),
         c85_section___1 = factor(c85_section___1, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___2 = factor(c85_section___2, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___3 = factor(c85_section___3, levels = c(1,0), labels=c("No completo", "Completo")),
         c85_section___4 = factor(c85_section___4, levels = c(1,0), labels=c("No completo", "Completo"))) %>% 
  mutate(month=as.integer(month)) %>% 
  mutate(
    Mes= factor(month, levels = c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12), labels=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
  ) %>% 
  select(id,  year, month, Mes, week, weekday, everything())
#agregar realizados
mdat <- mdat %>% 
  mutate(Realizados=as.numeric("1"))

mdat %>% 
  select(Mes, month, Realizados, c85_complete) %>% 
  group_by(month, Mes, c85_complete) %>% 
  count() %>% 
  select(month, Mes, Realizados=n, Estado=c85_complete) %>% 
  ggplot(aes(x=Mes, y=Realizados, fill=Estado)) +
  geom_col(position="dodge") + theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Mes",
       y="Realizados",
       title ="Avance mensual MDAT 36m HAPIN 2022") +
  theme(plot.title = element_text(hjust = 0.5)) 

mdat %>% 
  ungroup() %>% 
  select(week, Realizados, c85_complete) %>% 
  group_by(week, c85_complete) %>% 
  count() %>% 
  select(Week=week, Estado=c85_complete, Realizados=n) %>% 
  ggplot(aes(x=Week, y=Realizados, fill=Estado)) +
  geom_col(position="dodge") + theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Week",
       y="Realizados",
       title ="Avance semanal MDAT 36m HAPIN 2022") +
  theme(plot.title = element_text(hjust = 0.5)) 

#grafica por iniciales  
comp_mdata_graficas<-comp_mdat %>% 
  filter(c85_complete %in% ("0")) %>% 
  filter(year %in% "2022") 

comp_mdata_graficas<-comp_mdata_graficas %>% transmute(month, porcentaje=as.numeric(porcentaje)*100, iniciales=c85_by, freq=n)

comp_mdata_graficas %>% ggplot(
  aes(x=month, y=porcentaje, fill=iniciales)
) + geom_bar(stat = "identity", position = "stack") + theme_minimal() +
  geom_text( stat="identity", position = "stack",  size=2.5,  aes(
    label=paste0(freq,"(",round(porcentaje , 1),"%)"))
  ) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name="Mes", breaks = c(1:12)) +
  labs(x="Mes",
       y="% Incompleto",
       title ="Incompletitud MDAT 36m HAPIN 2022") 



# 
# comp_mdat %>% 
#   filter(c85_complete %in% ("0")) %>% 
#   filter(year %in% "2022") %>% ggplot(aes(x=month, y=porcentaje, fill=c85_by)) +
#     geom_bar(stat="identity", position = "stack") + theme_minimal() +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
# panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_continuous(name="Mes", breaks = c(1:12)) +
#   labs(x="Mes",
#        y="% Incompleto",
#        title ="Incompletitud MDAT 36m HAPIN 2022") 
#   




```


~~~~
  *MDAT al "`r as.Date(f_week2) %>% format(.,"%A %d %B  %Y")` "*
  ~~~~
  
  ### B7 (48 MESES)
  
  
  
  
