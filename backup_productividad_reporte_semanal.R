
## PRODUCTIVIDAD {.tabset}
### MEDIA PRODUCTIVIDAD

```{r productividad, echo=FALSE}

#para equipo de reclutamiento tomar M10
rec_semana<-gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(visit=="b6") %>% select(
  m10_date, m10_by
) %>% mutate(
  semana=lubridate::week(m10_date),
  mes=lubridate::month(m10_date),
  Mes=format(m10_date, "%B")
) %>% arrange(semana) %>% group_by(semana) %>% count() %>% mutate(media=round(n/5)) %>% select(-n) %>% 
  mutate(equipo="Reclutamiento")

rec_mes<-gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(visit=="b6") %>% select(
  m10_date, m10_by
) %>% mutate(
  semana=lubridate::week(m10_date),
  mes=lubridate::month(m10_date),
  Mes=format(m10_date, "%B")
) %>% arrange(semana) %>% group_by(mes,Mes) %>% count() %>% mutate(media=round(n/20)) %>% select(-n) %>% 
  mutate(equipo="Reclutamiento")

#para equipo de reclutamiento tomar H41
exp_semana<-gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(
  h41_date_v2, h41_by_v2
) %>% mutate(
  semana=lubridate::week(h41_date_v2),
  mes=lubridate::month(h41_date_v2),
  Mes=format(h41_date_v2, "%B")
) %>% arrange(semana) %>% group_by(semana) %>% count() %>% mutate(media=round(n/4)) %>% select(-n) %>% 
  mutate(equipo="Exposicion")

exp_mes<-gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(
  h41_date_v2, h41_by_v2
) %>% mutate(
  semana=lubridate::week(h41_date_v2),
  mes=lubridate::month(h41_date_v2),
  Mes=format(h41_date_v2, "%B")
) %>% arrange(mes,Mes) %>% group_by(mes,Mes) %>% count() %>% mutate(media=round(n/16)) %>% select(-n) %>% 
  mutate(equipo="Exposicion")

#integramos medias de equipos
g_equipo_semana<- rec_semana %>% bind_rows(
  exp_semana
) %>% ggplot(
  aes(x=semana, y=media, color=equipo)
)+ geom_line(stat="identity", size=1)+ 
  #geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text( size = 8))+
  scale_x_continuous(name="Semana", breaks = c(8:52)) + scale_y_continuous(name="Media", breaks = c(0:12))+
  labs(
    title = "MEDIA DE PRODUCTIVIDAD POR SEMANA",
    subtitle = "Hapin 36 meses",
    caption = "Indicador trazador Reclutamiento: M10 con Media sobre 5 dias habiles, Exposicion: H41 con Media sobre 4 dias habiles"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 7)
  ) 




g_equipo_mes<- rec_mes %>% bind_rows(
  exp_mes 
) %>%  ggplot(
  aes(x=mes, y=media, color=equipo)
)+ geom_line(stat="identity", size=1)+ 
  #geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text( size = 8))+
  scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(name="Media", 
                                                                        breaks = c(0:12))+
  labs(
    title = "MEDIA DE PRODUCTIVIDAD POR MES",
    subtitle = "Hapin 36 meses",
    caption = "Indicador trazador Reclutamiento: M10 con Media sobre 20 dias habiles, Exposicion: H41 con Media sobre 16 dias habiles"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 


#grafica por semana
g_equipo_semana

#tabla por semana

rec_semana %>% bind_rows(
  exp_semana
) %>% mutate(
  media_reclutamiento=if_else(equipo=="Reclutamiento",media, NA_real_),
  media_exposicion=if_else(equipo=="Exposicion",media, NA_real_),
) %>% filter(!is.na(media_reclutamiento)) %>% select(semana, media_reclutamiento) %>% 
  left_join(
    rec_semana %>% bind_rows(
      exp_semana
    ) %>% mutate(
      media_reclutamiento=if_else(equipo=="Reclutamiento",media, NA_real_),
      media_exposicion=if_else(equipo=="Exposicion",media, NA_real_),
    ) %>% filter(!is.na(media_exposicion)) %>% select(semana, media_exposicion)
    
  ) %>% kable(caption = paste0( "<center>REPORTE PRODUCTIVIDAD POR SEMANA<br>
                  HAPIN 36 meses al ", fecha2," </center>"), 
              align = "l") %>% 
  kable_classic(full_width=F, html_font = "Cambria") %>% column_spec(1, width = "2cm",  bold = T) %>%  kable_styling("striped", full_width = F) %>% scroll_box(width = "500px", height = "300px")

#grafica por mes
g_equipo_mes

#tabla mes
rec_mes %>% bind_rows(
  exp_mes
) %>% mutate(
  media_reclutamiento=if_else(equipo=="Reclutamiento",media, NA_real_),
  media_exposicion=if_else(equipo=="Exposicion",media, NA_real_),
) %>% filter(!is.na(media_reclutamiento)) %>% select(mes, Mes, media_reclutamiento) %>% 
  left_join(
    rec_mes %>% bind_rows(
      exp_mes
    ) %>% mutate(
      media_reclutamiento=if_else(equipo=="Reclutamiento",media, NA_real_),
      media_exposicion=if_else(equipo=="Exposicion",media, NA_real_),
    ) %>% filter(!is.na(media_exposicion)) %>% select(mes,Mes, media_exposicion)
    
  ) %>% ungroup() %>% arrange(mes) %>% select(-mes) %>%  kable(caption = paste0( "<center>REPORTE PRODUCTIVIDAD POR MES<br>
                  HAPIN 36 meses al ", fecha2," </center>"), 
                                                               align = "l") %>% 
  kable_classic(full_width=F, html_font = "Cambria") %>% column_spec(1, width = "2cm",  bold = T) %>%  kable_styling("striped", full_width = F) %>% scroll_box(width = "500px", height = "300px")

```

```{r promedios_crfs}

rec_por_dia_mes<-gt_hapin_II_data %>% filter(!is.na(c32_date_2)) %>% filter(visit=="b6") %>% 
  transmute(
    c32_date_2
  ) %>% mutate(
    Anio=lubridate::year(c32_date_2),
    mes=lubridate::month(c32_date_2),
    Mes=str_to_title(format(as.Date(c32_date_2), "%B")),
    semana=lubridate::week(c32_date_2),
    dia=lubridate::wday(c32_date_2, label = TRUE, abbr = FALSE)
  ) %>% group_by(Anio, mes, Mes, semana, dia) %>% count() %>% ungroup() %>% transmute(
    Anio, mes, Mes, semana, dia, realizados=n
  )


exp_por_dia_mes<-gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>% filter(visit=="b6") %>% 
  transmute(
    h42_date_2
  ) %>% mutate(
    Anio=lubridate::year(h42_date_2),
    mes=lubridate::month(h42_date_2),
    Mes=str_to_title(format(as.Date(h42_date_2), "%B")),
    semana=lubridate::week(h42_date_2),
    dia=lubridate::wday(h42_date_2, label = TRUE, abbr = FALSE)
  ) %>% group_by(Anio, mes, Mes, semana, dia) %>% count() %>% ungroup() %>% transmute(
    Anio, mes, Mes, semana, dia, realizados=n
  )

#productividad FOT
fot_por_dia_mes<-gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% filter(visit=="b6") %>% 
  transmute(
    c86_date
  ) %>% mutate(
    Anio=lubridate::year(c86_date),
    mes=lubridate::month(c86_date),
    Mes=str_to_title(format(as.Date(c86_date), "%B")),
    semana=lubridate::week(c86_date),
    dia=lubridate::wday(c86_date, label = TRUE, abbr = FALSE)
  ) %>% group_by(Anio, mes, Mes, semana, dia) %>% count() %>% ungroup() %>% transmute(
    Anio, mes, Mes, semana, dia, realizados=n
  )

#preparar set de datos para grafica reclutamiento dia mes
dt_rec_promedio_dia_mes<-rec_por_dia_mes %>% group_by(
  Anio, mes, Mes, dia
) %>% summarise(realizados_dia=sum(realizados)) %>% ungroup() %>% 
  left_join(
    rec_por_dia_mes %>% group_by(Anio,mes, dia) %>% count() %>% ungroup() %>% transmute(Anio, mes, dia, contador_dia=n)
  ) %>% mutate(
    promedio_dia_mes= realizados_dia / contador_dia
  ) %>%  left_join(
    rec_por_dia_mes %>% group_by(Anio,mes) %>% summarise(total_mes=sum(realizados)) %>% ungroup() 
  ) %>% mutate(
    prop_dia=percent(promedio_dia_mes/total_mes, digits = 1)
  ) %>% transmute(
    Anio, mes, Mes, dia, promedio_por_dia=round(promedio_dia_mes,digits = 0), prop_dia, total_mes
  ) %>% distinct()

#preparar set de datos para grafica Exposicion dia mes
dt_exp_promedio_dia_mes<-exp_por_dia_mes %>% group_by(
  Anio, mes, Mes, dia
) %>% summarise(realizados_dia=sum(realizados)) %>% ungroup() %>% 
  left_join(
    exp_por_dia_mes %>% group_by(Anio,mes, dia) %>% count() %>% ungroup() %>% transmute(Anio, mes, dia, contador_dia=n)
  ) %>% mutate(
    promedio_dia_mes= realizados_dia / contador_dia
  ) %>%  left_join(
    exp_por_dia_mes %>% group_by(Anio,mes) %>% summarise(total_mes=sum(realizados)) %>% ungroup() 
  ) %>% mutate(
    prop_dia=percent(promedio_dia_mes/total_mes, digits = 1)
  ) %>% transmute(
    Anio, mes, Mes, dia, promedio_por_dia=round(promedio_dia_mes,digits = 0), prop_dia, total_mes
  ) %>% distinct()

#preparar set de datos para grafica FOT dia mes
dt_fot_promedio_dia_mes<-fot_por_dia_mes %>% group_by(
  Anio, mes, Mes, dia
) %>% summarise(realizados_dia=sum(realizados)) %>% ungroup() %>% 
  left_join(
    fot_por_dia_mes %>% group_by(Anio,mes, dia) %>% count() %>% ungroup() %>% transmute(Anio, mes, dia, contador_dia=n)
  ) %>% mutate(
    promedio_dia_mes= realizados_dia / contador_dia
  ) %>%  left_join(
    fot_por_dia_mes %>% group_by(Anio,mes) %>% summarise(total_mes=sum(realizados)) %>% ungroup() 
  ) %>% mutate(
    prop_dia=percent(promedio_dia_mes/total_mes, digits = 1)
  ) %>% transmute(
    Anio, mes, Mes, dia, promedio_por_dia=round(promedio_dia_mes,digits = 0), prop_dia, total_mes
  ) %>% distinct()

# nb.cols<-30
# display.brewer.all()
# mycolors<- colorRampPalette(brewer.pal(10,"Dark2"))(nb.cols)

#productividad por dia, agrupado por mes barras
g_produc_reclutamiento_dia_mes<- dt_rec_promedio_dia_mes %>% ggplot(
  aes(x=mes, y=promedio_por_dia, group=dia, fill=dia, width=0.90)
) + geom_bar(
  stat = "identity", position = "dodge", color="black"
) + scale_fill_manual(values = c(
  '#aeb6bf', '#a9dfbf',  '#819FF7', '#f1948a', '#bb8fce', '#cd6155'
)
) +
  #scale_fill_brewer(palette = "RdGy") +
  #scale_fill_manual(values = mycolors) +
  
  theme(axis.text.x = element_text( size = 8))+
  scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(name="Promedio", 
                                                                        breaks = c(0:12))+
  labs(
    title = "PROMEDIO DE PRODUCTIVIDAD DIA / MES",
    subtitle = "Reclutamiento Hapin 36 meses",
    caption = "Indicador trazador Reclutamiento: M10"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 





#productividad por dia, agrupado por mes barras
g_produc_expo_dia_mes<-dt_exp_promedio_dia_mes %>% ggplot(
  aes(x=mes, y=promedio_por_dia, group=dia, fill=dia, width=0.90)
) + geom_bar(
  stat = "identity", position = "dodge", color="black"
) + scale_fill_manual(values = c(
  '#a9dfbf',  '#819FF7', '#f1948a', '#bb8fce', '#cd6155'
)
) +
  theme_get()+
  theme(axis.text.x = element_text( size = 8))+
  scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(name="Promedio", 
                                                                        breaks = c(0:12))+
  labs(
    title = "PROMEDIO DE PRODUCTIVIDAD DIA / MES",
    subtitle = "Exposicion Hapin 36 meses",
    caption = "Indicador trazador Exposicion: H42"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 

#productividad  FOT por dia, agrupado por mes barras
g_produc_fot_dia_mes<-dt_fot_promedio_dia_mes %>% ggplot(
  aes(x=mes, y=promedio_por_dia, group=dia, fill=dia, width=0.90)
) + geom_bar(
  stat = "identity", position = "dodge", color="black"
) + scale_fill_manual(values = c(
  '#aeb6bf', '#a9dfbf',  '#819FF7', '#f1948a', '#bb8fce', '#cd6155'
)
) +
  theme_get()+
  theme(axis.text.x = element_text( size = 8))+
  scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(name="Promedio", 
                                                                        breaks = c(0:12))+
  labs(
    title = "PROMEDIO DE PRODUCTIVIDAD DIA / MES",
    subtitle = "FOT Hapin 36 meses",
    caption = "Indicador trazador Exposicion: C86"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 

# g_produc_reclutamiento_dia_mes
# 
# 
# g_produc_expo_dia_mes
# 
# g_produc_fot_dia_mes

```

```{r productividad_iniciales}
#cargar todos los crfs y las iniciales
iniciales<-gt_hapin_II_data %>% filter(visit=="b6") %>%  
  select(id, s4_date, contains("_by"), contains("_date")) %>% mutate_all(as.character) %>% 
  discard(~all(is.na(.) | . == "")) %>% 
  select(id, s4_date, s4_by, e3_by, everything())


#sacar los ids que se han hecho en pareja grupo reclutamiento
#s4
crf_reclutamiento<-iniciales %>% filter(!is.na(s4_by)) %>% select(id, fecha=s4_date, iniciales=s4_by) %>% mutate(
  Anio=lubridate::year(fecha),
  mes=lubridate::month(fecha),
  Mes=format(as.Date(fecha), "%B"),
  crf="s4"
) %>% bind_rows(
  #m10
  iniciales %>% filter(!is.na(m10_by)) %>% select(id, fecha=m10_date, iniciales=m10_by) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="m10"
  )
) %>% bind_rows(
  #m11
  iniciales %>% filter(!is.na(m11_by)) %>% select(id, fecha=m11_date, iniciales=m11_by) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="m11"
  )
  
)   %>% bind_rows(
  #m19
  iniciales %>% filter(!is.na(m19_by)) %>% select(id, fecha=m19_date, iniciales=m19_by) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="m19"
  )
) %>% bind_rows(
  #c31
  iniciales %>% filter(!is.na(c31_by_2)) %>% select(id, fecha=c31_date_2, iniciales=c31_by_2) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="c31"
  )
) %>% bind_rows(
  #c32
  iniciales %>% filter(!is.na(c32_by_2)) %>% select(id, fecha=c32_date_2, iniciales=c32_by_2) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="c32"
  )
) %>% bind_rows(
  #c35
  iniciales %>% filter(!is.na(c35_by_2)) %>% select(id, fecha=c35_date_2, iniciales=c35_by_2) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="c35"
  )
) %>% bind_rows(
  #c42
  iniciales %>% filter(!is.na(c42_by)) %>% select(id, fecha=c42_date, iniciales=c42_by) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="c42"
  )
) %>% bind_rows(
  #E3
  iniciales %>% filter(!is.na(e3_by)) %>% select(id, fecha=e3_date, iniciales=e3_by) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="e3"
  )
) %>% bind_rows(
  #h57
  iniciales %>% filter(!is.na(h57_by_2)) %>% select(id, fecha=h57_date_2, iniciales=h57_by_2) %>% mutate(
    Anio=lubridate::year(fecha),
    mes=lubridate::month(fecha),
    Mes=format(as.Date(fecha), "%B"),
    crf="h57"
  )
)

#sacar set de crfs hechos en pareja
crf_reclutamiento_pareja<-crf_reclutamiento %>% group_by(id, iniciales) %>% 
  mutate(
    numeracion=row_number()
    
  ) %>%ungroup() %>% group_by(id, numeracion) %>% mutate(
    flag=row_number()
  ) %>% mutate(
    pareja=if_else(flag=="2","Si", "No")
  ) %>% ungroup() %>%  filter(pareja=="Si") %>% select(id) %>% distinct()

#hogares hechos individualmente por iniciales
crfs_reclutamiento_individual_iniciales<-crf_reclutamiento %>% anti_join(
  crf_reclutamiento_pareja
) %>% mutate(metodo=iniciales) %>% arrange(desc(fecha),id) 

#hogares hechos individualmente
crfs_reclutamiento_individual<-crf_reclutamiento %>% anti_join(
  crf_reclutamiento_pareja
) %>% mutate(metodo="INDIVIDUAL") %>% arrange(desc(fecha),id) 

#Verificar problemas de fechas en crfs hogares abordados individualmente ----
# crfs_reclutamiento_individual %>% select(id, fecha,iniciales) %>% group_by(id, fecha) %>% unique() %>% ungroup() %>% group_by(id) %>% count() %>% filter(n>1)

crfs_reclutamiento_equipo<-crf_reclutamiento_pareja %>% left_join(
  crf_reclutamiento
) %>% mutate(metodo=" EQUIPO") %>% arrange(desc(fecha))

#unificar set de datos reclutamiento en equipo e individual con iniciales
dt_reclutamiento_productividad<-crfs_reclutamiento_individual_iniciales %>% bind_rows(
  crfs_reclutamiento_equipo
)

#unificar set de datos reclutamiento en equipo e individual
dt_reclutamiento_productividad_ei<-crfs_reclutamiento_individual %>% bind_rows(
  crfs_reclutamiento_equipo
)

#denominador por mes en reclutamiento
dt_denominador_reclutamiento<-dt_reclutamiento_productividad %>%select( id, Anio, mes, metodo) %>% distinct() %>%  group_by(
  Anio, mes, metodo
) %>% count()  %>%  group_by(Anio, mes) %>% summarise(total=sum(n))

#grafica porcentaje de hogares abordados en pareja  e individualmente con iniciales
g_reclutamiento_equipo_iniciales<- dt_reclutamiento_productividad %>%select( id, Anio, mes, metodo) %>% distinct() %>%  group_by(
  Anio, mes, metodo
) %>% count()  %>%  ungroup() %>% left_join(
  dt_denominador_reclutamiento
) %>% mutate(
  porcent= percent((n / total),digits = 1),
  porcentaje=(n/total) * 100
) %>%   ggplot(
  aes(x=mes, y=porcentaje, fill=metodo)
) + geom_bar(stat = "identity", position = "dodge") + scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(
  name="Porcentaje (%)",   breaks = c(0,10,20,30,40,50,60,70,80,90,100)
) +  theme_get()+
  theme(axis.text.x = element_text( size = 8)) + labs(
    title = "PRODUCTIVIDAD DE RECLUTAMIENTO",
    subtitle = "POR MES",
    caption = "Método: Equipo / iniciales"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 

#grafica porcentaje de hogares abordados en pareja  e individualmente
g_reclutamiento_equipo_individual<- dt_reclutamiento_productividad_ei %>%select( id, Anio, mes, metodo) %>% distinct() %>%  group_by(
  Anio, mes, metodo
) %>% count()  %>%  ungroup() %>% left_join(
  dt_denominador_reclutamiento
) %>% mutate(
  porcent= percent((n / total),digits = 1),
  porcentaje=(n/total) * 100
) %>%   ggplot(
  aes(x=mes, y=porcentaje, fill=metodo)
) + geom_bar(stat = "identity", position = "dodge") + scale_x_continuous(name="Mes", breaks = c(1:12)) + scale_y_continuous(
  name="Porcentaje (%)",   breaks = c(0,10,20,30,40,50,60,70,80,90,100)
) +  theme_get()+
  theme(axis.text.x = element_text( size = 8)) + labs(
    title = "PRODUCTIVIDAD DE RECLUTAMIENTO",
    subtitle = "POR MES",
    caption = "Método: Equipo / Individual"
  )  + theme(plot.title = element_text(hjust=0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust=0.5, size = 6)
  ) 



```

### SEGUN METODO

```{r graficas_prod}
g_reclutamiento_equipo_iniciales

g_reclutamiento_equipo_individual
```

~~~~
  *Productividad al "`r as.Date(f_week2) %>% format(.,"%A %d %B  %Y")` "*
  ~~~~
  
