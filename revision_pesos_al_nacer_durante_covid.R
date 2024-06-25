library("tidyverse")

#leer resumen de pesos tomados durante covid
resumen_pesos<-read_csv("c:/temp/resumen_pesos.csv")

resumen_pesos<-resumen_pesos %>% mutate_all(as.character)

library(blandr)

# Generates a plot, with no optional arguments
blandr.draw(as.numeric(resumen_pesos$weight_maternalreport) , as.numeric(resumen_pesos$weight_chart) )

# Generates a plot, using the in-built R graphics
blandr.draw( measurement1 , measurement2 , plotter = 'rplot' )

# Generates a plot, with title changed
blandr.draw( measurement1 , measurement2 , plotTitle = 'Bland-Altman example plot' )

# Generates a plot, with title changed, and confidence intervals off
blandr.draw(as.numeric(resumen_pesos$weight_chart), 
                       as.numeric(resumen_pesos$weight_maternalreport) ,  plotTitle = 'Bland-Altman Pesos Hospital - Referencia madre' ,
            ciDisplay = TRUE , ciShading = TRUE )


library(BlandAltmanLeh)
bland.altman.plot(as.numeric(resumen_pesos$weight_chart), 
                  as.numeric(resumen_pesos$weight_maternalreport), 
                  main="Pesos Hospital - Referencia Madre", xlab="Means", ylab="Differences")

bland.altman.plot(as.numeric(resumen_pesos$weight_chart_peds), 
                  as.numeric(resumen_pesos$weight_maternalreport), 
                  main="Pesos Pediatria - Referencia Madre", xlab="Means", ylab="Differences")

resumen_pesos<-resumen_pesos %>% mutate_all(as.character)

resumen_pesos %>% filter(!is.na(weight_maternalreport)) %>% select(id, weight_maternalreport) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% transmute(id,lugar_pesado=c30_wt_where, lugar_nacio=c30_where )
) %>% group_by(lugar_nacio, lugar_pesado) %>% count()


resumen_pesos %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id)
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>% select(id, visit)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% transmute(id, visita=visit)
) %>% mutate(
  visit=if_else(is.na(visita), visit, visita)
) %>% group_by(visit) %>% count()
  filter(is.na(visita))

#obtener pesos en formato numerico
comparacion_pesos<-resumen_pesos %>% transmute(weight_chart=as.numeric(weight_chart),
                                               weight_materlnal_report=as.numeric(weight_maternalreport),
                                               weight_peds=as.numeric(weight_chart_peds))


#sample
weight_chart<-comparacion_pesos %>% filter(!is.na(weight_chart))
weight_maternal_report<-comparacion_pesos %>% filter(!is.na(weight_materlnal_report))
weight_peds<-comparacion_pesos %>% filter(!is.na(weight_peds))

hospital_pulsera<-comparacion_pesos %>% filter(!is.na(weight_chart)) %>% transmute(hospital=as.numeric(weight_chart), pulsera=as.numeric(weight_materlnal_report))

pedia_pulsera<-comparacion_pesos %>% filter(!is.na(weight_peds)) %>% 
  transmute(pedia=as.numeric(weight_peds), pulsera=as.numeric(weight_materlnal_report)) %>% filter(
    !is.na(pulsera)
  )

weight_chart %>% count()
weight_maternal_report %>% count()
weight_peds %>% count()

hist(mtcars$hp, freq=FALSE)
curve(dnorm(x,
            mean=mean(mtcars$hp),
            sd=sd(mtcars$hp)),
      add=TRUE, col="red")

#histograma pesos hospital
hist(weight_chart$weight_chart, freq=FALSE)+labs(title = "Pesos Hospital")
curve(dnorm(x, 
            mean=mean(weight_chart$weight_chart), 
            sd=sd(weight_chart$weight_chart)), 
      add=TRUE, col="red") 

#histograma pesos referencia de madre
hist(weight_maternal_report$weight_materlnal_report, freq=FALSE)
curve(dnorm(x, 
            mean=mean(weight_maternal_report$weight_materlnal_report), 
            sd=sd(weight_maternal_report$weight_materlnal_report)), 
      add=TRUE, col="red") 


#histograma pesos pediatria < 24horas
hist(weight_peds$weight_peds, freq=FALSE)
curve(dnorm(x, 
            mean=mean(weight_peds$weight_peds), 
            sd=sd(weight_peds$weight_peds)), 
      add=TRUE, col="red") 


cor(hospital_pulsera$hospital, hospital_pulsera$pulsera, use="everything",
    method=c("pearson", "kendall", "spearman"))

cor(pedia_pulsera$pedia, pedia_pulsera$pulsera, use="everything",
    method=c("pearson", "kendall", "spearman"))
