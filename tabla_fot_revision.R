#dt_export_complete<-readxl::read_xlsx("c:/temp/fot/Detailed (Export Excluded Measurements).xlsx", skip = 5)
dt_export_complete<-readxl::read_xlsx("c:/temp/fot/Detailed (No Excluded Measurements).xlsx", skip = 5)

library(ggplot2)

dt_table_analisis1<-dt_export_complete %>% mutate(
  id=if_else(substr(Patient, 5,7)=="DIA", 
             substr(
    Patient, 11,15) , 
  substr( Patient, 5, 9)

) 
) %>% 
  transmute(
    id, test_id=`Test ID`, variation=Variation, R7, `CV(R7)`, Excluded, X7
  ) 

dt_table_analisis2<-dt_table_analisis1 %>% filter(variation!="Mean") %>% left_join(
    # dt_export_complete %>%mutate(
    #   id=if_else(substr(Patient, 5,3)=="DIA", substr(
    #     Patient, 11,15), 
    #     substr( Patient, 5, 9)
    #   ) 
    # ) %>% 
  dt_table_analisis1 %>% 
      transmute(
        id, test_id, variation, Mean_R7=as.numeric(R7), cv_r7=as.numeric(`CV(R7)`)
      ) %>% filter(variation=="Mean") %>% select(id, test_id, Mean_R7, cv_r7)
  ) 
dt_table_analisis3<-dt_table_analisis2 %>% left_join(
  dt_table_analisis2   %>% group_by(id,test_id) %>% summarize(
        mean_calculated=mean(as.numeric(R7))
      ) 
  ) %>% mutate(
    dif_mean_fot=as.numeric(R7) - as.numeric(Mean_R7),
    dif_mean_calculated= as.numeric(R7) - as.numeric(mean_calculated)
  ) 

dt_analisis_4<-dt_table_analisis3 %>% left_join(
  dt_table_analisis3 %>% group_by(id, test_id) %>% 
    summarize(min_dif_mean_fot=min(dif_mean_fot),
              max_dif_mean_fot=max(dif_mean_fot),
              min_dif_mean_calculated=min(dif_mean_calculated),
              max_dif_mean_calculated=max(dif_mean_calculated)
              )
) %>% arrange(
  desc(id, test_id)
)

#dt_analisis_4 %>% writexl::write_xlsx("output/tabla_conExcluded.xlsx")

dt_analisis_4 %>% writexl::write_xlsx("output/tabla_SinExcluded_x7_r7.xlsx")
dt_analisis_4 %>% group_by(id)

#histograma diferencia

dt_histo <- ggplot(dt_analisis_4, aes(x=dif_mean_fot))

dt_histo + geom_histogram()


dt_histo2<-dt_histo + geom_histogram(binwidth = 0.1, col='black', fill='green', alpha=0.4)

dt_histo2 + xlab('Diferencias') + ylab('count') + ggtitle('Diferencias entre valor medida y promedio')

#histograma R7
x<-dt_analisis_4$R7
ggplot(dt_analisis_4, aes(x=R7))+stat_bin(binwidth=1, position="identity") 

ggplot(dat, aes(x=Date_of_Onset, fill=suburb)) + 
  stat_bin(binwidth=1, position="identity") + 
  scale_x_date(breaks=date_breaks(width="1 month"))

 plot(x=dt_analisis_4$R7, y=dt_analisis_4$X7)
plot()

