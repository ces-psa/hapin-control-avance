

#revision datos credi Lisa
dt_credi<-read_csv("c:/temp/air_HAPIN_CREDI_ITT_nf_20210219.csv", col_names = TRUE,
                   col_types = NULL, guess_max = 13500, na=c("","NA"),trim_ws = TRUE, skip = 0, n_max = 13473) 




dt_credi %>% filter(timepoint!="Birth") %>% filter(!is.na(c35_age))


dt_credi %>%  filter(timepoint!="Birth") %>% filter(!is.na(c35_age)) %>%  mutate(
  edad_adecuada=case_when(
    as.numeric(c35_age) < 6 & !is.na(c35_move)  ~ "1",
    as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 12 & !is.na(c35_grasp)  ~ "1",
    as.numeric(c35_age) >=12  & !is.na(c35_roll)  ~ "1",
    is.na(c35_age) ~ NA_character_,
    TRUE ~ "0"
  )
) %>% mutate(missing_total=if_else(
  ultima_columna==139,1,0
)) %>% 
  select(irc,visit, timepoint, hhid, c35_age, c35_move, c35_suck,
             c35_grasp, c35_roll, c35_interest, edad_adecuada,missing_total) %>% group_by(irc,visit,missing_total,edad_adecuada) %>% count() %>% 
  writexl::write_xlsx("output/conteo_inicio_correcto.xlsx")

dt_credi<-dt_credi %>% mutate(ultima_columna=NA_integer_)

(dt_credi[1,248])-1

#ciclos R

#verificar que hayan terminado correctamente

#buscar cual fue la última columna que tiene datos
for(i in 1:dim(dt_credi)[1]) {
j=229
while (j>139) {
  dt_credi[i,248]=(j-1)
  j=j-1
  if ( !is.na(dt_credi[i,j]))
  break
  else
  j<-j
  
}
}



#crear la validacion si terminó bien
#crear la columna terminó bien 0= Si, diferente que 0 hay error al terminar
dt_credi<-dt_credi %>% mutate(termino_bien= NA_integer_)

dt_credi %>% select(248,249) %>% filter(ultima_columna==139)

dt_credi %>% filter(ultima_columna!=139) 

#prueba de sumas


#Iteración para llenar la columna si terminó bien o no
for (i in 1:dim(dt_credi)[1] 
     ){
  
  if (
    dt_credi$ultima_columna[i] == 139)
  { 
    dt_credi[i,249]=NA_integer_

    }
  else
  {
    h=dt_credi$ultima_columna[i]
    #eliminar valor NA 1
    if(is.na(dt_credi[i,h])){
      valor1=999
    }
    else
    {
      valor1=dt_credi[i,h]
    }
    
    #eliminar valor NA 2
    if(is.na(dt_credi[i,h-1])){
      valor2=999
    }
    else
    {
      valor2=dt_credi[i,h-1]
    }
    
    #eliminar valor NA 3
    if(is.na(dt_credi[i,h-2])){
      valor3=999
    }
    else
    {
      valor3=dt_credi[i,h-2]
    }
    #eliminar valor NA 4
    if(is.na(dt_credi[i,h-3])){
      valor4=999
    }
    else
    {
      valor4=dt_credi[i,h-3]
    }
    #eliminar valor NA 5
    if(is.na(dt_credi[i,h-4])){
      valor5=999
    }
    else
    {
      valor5=dt_credi[i,h-4]
    }
    
    # sumar los valores para determinar si terminó bien
    dt_credi$termino_bien[i] = sum(
      valor1,valor2,valor3,valor4,valor5
      )
   # print(dt_credi$termino_bien[i])
  }

}

#revisar valores de los que terminaron mal
dt_credi %>% select(1:2,4,213,212, 248,249) %>% filter(ultima_columna!=139) %>% group_by(termino_bien) %>% filter(
  termino_bien=="3997"
)

#revisar la cantidad de NA
dt_credi<-dt_credi %>% mutate(cantidad_NA=NA_integer_)
dt_credi<-dt_credi %>% mutate(primera_columna=NA_integer_)
dt_credi %>% select(primera_columna)
dt_credi[2,251]

#revisar los NA desde la primera a la última columna

#buscar la primera columna con valor
for(i in 1:dim(dt_credi)[1]
    ) {
  j=dt_credi$ultima_columna[i]
  k=139
  if (dt_credi$ultima_columna[i]==139) {
    dt_credi[i,251]=139
  }
  else {
    while (k<229) {
      dt_credi[i,251]=(k+1)
      k=k+1
      if ( !is.na(dt_credi[i,k]))
        break
      else
        k<-k
      
    }
  }
 
}

#revisar datos de primera columna
dt_credi %>% select(hhid,visit,ultima_columna, primera_columna)

#sacar la cantidad de NA
for(i in 1:dim(dt_credi)[1]
    ) {
  p=dt_credi$primera_columna[i]
  u=dt_credi$ultima_columna[i]
  l=p+1
  k=0
  m=0
  if(u==p){
 dt_credi$cantidad_NA[i]=999
  }
  else {
    while (l<u+1) {
      if (is.na(dt_credi[i,l])) {
        k=k+1
        dt_credi$cantidad_NA[i]=k
        
        l=l+1

      }
      else
      {
        
        l=l+1
        
      }
      
    }
  }

}

dt_credi %>% select(hhid,visit,primera_columna, ultima_columna,cantidad_NA, termino_bien) %>% 
  filter(cantidad_NA=="1")

#conteo de los que si terminaron bien
dt_credi %>%mutate(
  terminaron_bien_conteo=if_else(
  termino_bien!="0" | is.na(termino_bien),"No","Si"
)) %>% filter(visit!="Birth") %>%  group_by(irc,terminaron_bien_conteo) %>% count()

#conteo de missing
dt_credi %>%filter(cantidad_NA!=999) %>% 
  group_by(irc,visit,cantidad_NA) %>% count() %>% writexl::write_xlsx("output/conteo_missing.xlsx")

#revisar completos
dt_credi %>%  filter(timepoint!="Birth") %>% filter(!is.na(c35_age)) %>%  mutate(
  edad_adecuada=case_when(
    as.numeric(c35_age) < 6 & !is.na(c35_move)  ~ "1",
    as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 12 & !is.na(c35_grasp)  ~ "1",
    as.numeric(c35_age) >=12  & !is.na(c35_roll)  ~ "1",
    is.na(c35_age) ~ NA_character_,
    TRUE ~ "0"
  )
) %>%   mutate(
  terminaron_bien_conteo=if_else(
    termino_bien!="0" | is.na(termino_bien),"No","Si"
  )) %>% filter(visit!="Birth") %>% filter(edad_adecuada=="1") %>% mutate(
    terminaron_bien_conteo=if_else(
      termino_bien!="0" | is.na(termino_bien),"No","Si"
    )) %>% filter(terminaron_bien_conteo=="Si") %>%   group_by(
    irc, visit,
    edad_adecuada,
    terminaron_bien_conteo
  ) %>% count() %>% writexl::write_xlsx("output/conteo_completos.xlsx")


#exportar los datos
#revisar completos
dt_credi %>%  filter(timepoint!="Birth") %>% filter(!is.na(c35_age)) %>%  mutate(
  edad_adecuada=case_when(
    timepoint!="Birth" & as.numeric(c35_age) < 6 & !is.na(c35_move)  ~ "1",
    timepoint!="Birth" & as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 12 & !is.na(c35_grasp)  ~ "1",
    timepoint!="Birth" & as.numeric(c35_age) >=12  & !is.na(c35_roll)  ~ "1",
    timepoint!="Birth" & is.na(c35_age) ~ NA_character_,
    TRUE ~ "0"
  )
) %>%   mutate(
  terminaron_bien_conteo=if_else(
    timepoint!="Birth" &  (termino_bien!="0" | is.na(termino_bien) ),"No","Si"
  )) %>% write_csv("output/bd_credi_para_conteos.csv")
