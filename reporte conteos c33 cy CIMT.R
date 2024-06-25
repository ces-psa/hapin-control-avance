# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

library("tidyverse")

#cantidad de nacimientos 770 nacidos
dt_nacimientos<-gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, dob=c30_dob)

gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)

#visitas esperadas por mes
#B1
c33_b1_feb_expect<-dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)<"2020-03-01") %>% 
  anti_join(
    #salidas antes de febrero 2020
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
      as.Date(e3_date_exit_c)<"2020-02-01"
    )
  ) %>% count %>% print()

#B2

c33_b2_feb_expect<-dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)<"2020-03-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-02-01"
  )
) %>% count()%>% print()

#B3
c33_b3_feb_expect<-dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% mutate(flag1=if_else(
  lubridate::month(expected_b3)>2 & lubridate::year(expected_b3)==2020,"1","0")
)%>% filter(as.Date(expected_b3)<"2020-03-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-02-01"
  )
) %>% count()%>% print()
#B4
c33_b4_feb_expect<-dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)<"2020-03-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-02-01"
  )
) %>% count()%>% print()




#visitas esperadas por mes marzo
#B1
c33_b1_marzo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-03-01" & as.Date(expected_b1)<"2020-04-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-03-01"
  )
) %>% count()%>% print()

#B2
c33_b2_marzo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-03-01" & as.Date(expected_b2)<"2020-04-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-03-01"
  )
) %>% count()%>% print()


#B3
c33_b3_marzo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
)  %>% filter(as.Date(expected_b3)>="2020-03-01" & as.Date(expected_b3)<"2020-04-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-03-01"
  )
) %>% count()%>% print()

#B4
c33_b4_marzo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-03-01" & as.Date(expected_b4)<"2020-04-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-03-01"
  )
) %>% count()%>% print()


#visitas esperadas por mes abril
#B1
c33_b1_abril_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-04-01" & as.Date(expected_b1)<"2020-05-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-04-01"
  )
) %>% count()%>% print()


#B2
c33_b2_abril_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-04-01" & as.Date(expected_b2)<"2020-05-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-04-01"
  )
) %>% count()%>% print()

#B3
c33_b3_abril_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-04-01" & as.Date(expected_b3)<"2020-05-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-04-01"
  )
) %>% count()%>% print()

#B4
c33_b4_abril_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-04-01" & as.Date(expected_b4)<"2020-05-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-04-01"
  )
) %>% count()%>% print()


#visitas esperadas por mes mayo
#B1
c33_b1_mayo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-05-01" & as.Date(expected_b1)<"2020-06-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-05-01"
  )
) %>% count()%>% print()

  #B2
c33_b2_mayo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-05-01" & as.Date(expected_b2)<"2020-06-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-05-01"
  )
) %>% count()%>% print()

#B3
c33_b3_mayo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-05-01" & as.Date(expected_b3)<"2020-06-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-05-01"
  )
) %>% count()%>% print()

#B4
c33_b4_mayo_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-05-01" & as.Date(expected_b4)<"2020-06-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-05-01"
  )
) %>% count()%>% print()

#visitas esperadas por mes junio
#B1
c33_b1_junio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-06-01" & as.Date(expected_b1)<"2020-07-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-06-01"
  )
) %>% count()%>% print()


#B2
c33_b2_junio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-06-01" & as.Date(expected_b2)<"2020-07-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-06-01"
  )
) %>% count()%>% print()

#B3
c33_b3_junio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-06-01" & as.Date(expected_b3)<"2020-07-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-06-01"
  )
) %>% count()%>% print()

#B4
c33_b4_junio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-06-01" & as.Date(expected_b4)<"2020-07-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-06-01"
  )
) %>% count()%>% print()



#visitas esperadas por mes julio
#B1
c33_b1_julio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-07-01" & as.Date(expected_b1)<"2020-08-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-07-01"
  )
) %>% count()%>% print()


#B2
c33_b2_julio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
)  %>% filter(as.Date(expected_b2)>="2020-07-01" & as.Date(expected_b2)<"2020-08-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-07-01"
  )
) %>% count()%>% print()

#B3
c33_b3_julio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
)  %>% filter(as.Date(expected_b3)>="2020-07-01" & as.Date(expected_b3)<"2020-08-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-07-01"
  )
) %>% count()%>% print()

#B4
c33_b4_julio_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
)  %>% filter(as.Date(expected_b4)>="2020-07-01" & as.Date(expected_b4)<"2020-08-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-07-01"
  )
) %>% count()%>% print()



#visitas esperadas por mes Agosto
#B1
c33_b1_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
)  %>% filter(as.Date(expected_b1)>="2020-08-01" & as.Date(expected_b1)<"2020-09-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-08-01"
  )
) %>% count()%>% print()

#B2
c33_b2_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-08-01" & as.Date(expected_b2)<"2020-09-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-08-01"
  )
) %>% count()%>% print()

#B3
c33_b3_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-08-01" & as.Date(expected_b3)<"2020-09-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-08-01"
  )
) %>% count()%>% print()

#B4
c33_b4_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-08-01" & as.Date(expected_b4)<"2020-09-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-08-01"
  )
) %>% count()%>% print()


#B2 septiembre
c33_b2_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-09-01" & as.Date(expected_b2)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B2 septiembre
c33_b3_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-09-01" & as.Date(expected_b3)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B3 septiembre
c33_b4_agosto_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-09-01" & as.Date(expected_b4)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()


#REVISAR DATOS AGRUPADOS DE SEPTIEMBRE EN ADELANTE
#B1  No hay B1 pendientes
c33_b1_agrupados_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b1)>="2020-09-01" & as.Date(expected_b1)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B2  
c33_b2_agrupados_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b2)>="2020-09-01" & as.Date(expected_b2)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B3  
c33_b3_agrupados_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b3)>="2020-09-01" & as.Date(expected_b3)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B4  
c33_b4_agrupados_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-09-01" & as.Date(expected_b4)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()

#B4  octubre en adelante
c33_b4_agrupados_expect <- dt_nacimientos %>% mutate(
  expected_b1=as.Date(dob) + lubridate::days(round(30.25*3)),
  expected_b2=as.Date(dob) + lubridate::days(round(30.25*6)),
  expected_b3=as.Date(dob) + lubridate::days(round(30.25*9)),
  expected_b4=as.Date(dob) + lubridate::days(round(30.25*12))
) %>% filter(as.Date(expected_b4)>="2020-09-01" & as.Date(expected_b4)<"2020-10-01") %>% anti_join(
  #salidas antes de febrero 2020
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-09-01"
  )
) %>% count()%>% print()


#-------------------------------------------------------------------


#ARMAR LA TABLITA PARA VISITAS
visita<-c("B1","B2","B3","B4")
dt_reporte<-data.frame(visita) 
  
dt_reporte %>% mutate(
  `Child Anthropometry`="Expected",
  `Till-Feb-2020` = case_when(
    visita=="B1" ~ c33_b1_feb_expect$n,
    visita=="B2" ~ c33_b2_feb_expect$n,
    visita=="B3" ~ c33_b3_feb_expect$n,
    visita=="B4" ~ c33_b4_feb_expect$n,
  ) ,
  `Mar-20` = case_when(
    visita=="B1" ~ c33_b1_marzo_expect$n,
    visita=="B2" ~ c33_b2_marzo_expect$n,
    visita=="B3" ~ c33_b3_marzo_expect$n,
    visita=="B4" ~ c33_b4_marzo_expect$n,
  ),
  `Apr-20` = case_when(
    visita=="B1" ~ c33_b1_abril_expect$n,
    visita=="B2" ~ c33_b2_abril_expect$n,
    visita=="B3" ~ c33_b3_abril_expect$n,
    visita=="B4" ~ c33_b4_abril_expect$n,
  ),
  `May-20` = case_when(
    visita=="B1" ~ c33_b1_mayo_expect$n,
    visita=="B2" ~ c33_b2_mayo_expect$n,
    visita=="B3" ~ c33_b3_mayo_expect$n,
    visita=="B4" ~ c33_b4_mayo_expect$n,
  ),
  `Jun-20` = case_when(
    visita=="B1" ~ c33_b1_junio_expect$n,
    visita=="B2" ~ c33_b2_junio_expect$n,
    visita=="B3" ~ c33_b3_junio_expect$n,
    visita=="B4" ~ c33_b4_junio_expect$n,
  ),
  `Jul-20` = case_when(
    visita=="B1" ~ c33_b1_julio_expect$n,
    visita=="B2" ~ c33_b2_julio_expect$n,
    visita=="B3" ~ c33_b3_julio_expect$n,
    visita=="B4" ~ c33_b4_julio_expect$n,
  ),
  `Agu-20` = case_when(
    visita=="B1" ~ c33_b1_agosto_expect$n,
    visita=="B2" ~ c33_b2_agosto_expect$n,
    visita=="B3" ~ c33_b3_agosto_expect$n,
    visita=="B4" ~ c33_b4_agosto_expect$n,
  )
) %>% print()

#-----------------------------------------------------------
#excel para conteo de realizadas en C33, se consideró la variable c33_ave_ht como parametro indicador que se tomaron datos
gt_emory_data_arm2 %>% filter(!is.na(c33_ave_ht)) %>% select(id, visit, c33_date) %>% 
  group_by(visit, anio=lubridate::year(c33_date), 
           mes=lubridate::month(c33_date)) %>% count() %>% write_csv( "output/c33_por_mes.csv"
                                                                                               )
#conteo de c33 realizados
gt_emory_data_arm2 %>% filter(!is.na(c33_ave_ht)) %>% select(id, visit, c33_date) %>% filter(
  lubridate::year(c33_date)>2019
) %>%  group_by(
  anio=lubridate::year(c33_date),
  mes=lubridate::month(c33_date),
  visit
) %>% count() %>% print(n=Inf)

##-----------------------------------------------------
#REVISION DE CONTEOS A24b-bp
#------------------------------------------------------
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_reason_o) %>% mutate(
    e3_reason_o=recode(e3_reason_o, "1"="Finalizacion_estudio",
                       "2"="No elegible",
                       "3"="Retiro_voluntario",
                       "4"="Retirada_por_equipo",
                       )
  )
) %>% #filter(e3_reason_o!="Finalizacion_estudio") %>% filter(!is.na(e3_reason_o))
mutate(
  fcc=as.Date(fpp) - lubridate::days(280),
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% mutate(
  flag=if_else(lubridate::year(e3_date_o)==2020 & lubridate::month(e3_date_o)>=2,"1","0")
) %>% filter(flag==0)

#a26_esperadas
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(
    !is.na(m17_ga)
  ) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fec_nac=if_else(is.na(c30_dob), as.Date(m17_ga),as.Date(c30_dob))
) %>% select(id, fec_nac) %>% mutate(
  fec_b4=fec_nac + lubridate::days(round(30.25*12)) 
 ) %>%
  anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o) %>% filter(
    as.Date(e3_date_o) < "2020-01-01"
  )
) %>%
  select(id, fec_b4) %>%  #filter(fec_b4>="2020-08-01" & fec_b4 < "2020-09-01")
group_by(lubridate::year(fec_b4),
         lubridate::month(fec_b4),
         ) %>% count() %>% print(n=Inf)
#conteo cauntas hemos realizado en LB y en B4
gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, visit) %>% group_by(visit) %>% count()

#conteo cuantas esperamos para febrero
gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, a26_date, visit) %>% filter(visit=="b4") %>% 
  group_by(lubridate::year(a26_date), lubridate::month(a26_date)) %>% count() %>% 
writexl::write_xlsx("output/a26_realizados_b4.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, visit, a26_date) %>% filter(lubridate::year(a26_date)>2019) %>% 
  group_by(
  lubridate::year(a26_date),
  lubridate::month(a26_date)
) %>% count()

#-------------------------------------------
#A26A CIMT SACAR LOS ESPERADOS POR MES 
#-------------------------------------------

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% mutate(
  fec_b4=as.Date(fec_nacimiento) + lubridate::days(365)
) %>% filter(fec_b4>="2020-03-01") %>% group_by(
  lubridate::year(fec_b4),
  lubridate::month(fec_b4) 
) %>% count() %>% writexl::write_xlsx("output/cimt_expected_b4.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(a26_3measur)) %>% select(id, visit) %>% group_by(visit)%>% count()


gt_emory_data_arm2 %>% filter(!is.na(a26_3measur)) %>% select(id, visit)  %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% mutate(
  fec_b4=as.Date(fec_nacimiento) + lubridate::days(365)
) %>% filter(as.Date(fec_b4)>="2020-04-01" & as.Date(fec_b4)<"2020-07-01") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_reason_o) %>% mutate(
    e3_reason_o=recode(e3_reason_o, "1"="Finalizacion_estudio",
                       "2"="No elegible",
                       "3"="Retiro_voluntario",
                       "4"="Retirada_por_equipo",
    )
  )
) %>%   select(id, fec_b4, e3_reason_o,e3_date_o) %>% filter(
    as.Date(e3_date_o)>="2020-04-01"
  ) %>% filter(
    as.Date(e3_date_o)<"2020-07-01"
  )

#Sacar conteos de esperados CIMT

cimt_candidate<<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_reason_o) %>% mutate(
    e3_reason_o=recode(e3_reason_o, "1"="Finalizacion_estudio",
                       "2"="No elegible",
                       "3"="Retiro_voluntario",
                       "4"="Retirada_por_equipo",
    )
  )
) %>% #filter(e3_reason_o!="Finalizacion_estudio") %>% filter(!is.na(e3_reason_o))
  mutate(
    fcc=as.Date(fpp) - lubridate::days(280),
    fec_nacimiento=if_else(
      is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
    )
  )
  
#Esperados por mes
#feb
cimt_b4_feb_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o)  %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
) %>% filter(lubridate::month(expected_b4)==2 & lubridate::year(expected_b4)==2020) %>% count() %>% print()

#MARZO
cimt_b4_mar_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=3,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==3 & lubridate::year(expected_b4)==2020) %>% count()

#ABRIL
cimt_b4_abr_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
    ) 
  ) %>% filter(
    expected_b4>="2020-04-01" & expected_b4<"2020-05-01"
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o, e3_reason_o)
  )
  
  count() %>% print()


#mMAYO
cimt_b4_may_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=5,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==5 & lubridate::year(expected_b4)==2020) %>% count() %>% print()

#mJUNIO
cimt_b4_jun_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=6,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==6 & lubridate::year(expected_b4)==2020) %>% count() %>% print()

#mJulio

cimt_b4_jul_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=7,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==7 & lubridate::year(expected_b4)==2020) %>% count() %>% print()

#magosto

cimt_b4_ago_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=8,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==8 & lubridate::year(expected_b4)==2020) %>% count() %>% print()

#Septiembre
cimt_b4_sep_expect<-cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=9,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% filter(lubridate::month(expected_b4)==9 & lubridate::year(expected_b4)==2020) %>% count() %>% print()


#octubre en adleante
cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o)
    ) %>% group_by(
      lubridate::year(expected_b4),
      lubridate::month(expected_b4)
    ) %>% count()

#octubre en adleante
cimt_candidate %>% mutate(
  expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
) %>% select(id, fec_nacimiento, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% mutate(
    flag1=if_else(
      lubridate::year(e3_date_exit_o)=="2020" & lubridate::month(e3_date_exit_o)>=9,
      "1","0"
    )
  ) %>% filter(flag1=="0")
) %>% group_by(
  lubridate::year(expected_b4),
  lubridate::month(expected_b4)
) %>% count()


####-----------------------------------------------------
#
#CONTEOS A24b
#--------------------------------------------------------

a24_b_fechas<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% 
                                  select(id_tamizaje=id, id=s4_main_id)
  ) %>%  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
  ) %>% mutate(
    fec_nac=if_else(
      is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)
    ),
    fcc=as.Date(m17_ga) - lubridate::days(280)
   ) %>% select(id, fcc,fec_nac) %>% mutate(
    p1_fec= as.Date(fcc) + lubridate::days(154),
    p2_fec=as.Date(fcc)+  lubridate::days(224),
    b1_fec=as.Date(fec_nac) + lubridate::days(round(30.25*3)),
    b2_fec=as.Date(fec_nac) + lubridate::days(round(30.25*6)),
    b4_fec=as.Date(fec_nac) + lubridate::days(round(30.25*12))
   ) %>% mutate(
     type=if_else(
       grepl("^35[0-9]",id),"owa","pwg"
     )
   ) %>%filter(type=="owa") %>%  print()

#esperadas
a24_b_fechas %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    e3_date_exit_o < "2020-10-01"
  )
) %>% group_by(
  lubridate::year(b4_fec),
  lubridate::month(b4_fec)
) %>%  count()
#conteo de realizadas a24b por año y mes
gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% select(id, a24b_date, visit) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    e3_date_exit_o < "2020-09-01"
)
) %>% filter(
  a24b_date>"2020-01-01"
) %>%  group_by(
  lubridate::year(a24b_date),
  lubridate::month(a24b_date),
  visit
) %>% count() %>% print(n=Inf)

#identificar cuantos a24b se hicieron en LB, p1 y p2
gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% select(id, visit) %>% group_by(visit) %>% count()

#SACAR LOS A24b ESPERADOS DE P1 EN ADELANTE
#esperadas para Febrero
#P1 
a24_b_fechas %>% filter(p1_fec<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-02-01"
  )
)

#P2 
a24_b_fechas %>% filter(p2_fec<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-02-01"
  )
)

#b1  
a24_b_fechas %>% filter(b1_fec<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-02-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-02-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec<="2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-02-01"
  )
)

#esperadas marzo
#P1 
a24_b_fechas %>% filter(p1_fec>="2020-03-01" & p1_fec<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-03-01"
  )
)

#P2 
a24_b_fechas %>% filter(p2_fec>="2020-03-01" & p2_fec<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-03-01"
  )
)

#b1  
a24_b_fechas %>% filter(b1_fec>="2020-03-01" & b1_fec<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-03-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-03-01" & b2_fec<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-03-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-03-01" & b4_fec<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-03-01"
  )
)

#esperados abril

#P2 
a24_b_fechas %>% filter(p2_fec>="2020-04-01" & p2_fec<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-04-01"
  )
)

#b1  
a24_b_fechas %>% filter(b1_fec>="2020-04-01" & b1_fec<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-04-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-04-01" & b2_fec<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-04-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-04-01" & b4_fec<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-04-01"
  )
)

# esperados mayo (ya no hay ps)
#b1  
a24_b_fechas %>% filter(b1_fec>="2020-05-01" & b1_fec<"2020-06-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-05-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-05-01" & b2_fec<"2020-06-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-05-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-05-01" & b4_fec<"2020-06-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-05-01"
  )
)

# esperados Junio (ya no hay ps)
#b1  
a24_b_fechas %>% filter(b1_fec>="2020-06-01" & b1_fec<"2020-07-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-06-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-06-01" & b2_fec<"2020-07-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-06-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-06-01" & b4_fec<"2020-07-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-06-01"
  )
)

# esperados Julio (ya no hay ps)
#b1  
a24_b_fechas %>% filter(b1_fec>="2020-07-01" & b1_fec<"2020-08-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-07-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-07-01" & b2_fec<"2020-08-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-07-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-07-01" & b4_fec<"2020-08-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-07-01"
  )
)

# esperados Julio (ya no hay ps)
#b1  
a24_b_fechas %>% filter(b1_fec>="2020-08-01" & b1_fec<"2020-09-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-08-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-08-01" & b2_fec<"2020-09-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-08-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-08-01" & b4_fec<"2020-09-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-08-01"
  )
)


# esperados Septiembre (ya no hay ps)
#b1  
a24_b_fechas %>% filter(b1_fec>="2020-09-01" & b1_fec<"2020-10-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-09-01"
  )
)

#b2  
a24_b_fechas %>% filter(b2_fec>="2020-09-01" & b2_fec<"2020-10-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-09-01"
  )
)

#b4  
a24_b_fechas %>% filter(b4_fec>="2020-09-01" & b4_fec<"2020-10-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o) <"2020-09-01"
  )
)

# revisar proyeccion de octubre en adelante
#B2
a24_b_fechas %>% filter(b2_fec>="2020-10-01" ) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) 
  
) %>% group_by(
  lubridate::year(b2_fec), 
  lubridate::month(b2_fec)
) %>% count()


#B4
a24_b_fechas %>% filter(b4_fec>="2020-10-01" ) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) 
  
) %>% group_by(
  lubridate::year(b4_fec), 
  lubridate::month(b4_fec)
) %>% count()


#conteos de los que se hicieron por mes
gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% select(id, visit, a24b_date) %>% 
  group_by(visit,
           lubridate::year(a24b_date),
           lubridate::month(a24b_date)
           ) %>% count() %>% writexl::write_xlsx("output/a24b_conteos2.xlsx")
#partos esperados para febrero 2020
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id,
                                                              id=s4_main_id)
         ) %>% left_join(
           gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
         ) %>% left_join(
           gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
         ) %>% filter(as.Date(e3_date_exit) < as.Date(m17_ga))


#------------------------------------------------
#HOJA DE CONTEO DE NACIMIENTOS C30
#------------------------------------------------
#cantidad de nacimientos que no se lograron 30, se tienen 660 c30
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, visit) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% filter(is.na(c30_dob))


#cantidad de hogares con promedio de peso al nacer
c30_pesos<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, visit) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob,c30_ave_wt)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, c30a_date,c30a_ave_wt)
) %>% filter(!is.na(c30_dob)) %>% mutate(
  peso_bb=if_else(is.na(c30_ave_wt), c30a_ave_wt, c30_ave_wt)
) %>%  select(id, c30_dob, peso_bb) %>% print()

#sacar los partos esperados para febrero
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% filter(
  as.Date(m17_ga)<="2020-03-01"
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-02-01"
    )
) %>% filter(is.na(e3_date_exit))

#nacimientos que ocurrieron en febrero
c30_pesos %>% filter(c30_dob<"2020-03-01") %>% print()
c30_pesos %>% filter(c30_dob<"2020-03-01") %>% filter(is.na(peso_bb))


#nacimientos que ocurrieron en marzo
c30_pesos %>% filter(c30_dob>="2020-03-01" & c30_dob<"2020-04-01") %>% print()
c30_pesos %>% filter(c30_dob>="2020-03-01" & c30_dob<"2020-04-01") %>% filter(is.na(peso_bb)) %>% print()

#nacimientos que ocurrieron en abril
c30_pesos %>% filter(c30_dob>="2020-04-01" & c30_dob<"2020-05-01") %>% print()
c30_pesos %>% filter(c30_dob>="2020-04-01" & c30_dob<"2020-05-01") %>% filter(is.na(peso_bb)) %>% print()

#nacimientos que ocurrieron en mayo
c30_pesos %>% filter(c30_dob>="2020-05-01" & c30_dob<"2020-06-01") %>% print()
c30_pesos %>% filter(c30_dob>="2020-05-01" & c30_dob<"2020-06-01") %>% filter(is.na(peso_bb)) %>% print()

#nacimientos que ocurrieron en mayo
c30_pesos %>% filter(c30_dob>="2020-06-01" & c30_dob<"2020-07-01") %>% print()
c30_pesos %>% filter(c30_dob>="2020-06-01" & c30_dob<"2020-07-01") %>% filter(is.na(peso_bb)) %>% print()

#---------------------------------------------------
#CONTEOS B10
#---------------------------------------------------+
#conteo BL madres
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% 
  select(id, visit) %>% group_by(visit) %>% count()
#Conteo BL adultas
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_to_spots=="1") %>% 
  select(id, visit) %>% group_by(visit) %>% count()
#Conteo B10 nacimientos niños
gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% filter(b10a_tc_spots=="1") %>% 
  select(id, visit) %>% group_by(visit) %>% count()

#b10 madre esperados
dt_b10 <- gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fcc=as.Date(fpp) - lubridate::days(280),
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, type, fcc, fec_nacimiento) %>% 
  mutate(
    p1_fec= as.Date(fcc) + lubridate::days(154),
    p2_fec=as.Date(fcc)+  lubridate::days(224),
    expected_b1=as.Date(fec_nacimiento) + lubridate::days(round(30.25*3)),
    expected_b2=as.Date(fec_nacimiento) + lubridate::days(round(30.25*6)),
    expected_b3=as.Date(fec_nacimiento) + lubridate::days(round(30.25*9)),
    expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
  ) 

#conteos esperados P1 Febrero madre de marzo en adelante no se esperaba P1
dt_b10 %>% filter(as.Date(expected_b2)>"2020-01-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
    filter(
      e3_date_exit < "2020-09-01"
    )
) %>% group_by(
  lubridate::year(expected_b2),
  lubridate::month(expected_b2),
  type
) %>% count() %>% print(n=Inf)

#conteos esperados P1 marzo madre
dt_b10 %>% filter(as.Date(p1_fec)>="2020-03-01" & as.Date(p1_fec)<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
    filter(
      e3_date_exit < "2020-03-01"
    )
)


#realizados en P1
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% 
  select(id, visit) %>%  anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
      filter(
        e3_date_exit < "2020-02-01"
      )
  ) %>% group_by(visit) %>%  count()

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% 
  select(id, visit, b10_date) %>% filter(as.Date(b10_date)>="2020-03-01" & as.Date(b10_date)<"2020-04-01") %>%  anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
      filter(
        e3_date_exit < "2020-03-01" 
      )
  ) %>% group_by(visit) %>%  count()


#conteos esperados P2 Febrero madre de marzo en adelante no se esperaba P1
dt_b10 %>% filter(as.Date(p2_fec)<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
    filter(
      e3_date_exit < "2020-02-01"
    )
)



#conteos esperados P2 Marzo madre 
dt_b10 %>% filter(as.Date(p2_fec)>="2020-03-01" & as.Date(p2_fec)<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
    filter(
      e3_date_exit < "2020-03-01"
    )
)

#conteos esperados P2 Abril madre 
dt_b10 %>% filter(as.Date(p2_fec)>="2020-04-01" & as.Date(p2_fec)<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% 
    filter(
      e3_date_exit < "2020-04-01"
    )
)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% select(id, visit, b10_date) %>% filter(
  as.Date(b10_date)<"2020-03-01"
) %>% filter(visit=="p2") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
   as.Date(e3_date_exit)<"2020-02-01"
  )
)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% select(id, visit, b10_date) %>% filter(
  as.Date(b10_date)>="2020-03-01" & as.Date(b10_date)<"2020-04-01"
) %>% filter(visit=="p2") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-03-01"
  )
)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_spots=="1") %>% select(id, visit, b10_date) %>% filter(
  as.Date(b10_date)>="2020-04-01" & as.Date(b10_date)<"2020-05-01"
) %>% filter(visit=="p2") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-04-01"
  )
)

#NACIMIENTOS B10a
dt_b10 %>% filter(as.Date(fec_nacimiento)<"2020-03-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-02-01"
  )
)
#marzo b10a
dt_b10 %>% filter(as.Date(fec_nacimiento)>="2020-03-01" & as.Date(fec_nacimiento)<"2020-04-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-03-01"
  )
)

#abril b10a
dt_b10 %>% filter(as.Date(fec_nacimiento)>="2020-04-01" & as.Date(fec_nacimiento)<"2020-05-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-04-01"
  )
)

#mayo b10a
dt_b10 %>% filter(as.Date(fec_nacimiento)>="2020-05-01" & as.Date(fec_nacimiento)<"2020-06-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-05-01"
  )
)

#junio b10a
dt_b10 %>% filter(as.Date(fec_nacimiento)>="2020-06-01" & as.Date(fec_nacimiento)<"2020-07-01") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(
    as.Date(e3_date_exit)<"2020-06-01"
  )
)

gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, b10a_tc_spots, visit) %>% filter(b10a_tc_spots=="1") %>% 
filter(
  as.Date(b10a_date)<"2020-03-01"
)

gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, b10a_tc_spots, visit) %>% filter(b10a_tc_spots=="1") %>% 
  filter(
    as.Date(b10a_date)>="2020-03-01"
  ) %>% group_by(
    lubridate::year(as.Date(b10a_date)), 
    lubridate::month(as.Date(b10a_date))
  ) %>% count()


#esperados B1 febrero
dt_b10 %>% filter(as.Date(expected_b1)<"2020-03-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-02-01"
  )
) 

#esperados B1 marzo
dt_b10 %>% filter(as.Date(expected_b1)>="2020-03-01" & as.Date(expected_b1)<"2020-04-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-03-01"
  )
) 

#esperados B1 abril
dt_b10 %>% filter(as.Date(expected_b1)>="2020-04-01" & as.Date(expected_b1)<"2020-05-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-04-01"
  )
) 

#esperados B1 mayo
dt_b10 %>% filter(as.Date(expected_b1)>="2020-05-01" & as.Date(expected_b1)<"2020-06-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-05-01"
  )
) 

#esperados B1 junio
dt_b10 %>% filter(as.Date(expected_b1)>="2020-06-01" & as.Date(expected_b1)<"2020-07-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-06-01"
  )
) 

#esperados B1 julio
dt_b10 %>% filter(as.Date(expected_b1)>="2020-07-01" & as.Date(expected_b1)<"2020-08-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-07-01"
  )
) 

#esperados B1 agosto
dt_b10 %>% filter(as.Date(expected_b1)>="2020-08-01" & as.Date(expected_b1)<"2020-09-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c) %>% filter(
    as.Date(e3_date_exit_c)<"2020-08-01"
  )
) 

#esperados B1 septiembre en adelante
dt_b10 %>% filter(as.Date(expected_b1)>="2020-09-01") %>% select(id, expected_b1) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) 

#B1 realizados febrero
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(visit=="b1") %>% filter(
  as.Date(b10_date)<"2020-03-01"
) 


#B4 realizados febrero
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(visit=="b4") %>% filter(
  as.Date(b10_date)<"2020-03-01"
) 

#B1  realizado marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(visit=="b1") %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% group_by(
  lubridate::year(as.Date(b10_date)),
  lubridate::month(as.Date(b10_date))
) %>% count()

#B4 realizado marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(visit=="b2") %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% group_by(
  lubridate::year(as.Date(b10_date)),
  lubridate::month(as.Date(b10_date))
) %>% count()

#B4 realizado marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(visit=="b4") %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% group_by(
  lubridate::year(as.Date(b10_date)),
  lubridate::month(as.Date(b10_date))
) %>% count()

# B2 febrero niño
dt_b10 %>% filter(as.Date(expected_b2)<"2020-03-01" ) %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-02-01")
)

# B4 febrero niño
dt_b10 %>% filter(as.Date(expected_b4)<"2020-03-01" ) %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-02-01")
)

# B2 marzo ninñ
dt_b10 %>% filter(as.Date(expected_b2)>="2020-03-01" & as.Date(expected_b2)<"2020-04-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-03-01")
)

# B4 marzo ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-03-01" & as.Date(expected_b4)<"2020-04-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-03-01")
)

# B2 abril ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-04-01" & as.Date(expected_b2)<"2020-05-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-04-01")
)

# B4 abril ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-04-01" & as.Date(expected_b4)<"2020-05-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-04-01")
)

# B2 mayo ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-05-01" & as.Date(expected_b2)<"2020-06-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-05-01")
)
# B4 mayo ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-05-01" & as.Date(expected_b4)<"2020-06-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-05-01")
)

# B2 junio ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-06-01" & as.Date(expected_b2)<"2020-07-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-06-01")
)

# B4 junio ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-06-01" & as.Date(expected_b4)<"2020-07-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-06-01")
)

# B2 julio ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-07-01" & as.Date(expected_b2)<"2020-08-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-07-01")
)
# B4 julio ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-07-01" & as.Date(expected_b4)<"2020-08-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-07-01")
)

# B2 agosto ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-08-01" & as.Date(expected_b2)<"2020-09-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-08-01")
)

# B4 agosto ninñ
dt_b10 %>% filter(as.Date(expected_b4)>="2020-08-01" & as.Date(expected_b4)<"2020-09-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% filter(as.Date(e3_date_exit_c)<"2020-08-01")
)

# B2 septiembre en adelante ninño
dt_b10 %>% filter(as.Date(expected_b2)>="2020-09-01") %>% select(id, expected_b2) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_c)
) %>% group_by(
  lubridate::year(as.Date(expected_b2)),
  lubridate::month(as.Date(expected_b2))
) %>% count()

# B4 septiembre en adelante ninño
dt_b10 %>% filter(as.Date(expected_b4)>="2020-09-01") %>% select(id, expected_b4) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_c)
) %>% group_by(
  lubridate::year(as.Date(expected_b4)),
  lubridate::month(as.Date(expected_b4))
) %>% count()

#conteo realizadas child b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots, visit) %>% filter(
  b10_tc_spots=='1'
) %>% filter(
  as.Date(b10_date)>="2020-01-01"
) %>% group_by(
  lubridate::year(as.Date(b10_date)),
  lubridate::month(as.Date(b10_date)),
  visit
) %>% count() %>% print(n=Inf)

#-------------------------
#B10 adulta
#--------------------------
dt_b10 %>% filter(type=="owa") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)

dt_b10 %>% filter(type=="owa") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% 
  filter(
    b10_to_spots=='1' & visit=="baseline"
  )

#esperadas adultas P1 febrero
dt_b10 %>% filter(type=="owa") %>%filter(
  as.Date(as.Date(p1_fec))<"2020-03-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)

#hechas p1 hasta febrero
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)<"2020-02-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p1"
  ) %>% print(n=Inf)

#esperadas adultas P1 marzo
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(p1_fec)>="2020-03-01" & as.Date(p1_fec)<"2020-04-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
)

#hechas p1 marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01" & as.Date(b10_date)<"2020-04-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p1"
  )

#esperadas adultas B1 abril
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(p1_fec)>="2020-04-01" & as.Date(p1_fec)<"2020-05-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
  )
)

#hechas p1 abril
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-04-01" & as.Date(b10_date)<"2020-05-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p1"
  )

#P2 esperadas febrero
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(p2_fec)<"2020-03-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)


#hechas p2 hechas hasta febrero
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)<"2020-03-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p2"
)

#esperadas adultas P2 marzo
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(p2_fec)>="2020-03-01" & as.Date(p2_fec)<"2020-04-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
)


#esperadas adultas P2 marzo
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(p2_fec)>="2020-03-01" & as.Date(p2_fec)<"2020-04-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
)

#hechas p2 hechas hasta marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01" & as.Date(b10_date)<"2020-04-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p2"
  )

#hechas p2 hechas hasta marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-04-01" & as.Date(b10_date)<"2020-05-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="p2"
  )

# B1 esperadas adultas Febrero
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)<"2020-03-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)

#hechas B1  hasta Febrero
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)<"2020-03-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )


# B1 esperadas adultas marzo
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-03-01" &  as.Date(expected_b1)<"2020-04-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
)

#hechas B1  hasta marzo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01" &  as.Date(b10_date)<"2020-04-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )
# B1 esperadas adultas abril
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-04-01" &  as.Date(expected_b1)<"2020-05-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
  )
)

#hechas B1  hasta abril
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-04-01" &  as.Date(b10_date)<"2020-05-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )


# B1 esperadas adultas mayo
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-05-01" &  as.Date(expected_b1)<"2020-06-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-05-01"
  )
)

#hechas B1  hasta mayo
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-05-01" &  as.Date(b10_date)<"2020-06-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )

# B1 esperadas adultas junio
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-06-01" &  as.Date(expected_b1)<"2020-07-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-06-01"
  )
)

#hechas B1  hasta junio
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-06-01" &  as.Date(b10_date)<"2020-07-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )

# B1 esperadas adultas julio
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-07-01" &  as.Date(expected_b1)<"2020-08-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-07-01"
  )
)

#hechas B1  hasta junio
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-07-01" &  as.Date(b10_date)<"2020-08-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )

# B1 esperadas adultas julio
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-08-01" &  as.Date(expected_b1)<"2020-09-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-08-01"
  )
)

#hechas B1  hasta agosto
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-08-01" &  as.Date(b10_date)<"2020-09-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )

# B1 esperadas adultas julio
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b1)>="2020-09-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-09-01"
  )
)

#hechas B1  hasta agosto
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-09-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b1"
  )

# B2 esperadas adultas hasta febrero
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)<"2020-03-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
)

#hechas B2  hasta febrero adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)<"2020-03-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )


# B2 esperadas adultas marzo b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-03-01" &  as.Date(expected_b2)<"2020-04-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
)

#hechas B2  hasta marzo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01" &  as.Date(b10_date)<"2020-04-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )


# B2 esperadas adultas abril b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-04-01" &  as.Date(expected_b2)<"2020-05-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
  )
)

#hechas B2  hasta abril adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-04-01" &  as.Date(b10_date)<"2020-05-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )

# B2 esperadas adultas mayo b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-05-01" &  as.Date(expected_b2)<"2020-06-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-05-01"
  )
)

#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-05-01" &  as.Date(b10_date)<"2020-06-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )


# B2 esperadas adultas mayo b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-06-01" &  as.Date(expected_b2)<"2020-07-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-06-01"
  )
)

#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-06-01" &  as.Date(b10_date)<"2020-07-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )

# B2 esperadas adultas julio   b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-07-01" &  as.Date(expected_b2)<"2020-08-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-07-01"
  )
)

#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-07-01" &  as.Date(b10_date)<"2020-08-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )
# B2 esperadas adultas agosto   b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-08-01" &  as.Date(expected_b2)<"2020-09-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-08-01"
  )
)

#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-08-01" &  as.Date(b10_date)<"2020-09-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )

# B2 esperadas adultas septiembre en adelante   b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b2)>="2020-09-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-09-01"
  )
) %>% group_by(
  lubridate::year(expected_b2),
  lubridate::month(expected_b2),
  visit
) %>% count()

#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-08-01" &  as.Date(b10_date)<"2020-09-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b2"
  )

#-----------B4
# B4 esperadas adultas hasta febrero b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)<"2020-03-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-02-01"
  )
) 
#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)<"2020-03-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b4"
  )

# B4 esperadas adultas hasta marzo b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-03-01" &   as.Date(expected_b4)<"2020-04-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-03-01"
  )
) 
#hechas B2  hasta mayo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01" &  as.Date(b10_date)<"2020-04-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b4"
  )

# B4 esperadas adultas hasta abril b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-04-01" &   as.Date(expected_b4)<"2020-05-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
  )
) 
#hechas B2  abrilo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-04-01" &  as.Date(b10_date)<"2020-05-01" 
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b4"
  )

# B4 esperadas adultas hasta abril b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-04-01" &   as.Date(expected_b4)<"2020-05-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-04-01"
  )
) 
#hechas B2  abrilo adultas b10
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_to_spots, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% 
  filter(
    b10_to_spots=='1' & visit=="b4"
  ) %>% group_by(
    lubridate::year(b10_date),
    lubridate::month(b10_date)
  ) %>% count()


# B4 esperadas adultas hasta mayo b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-05-01" &   as.Date(expected_b4)<"2020-06-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-05-01"
  )
) 

# B4 esperadas adultas hasta junio b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-06-01" &   as.Date(expected_b4)<"2020-07-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-06-01"
  )
) 

# B4 esperadas adultas hasta julio b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-07-01" &   as.Date(expected_b4)<"2020-08-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-07-01"
  )
) 

# B4 esperadas adultas hasta agosto b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-08-01" &   as.Date(expected_b4)<"2020-09-01" 
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-08-01"
  )
) 
# B4 esperadas adultas hasta agosto b10
dt_b10 %>% filter(type=="owa") %>%   filter(
  as.Date(expected_b4)>="2020-09-01"
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(
    as.Date(e3_date_exit_o)<"2020-09-01"
  )
) %>% group_by(
  lubridate::year(expected_b4),
  lubridate::month(expected_b4)
) %>% count()



###---------------Biomarcadores Orina
# B4 esperadas adultas hasta agosto b10
dt_b10 %>%     filter(
  as.Date(p1_fec)<"2020-03-01"
) %>% left_join(
  gt_emory_data_arm2 %>%  filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(as.Date(e3_date_exit)<"2020-02-01")
) %>% mutate(
  flag=case_when(
    is.na(e3_date_exit) ~ "1",
    as.Date(e3_date_exit) < as.Date(p1_fec) ~ "1",
    TRUE ~ "0"
  )
) %>% group_by(flag) %>% count()

dt_b10 %>%     filter(
  as.Date(p2_fec)<"2020-03-01"
) %>% left_join(
  gt_emory_data_arm2 %>%  filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(as.Date(e3_date_exit)<"2020-02-01")
) %>% mutate(
  flag=case_when(
    is.na(e3_date_exit) ~ "1",
    as.Date(e3_date_exit) < as.Date(p2_fec) ~ "1",
    TRUE ~ "0"
  )
) %>% group_by(flag) %>% count()


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_m_urine, visit) %>% filter(
  as.Date(b10_date)<"2020-03-01"
) %>% 
  filter(
    b10_m_urine=='1' & visit=="p1"
  )

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id,b10_date, b10_tc_urine, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% 
  filter(
    b10_tc_urine=='1' & visit=="b4"
  ) %>% group_by(
    lubridate::year(b10_date),
    lubridate::month(b10_date),
    visit

  ) %>% count()

dt_b10 %>% filter(as.Date(expected_b1) >"2020-03-01") %>% group_by(
  lubridate::year(expected_b1),
  lubridate::month(expected_b1),
) %>% count()

#adultas realizadas b10 BMC
gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>%  select(id,b10_date, b10_to_urine, visit) %>% filter(
  as.Date(b10_date)>="2020-03-01"
) %>% 
  filter(
    b10_to_urine=='1' 
  ) %>% group_by(
    lubridate::year(b10_date),
    lubridate::month(b10_date),
    visit
    
  ) %>% count()

#---------------------------------------------------
#EXPOSICION H41
#---------------------------------------------------

#h41  esperados
dt_h41 <- gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(
  type=if_else(
    grepl("^35[0-9]",id),"owa","pwg"
  )
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fcc=as.Date(fpp) - lubridate::days(280),
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, type, fcc, fec_nacimiento) %>% 
  mutate(
    p1_fec= as.Date(fcc) + lubridate::days(154),
    p2_fec=as.Date(fcc)+  lubridate::days(224),
    expected_b1=as.Date(fec_nacimiento) + lubridate::days(round(30.25*3)),
    expected_b2=as.Date(fec_nacimiento) + lubridate::days(round(30.25*6)),
    expected_b3=as.Date(fec_nacimiento) + lubridate::days(round(30.25*9)),
    expected_b4=as.Date(fec_nacimiento) + lubridate::days(round(30.25*12))
  ) 

#Esperadas linea basal
#esperadas hasta febrero, descontando las salidas no hay p1 en marzo 2020
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(p1_fec)<"2020-03-01") %>% mutate(
  flag_p1=case_when(
   as.Date(e3_date_exit)<as.Date(p1_fec) ~ "0" ,
   TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#P2 hasta febrero
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(p2_fec)<"2020-03-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(p2_fec) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#P2 hasta marzo
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(p2_fec)>="2020-03-01" & as.Date(p2_fec)<"2020-04-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(p2_fec) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#P2 hasta abril
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(p2_fec)>="2020-04-01" & as.Date(p2_fec)<"2020-05-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(p2_fec) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#P2 hasta mayo
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(p2_fec)>="2020-04-01" & as.Date(p2_fec)<"2020-05-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(p2_fec) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta febrero
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)<"2020-03-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta marzo
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-03-01" & as.Date(expected_b1)<"2020-04-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta abril
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-04-01" & as.Date(expected_b1)<"2020-05-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()


#b1 hasta mayo
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-05-01" & as.Date(expected_b1)<"2020-06-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()


#b1 hasta junio
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-06-01" & as.Date(expected_b1)<"2020-07-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()


#b1 hasta junio
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-07-01" & as.Date(expected_b1)<"2020-08-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta julio
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-07-01" & as.Date(expected_b1)<"2020-08-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta agosto
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-08-01" & as.Date(expected_b1)<"2020-09-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% group_by(flag_p1) %>% count()

#b1 hasta agosto
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b1)>="2020-09-01") %>% mutate(
  flag_p1=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b1) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag_p1=="1") %>% count()

#-------B2
#b2 hasta feb
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)<"2020-03-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b4 hasta feb
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b4)<"2020-03-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b4) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta marzo
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-03-01" & as.Date(expected_b2)<"2020-04-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta abril
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-04-01" & as.Date(expected_b2)<"2020-05-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta abril
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-05-01" & as.Date(expected_b2)<"2020-06-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta junio 
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-06-01" & as.Date(expected_b2)<"2020-07-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta julio 
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-07-01" & as.Date(expected_b2)<"2020-08-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta julio 
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-08-01" & as.Date(expected_b2)<"2020-09-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% count()

#b2 hasta sep 
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b2)>"2020-09-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b2) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% group_by(
  lubridate::year(expected_b2),
  lubridate::month(expected_b2)
) %>%  count()

#b2 hasta marzo 
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
) %>% filter(as.Date(expected_b)>"2020-03-01") %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b4) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% group_by(
  lubridate::year(expected_b4),
  lubridate::month(expected_b4)
  ) %>% count()

#revision h41 madre
gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_person___1 ) %>% filter(
  h41_person___1=="1"
) %>% filter(visit=="b4") %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date)
) %>% count()

#revision h41 Adulta
gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_person___2 ) %>% filter(
  h41_person___2=="1"
) %>% filter(visit=="b4") %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date)
) %>% count()

#----------Adultas
#LB hasta febrero 
dt_h41 %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit_o)
)  %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit_o)<as.Date(expected_b4) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% group_by(
  lubridate::year(expected_b4),
  lubridate::month(expected_b4)
) %>% count()

#-------------cuantos h41 madre se han hecho
gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_person___2 ) %>% filter(
  h41_person___2=="1"
)  %>%filter(h41_date>"2020-01-01") %>%   group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date),
  visit
) %>% count() %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_person___1 ) %>% filter(
  h41_person___1=="1"
) %>% filter(visit=="b4") %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date),
  visit
) %>% count() %>% filter(`lubridate::year(h41_date)`>2019)

#revisar conteo B2
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
)  %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b3) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% group_by(
  lubridate::year(expected_b3),
  lubridate::month(expected_b3)
) %>% count()

#revisar conteo B3
dt_h41 %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit)
)  %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)<as.Date(expected_b3) ~ "0" ,
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% group_by(
  lubridate::year(expected_b3),
  lubridate::month(expected_b3)
) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_person___2) %>% filter(h41_date>"2020-08-01") %>% filter(
  h41_person___2=="1"
) %>% 
  group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date),
  visit
) %>% count()


#-------------------------
# CONTEOS INTENSIVO EXPOSICIÓN
#-------------------------

#identificar las fechas de visitas intensivo
dt_visitas<-dt_h41 %>%left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit) %>% filter(visit=="baseline")
) %>%  mutate(
  dias_blp1=as.Date(p1_fec) - as.Date(h41_date),
  blp1=as.Date(h41_date)+ lubridate::days(round(dias_blp1/2)),
  p1p2=as.Date(p1_fec)+round((as.Date(p2_fec) - as.Date(p1_fec))/2),
  p2b1=as.Date(p2_fec)+round((as.Date(expected_b1) - as.Date(p2_fec))/2),
  b1b2=as.Date(expected_b1)+round((as.Date(expected_b2) - as.Date(expected_b1))/2),
  b2b3=as.Date(expected_b2)+round((as.Date(expected_b3) - as.Date(expected_b2))/2),
  b3b4=as.Date(expected_b3)+round((as.Date(expected_b4) - as.Date(expected_b3))/2),
) %>% select(-h41_date, -dias_blp1)

#leer datos de intensivo
data_intensivo<- read_csv("data/exports/HAPINGuatemalaExposu_DATA_2020-10-28_1538.csv")
intensivo<-data_intensivo %>% mutate_all(as.character) %>%  select(id=record_id, redcap_event_name, h41_date) %>%  mutate(
  visit= substr(redcap_event_name,1,4)
) 

#set de datos participantes en intensivo
dt_intensivo<-intensivo %>% filter(visit=="blp1") %>% left_join(
  dt_visitas %>% select(id, type,blp1,p1p2,p2b1,b1b2,b2b3,b3b4)
)

#Conteo esperados hasta febrero
dt_intensivo %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(
    id, e3_date_exit
  )
) %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit)< as.Date(b3b4) ~ "0",
    TRUE ~ "1"
  )
) %>% filter(flag=="1") %>% 
  group_by(
  lubridate::year(as.Date(b3b4)),
  lubridate::month(as.Date(b3b4))
) %>% count()
  

intensivo %>% filter(
  visit=="b1b2"
) %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date)
) %>% count()

intensivo %>% filter(
  visit=="b2b3"
) %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date)
) %>% count()

intensivo %>% filter(
  visit=="b3b4"
) %>% group_by(
  lubridate::year(h41_date),
  lubridate::month(h41_date)
) %>% count()

intensivo %>% filter(
  visit=="b2b3"
) %>% anti_join(
  salidas %>% select(id)
)


##REVISAR DATOS DE SALIDAS
gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o, e3_reason_o) %>% filter(
  e3_date_exit_o<"2020-09-01"
  ) %>% group_by(e3_reason_o) %>% count()

dt_h41 %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o, e3_reason_o)
) %>% mutate(
  flag=case_when(
    as.Date(e3_date_exit_o)< as.Date(p2_fec) ~ "1",
    TRUE ~ "0"
  )
) %>% filter(flag=="0") %>%  group_by(
  lubridate::year(p2_fec),
  #lubridate::month(p1_fec)
) %>% count()

dt_h41 %>% filter(
  id=="33228" |   id=="33195" |   id=="33174" | id=="33259" |  id=="33454" 
)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_o_col_time_2, visit) %>% filter(
  b10_date>"2020-03-01"
  ) %>% arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_to_spots_time_2, visit) %>%
  filter( !is.na(b10_to_spots_time_2)
  ) %>% arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_c_time, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_c_time)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_spots_time, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_tc_spots_time)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tm_spots_time, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_tm_spots_time)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, b10a_tc_spots_time, visit) %>% filter(
  b10a_date>"2020-03-01"
) %>% filter(!is.na(b10a_tc_spots_time)) %>% 
  arrange(b10a_date) %>% print(n=Inf)


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_to_spots_time, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_to_spots_time)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_to_hba1c, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_to_hba1c)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_venous_time, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_venous_time)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_buccal, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_buccal)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_oral, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_oral)) %>% 
  arrange(b10_date) %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_nasal, visit) %>% filter(
  b10_date>"2020-03-01"
) %>% filter(!is.na(b10_nasal)) %>% 
  arrange(b10_date) %>% print(n=Inf)

# b10_o_col_time_2 para orina de adultas
# b10_to_spots_time_2 para sangre seca de adultos
# b10_c_time sangre de bb
# b10_tc_spots_time orina niño
# 
# madre: b10_tm_time

gt_emory_data_arm2 %>% filter(!is.na(a23_date)) %>% select(id, a23_date, visit) %>% group_by(visit) %>% count()
gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, a26_date, visit) %>% group_by(visit) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(a23_date)) %>% select(id, a23_date, visit) %>% filter(visit=="baseline") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, a26_date, visit) %>% filter(visit=="baseline") %>% 
    select(id)
) %>% write_csv("output/participantes_sin_CIMT_LB.csv")

gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id, m10_by, m10_date) %>% filter(as.Date(m10_date)=="2019-04-12")

#duplicados intensivo
data_intensivo %>% select(record_id, h41b_envir1,h41b_type1, redcap_event_name) %>% filter(!is.na(h41b_envir1)) %>% mutate(
  visit=substr(redcap_event_name,1,4)
) %>% filter(h41b_envir1=="7")

  group_by(visit,h41b_envir1,h41b_type1) %>% count() %>% write_csv("output/conteo_intensivo_amb1.csv")

data_intensivo %>% select(record_id, h41b_envir2, h41b_type2, redcap_event_name) %>% filter(!is.na(h41b_envir2)) %>% mutate(
  visit=substr(redcap_event_name,1,4)
) %>% print(n=Inf) %>% 
  group_by(visit, h41b_envir2,h41b_type2) %>% count()%>% filter(
  h41b_envir2
)
  
  write_csv("output/conteo_intensivo_amb2.csv")

h41b<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2020-11-05_1318.csv")


h41b %>% filter(!is.na(h41b_date)) %>% select(id, h41b_envir1, h41b_type1, redcap_event_name) %>% 
  filter(!is.na(h41b_envir1)) %>% mutate(
    visit=substr(redcap_event_name,1,2)
  ) %>% #33267 es NA 
  filter(
    h41b_envir1 =="9" & h41b_type1=="4"
  )
  group_by(visit,h41b_envir1,h41b_type1) %>% count() %>% write_csv("output/conteo_duplicados_principal.csv")


h41b %>% filter(!is.na(h41b_date)) %>% select(id, h41b_envir2,h41b_type2, redcap_event_name) %>%
  filter(!is.na(h41b_envir2)) %>% 
  group_by(redcap_event_name,h41b_envir2,h41b_type2) %>% count()

#edades de los niños b2b3 que aun estan en el estudio
data_intensivo<-data_intensivo %>% mutate_all(as.character)

data_intensivo  %>% 
  group_by(
  redcap_event_name
) %>% count()
