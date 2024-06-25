library(tidyverse)

data_lab<-read_csv("data/exports/HAPINGuatemalaLab_DATA_2023-05-03_1006.csv")

#Sacar todas las muestras de orina
all_urine<-data_lab %>% filter(!is.na(aliquot_container_id)) %>% select(aliquot_container_id,
                                                                        matches("aliquot_id_"),
                                                                        matches("aliquot_volume_"))

all_urine<-all_urine %>% mutate_all(as.character)
#sacar matriz de muestras de orina
td_urine<-all_urine %>% select(aliquot_container_id,
                               Sample_ID=aliquot_id_001,
                               volumen=aliquot_volume_001
) %>% filter(!is.na(
  Sample_ID
)) %>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_002,
                         volumen=aliquot_volume_002
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  ) %>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_003,
                         volumen=aliquot_volume_003
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_004,
                         volumen=aliquot_volume_004
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_005,
                         volumen=aliquot_volume_005
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_006,
                         volumen=aliquot_volume_006
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_007,
                         volumen=aliquot_volume_007
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_008,
                         volumen=aliquot_volume_008
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  ) %>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_010,
                         volumen=aliquot_volume_010
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_011,
                         volumen=aliquot_volume_011
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_012,
                         volumen=aliquot_volume_012
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_013,
                         volumen=aliquot_volume_013
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_014,
                         volumen=aliquot_volume_014
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_015,
                         volumen=aliquot_volume_015
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_016,
                         volumen=aliquot_volume_017
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_018,
                         volumen=aliquot_volume_018
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_019,
                         volumen=aliquot_volume_019
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_020,
                         volumen=aliquot_volume_020
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_021,
                         volumen=aliquot_volume_021
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_022,
                         volumen=aliquot_volume_022
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_023,
                         volumen=aliquot_volume_023
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_024,
                         volumen=aliquot_volume_024
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_025,
                         volumen=aliquot_volume_025
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_026,
                         volumen=aliquot_volume_026
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_027,
                         volumen=aliquot_volume_027
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_028,
                         volumen=aliquot_volume_028
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_029,
                         volumen=aliquot_volume_029
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_030,
                         volumen=aliquot_volume_030
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_031,
                         volumen=aliquot_volume_031
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_032,
                         volumen=aliquot_volume_032
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_033,
                         volumen=aliquot_volume_033
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_034,
                         volumen=aliquot_volume_034
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_035,
                         volumen=aliquot_volume_035
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_036,
                         volumen=aliquot_volume_036
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_037,
                         volumen=aliquot_volume_037
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_038,
                         volumen=aliquot_volume_038
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_039,
                         volumen=aliquot_volume_039
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_040,
                         volumen=aliquot_volume_040
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_041,
                         volumen=aliquot_volume_041
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_042,
                         volumen=aliquot_volume_042
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_043,
                         volumen=aliquot_volume_043
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_044,
                         volumen=aliquot_volume_044
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_045,
                         volumen=aliquot_volume_045
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_046,
                         volumen=aliquot_volume_046
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_047,
                         volumen=aliquot_volume_047
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_048,
                         volumen=aliquot_volume_048
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_049,
                         volumen=aliquot_volume_049
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_050,
                         volumen=aliquot_volume_050
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_051,
                         volumen=aliquot_volume_051
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_052,
                         volumen=aliquot_volume_052
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_053,
                         volumen=aliquot_volume_053
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_054,
                         volumen=aliquot_volume_054
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_055,
                         volumen=aliquot_volume_055
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_056,
                         volumen=aliquot_volume_056
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_057,
                         volumen=aliquot_volume_057
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_058,
                         volumen=aliquot_volume_058
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_059,
                         volumen=aliquot_volume_059
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_060,
                         volumen=aliquot_volume_060
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_061,
                         volumen=aliquot_volume_061
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_062,
                         volumen=aliquot_volume_062
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_063,
                         volumen=aliquot_volume_063
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_064,
                         volumen=aliquot_volume_064
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_065,
                         volumen=aliquot_volume_065
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_066,
                         volumen=aliquot_volume_066
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_067,
                         volumen=aliquot_volume_067
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_068,
                         volumen=aliquot_volume_068
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_069,
                         volumen=aliquot_volume_069
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_070,
                         volumen=aliquot_volume_070
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_071,
                         volumen=aliquot_volume_071
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_072,
                         volumen=aliquot_volume_072
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_073,
                         volumen=aliquot_volume_073
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_074,
                         volumen=aliquot_volume_074
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_075,
                         volumen=aliquot_volume_075
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_076,
                         volumen=aliquot_volume_076
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_077,
                         volumen=aliquot_volume_077
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_078,
                         volumen=aliquot_volume_078
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_079,
                         volumen=aliquot_volume_079
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_080,
                         volumen=aliquot_volume_080
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )%>% 
  bind_rows(
    all_urine %>% select(aliquot_container_id,
                         Sample_ID=aliquot_id_081,
                         volumen=aliquot_volume_081
    ) %>% filter(!is.na(
      Sample_ID
    )) 
  )

#leer datos del inventario de Maria Rene
#listados inventario Maria Rene sangre y orina
listado_orina<-read_csv("c:/temp/lista_orina_envio.csv")


listado_orina %>% anti_join(
  td_urine %>% group_by(aliquot_container_id) %>% transmute(box_id=aliquot_container_id)
) #al parecer orina si cuadra todo para sacar manifiesto

#Orina
manifiesto_orina<-td_urine %>% left_join(
  listado_orina %>% transmute(aliquot_container_id=box_id, envio="Si") 
) %>% filter(!is.na(envio)) %>% #group_by(card_container_id) %>% count()
  transmute(`Sample ID`=Sample_ID, MATRIX="Urine",
            `VOL (mL)`=paste0(volumen),
            `Package #`=aliquot_container_id,`BOX POSITION`=NA ) 

#Orina
td_urine%>% transmute(`Sample ID`=Sample_ID, MATRIX="Urine",
                      `VOL (mL)`=paste0(volumen),
                      `Package #`=aliquot_container_id,`BOX POSITION`=NA ) %>% mutate(
                        flag=substr(`Sample ID`,10,10)
                      ) %>% filter(flag=="U")%>% left_join(
                        listado_orina %>% transmute(`Package #`=box_id, envio="Si") 
                      ) %>% filter(!is.na(envio)) %>% writexl::write_xlsx(
                        "output/manifiesto_orina_hapin_05-05-2023.xlsx"
                      )


