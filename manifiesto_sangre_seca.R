library(tidyverse)

data_lab<-read_csv("data/exports/HAPINGuatemalaLab_DATA_2022-07-28_1153.csv")

all_dbs<-data_lab %>% filter(redcap_event_name=="registro_arm_2") %>% select(
  card_container_id, matches("card_id_"),
  matches("blood_spots_valid_"),
  matches("blood_spots_invalid_")
) %>% filter(!is.na(card_container_id))

td_dbs<-all_dbs %>% select(card_container_id,
                   Sample_ID=card_id_001,
                   validas=blood_spots_valid_001,
                   invalidas=blood_spots_invalid_001) %>% filter(!is.na(
                     Sample_ID
                   )) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_002,
                                        validas=blood_spots_valid_002,
                                        invalidas=blood_spots_invalid_002) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_003,
                                        validas=blood_spots_valid_003,
                                        invalidas=blood_spots_invalid_003) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_004,
                                        validas=blood_spots_valid_004,
                                        invalidas=blood_spots_invalid_004) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_005,
                                        validas=blood_spots_valid_005,
                                        invalidas=blood_spots_invalid_005) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_006,
                                        validas=blood_spots_valid_006,
                                        invalidas=blood_spots_invalid_006) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_007,
                                        validas=blood_spots_valid_007,
                                        invalidas=blood_spots_invalid_007) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_008,
                                        validas=blood_spots_valid_008,
                                        invalidas=blood_spots_invalid_008) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_009,
                                        validas=blood_spots_valid_009,
                                        invalidas=blood_spots_invalid_009) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_010,
                                        validas=blood_spots_valid_010,
                                        invalidas=blood_spots_invalid_010) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )  %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_011,
                                        validas=blood_spots_valid_011,
                                        invalidas=blood_spots_invalid_011) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_012,
                                        validas=blood_spots_valid_012,
                                        invalidas=blood_spots_invalid_012) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_013,
                                        validas=blood_spots_valid_013,
                                        invalidas=blood_spots_invalid_013) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_014,
                                        validas=blood_spots_valid_014,
                                        invalidas=blood_spots_invalid_014) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_015,
                                        validas=blood_spots_valid_015,
                                        invalidas=blood_spots_invalid_015) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )  %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_016,
                                        validas=blood_spots_valid_016,
                                        invalidas=blood_spots_invalid_016) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_017,
                                        validas=blood_spots_valid_017,
                                        invalidas=blood_spots_invalid_017) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_018,
                                        validas=blood_spots_valid_018,
                                        invalidas=blood_spots_invalid_018) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_019,
                                        validas=blood_spots_valid_019,
                                        invalidas=blood_spots_invalid_019) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_020,
                                        validas=blood_spots_valid_020,
                                        invalidas=blood_spots_invalid_020) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_021,
                                        validas=blood_spots_valid_021,
                                        invalidas=blood_spots_invalid_021) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_022,
                                        validas=blood_spots_valid_022,
                                        invalidas=blood_spots_invalid_022) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_023,
                                        validas=blood_spots_valid_023,
                                        invalidas=blood_spots_invalid_023) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_024,
                                        validas=blood_spots_valid_024,
                                        invalidas=blood_spots_invalid_024) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_025,
                                        validas=blood_spots_valid_025,
                                        invalidas=blood_spots_invalid_025) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_026,
                                        validas=blood_spots_valid_026,
                                        invalidas=blood_spots_invalid_026) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_027,
                                        validas=blood_spots_valid_027,
                                        invalidas=blood_spots_invalid_027) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_028,
                                        validas=blood_spots_valid_028,
                                        invalidas=blood_spots_invalid_028) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_029,
                                        validas=blood_spots_valid_029,
                                        invalidas=blood_spots_invalid_029) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_030,
                                        validas=blood_spots_valid_030,
                                        invalidas=blood_spots_invalid_030) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_031,
                                        validas=blood_spots_valid_031,
                                        invalidas=blood_spots_invalid_031) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   ) %>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_032,
                                        validas=blood_spots_valid_032,
                                        invalidas=blood_spots_invalid_032) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )%>% bind_rows(
                     all_dbs %>% select(card_container_id,
                                        Sample_ID=card_id_033,
                                        validas=blood_spots_valid_033,
                                        invalidas=blood_spots_invalid_033) %>% filter(!is.na(
                                          Sample_ID
                                        )) 
                   )  %>% bind_rows(
                    all_dbs %>% select(card_container_id,
                                       Sample_ID=card_id_034,
                                       validas=blood_spots_valid_034,
                                       invalidas=blood_spots_invalid_034) %>% filter(!is.na(
                                         Sample_ID
                                       ))
                  ) 
# %>% bind_rows(
#                      all_dbs %>% select(card_container_id,
#                                         Sample_ID=card_id_035,
#                                         validas=blood_spots_valid_035,
#                                         invalidas=blood_spots_invalid_035) %>% filter(!is.na(
#                                           Sample_ID
#                                         ))
#                    )
#                   %>% bind_rows(
#                      all_dbs %>% select(card_container_id,
#                                         Sample_ID=card_id_036,
#                                         validas=blood_spots_valid_036,
#                                         invalidas=blood_spots_invalid_036) %>% filter(!is.na(
#                                           Sample_ID
#                                         ))
#                    ) 
#%>%  bind_rows(
                #    #   all_dbs %>% select(card_container_id,
                #    #                      Sample_ID=card_id_037,
                #    #                      validas=blood_spots_valid_037,
                #    #                      invalidas=blood_spots_invalid_037) %>% filter(!is.na(
                #    #                        Sample_ID
                #    #                      )) 
                #    # ) %>% bind_rows(
                   #   all_dbs %>% select(card_container_id,
                   #                      Sample_ID=card_id_038,
                   #                      validas=blood_spots_valid_038,
                   #                      invalidas=blood_spots_invalid_038) %>% filter(!is.na(
                   #                        Sample_ID
                   #                      )) 
                   # ) %>% bind_rows(
                   #   all_dbs %>% select(card_container_id,
                   #                      Sample_ID=card_id_039,
                   #                      validas=blood_spots_valid_039,
                   #                      invalidas=blood_spots_invalid_039) %>% filter(!is.na(
                   #                        Sample_ID
                   #                      )) 
                   # ) %>% bind_rows(
                   #   all_dbs %>% select(card_container_id,
                   #                      Sample_ID=card_id_040,
                   #                      validas=blood_spots_valid_040,
                   #                      invalidas=blood_spots_invalid_040) %>% filter(!is.na(
                   #                        Sample_ID
                   #                      )) 
                   # ) %>% bind_rows(
                   #   all_dbs %>% select(card_container_id,
                   #                      Sample_ID=card_id_041,
                   #                      validas=blood_spots_valid_041,
                   #                      invalidas=blood_spots_invalid_041) %>% filter(!is.na(
                   #                        Sample_ID
                   #                      )) 
                   # ) %>% bind_rows(
                   #   all_dbs %>% select(card_container_id,
                   #                      Sample_ID=card_id_042,
                   #                      validas=blood_spots_valid_042,
                   #                      invalidas=blood_spots_invalid_042) %>% filter(!is.na(
                   #                        Sample_ID
                   #                      )) 
                   #  )

#Sacar todas las muestras de orina
all_urine<-data_lab %>% filter(!is.na(aliquot_container_id)) %>% select(aliquot_container_id,
                                                             matches("aliquot_id_"),
                                                             matches("aliquot_volume_"))

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

all_urine %>% gather(key = "variable", value = "value", -aliquot_container_id) %>% 
  filter(!is.na(value)) %>% mutate(
    id=substr(value,1,5),
    visita_tipo=substr(value,7,8)
  ) %>% group_by(visita_tipo) %>% count() %>% arrange(visita_tipo)


all_dbs %>% gather(key = "variable", value = "value", -card_container_id) %>% 
  filter(!is.na(value)) %>% mutate(
    id=substr(value,1,5),
    visita_tipo=substr(value,7,8)
  ) %>% group_by(visita_tipo) %>% count() %>% arrange(visita_tipo)

#leer datos del inventario de Maria Rene
#listados inventario Maria Rene sangre y orina
listado_dbs<-read_csv("output/old/listado_maria_rene.csv", )
listado_orina<-read_csv("c:/temp/lista_orina_envio.csv")
listado_sars<-read_csv("c:/temp/listado_sars.csv")
listado_otras<-read_csv("c:/temp/listado_otras.csv")

#pendientes_ingresar<-read_csv("c:/temp/muestras_faltantes.csv")
# pendientes_ingresar %>% left_join(
# gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
#   select(id, b10_date, b10_by,
#         visit,  b10_tc_spots_num, id_muestra=b10_tc_b_code)
# ) %>% writexl::write_xlsx("output/pendientes_ingresar_dbs.xlsx")

#revisar 
listado_dbs %>% anti_join(
  td_dbs %>% group_by(card_container_id) %>% transmute(id_bag=card_container_id)
) #aun falta ingresar algunos contenedores de sangre seca

listado_orina %>% anti_join(
  td_urine %>% group_by(aliquot_container_id) %>% transmute(box_id=aliquot_container_id)
) #al parecer orina si cuadra todo para sacar manifiesto

listado_sars %>% select(id_bag) %>% anti_join(
  td_dbs %>% group_by(card_container_id) %>% transmute(id_bag=card_container_id)
)

listado_otras %>% select(box_id) %>% anti_join(
  td_urine %>% group_by(aliquot_container_id) %>% transmute(box_id=aliquot_container_id)
)
  
# data_lab %>% group_by(card_container_id) %>% count()
# data_lab %>% filter(card_container_id=="33306-C4-X0")

# tmp_envios<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2021-05-19_1621.csv")
# tmp_envios %>% filter(redcap_event_name=="envio_arm_3") %>% gather(
#   key = "variable", value = "value", -correlativo
# ) %>% filter(value=="Bag-0378")
# 
# td_dbs %>% group_by(card_container_id) %>% count() %>% arrange(desc(n)) %>% print(n=120)

#listado sars cov 2 que estan en Jalapa
manifiesto_dbs1<-td_dbs %>% left_join(
  listado_sars %>% transmute(card_container_id=id_bag, envio="Si")
) %>% filter(!is.na(envio)) %>%  transmute(`Sample ID`=Sample_ID, MATRIX="DBS",
                                           `VOL (mL)`=paste0("5 total, ",validas," valid"),
                                           `Package #`=card_container_id,  `BOX POSITION`=NA )
# %>% 
#   writexl::write_xlsx("output/muestras_dbs_sarsCov.xlsx")

#armar manifiesto:
#Sangre seca
manifiesto_dbs2<-td_dbs %>% left_join(
  listado_dbs %>% transmute(card_container_id=id_bag, envio="Si") 
) %>% filter(!is.na(envio)) %>% #group_by(card_container_id) %>% count()
 transmute(`Sample ID`=Sample_ID, MATRIX="DBS",
           `VOL (mL)`=paste0("5 total, ",validas," valid"),
            `Package #`=card_container_id,  `BOX POSITION`=NA ) 
# %>%
#   writexl::write_xlsx("output/manifiesta_mayo_2021.xlsx")

#Orina
manifiesto_orina<-td_urine %>% left_join(
  listado_orina %>% transmute(aliquot_container_id=box_id, envio="Si") 
) %>% filter(!is.na(envio)) %>% #group_by(card_container_id) %>% count()
  transmute(`Sample ID`=Sample_ID, MATRIX="Urine",
            `VOL (mL)`=paste0(volumen),
            `Package #`=aliquot_container_id,`BOX POSITION`=NA ) 

#Orina
manifiesto_otras<-td_urine %>% left_join(
  listado_otras %>% transmute(aliquot_container_id=box_id, envio="Si", MATRIX=tipo) 
) %>% filter(!is.na(envio)) %>% #group_by(card_container_id) %>% count()
  transmute(`Sample ID`=Sample_ID, MATRIX,
            `VOL (mL)`=paste0(volumen),
            `Package #`=aliquot_container_id,`BOX POSITION`=NA ) 

manifiesto_dbs2 %>% arrange(`Package #`) %>% bind_rows(
  manifiesto_dbs2 %>% arrange(`Package #`)
) %>% 
  bind_rows(
  manifiesto_orina %>% arrange(`Package #`)
) %>% bind_rows(
  manifiesto_otras %>% arrange(`Package #`)
) %>%   writexl::write_xlsx("output/manifiesto_integrado.xlsx")



#leer manifiesto
manifiesto_integrado<-readxl::read_xlsx(path = "c:/temp/manifiesto_integrado_muestras.xlsx")

#listado de DBS laboratorio Jalapa solicitado por Anaite, muestras covid colectadas y fecha de envío a emory
td_dbs
gt_emory_data_arm2 %>% filter(b10_tm_spots=="1") %>% select(
  id, visit,b10_date, Sample_ID=b10_tm_b_code, validas_segun_redcap=b10_tm_spots_num
) %>% filter(visit=="b4") %>% arrange(b10_date) %>% left_join(
  td_dbs
) %>% mutate(
  flag1=if_else(
    validas_segun_redcap!=validas,1,0
  )
) 
#%>% writexl::write_xlsx("output/revision_dbs_covid_10-02-2022.xlsx")

#listado de muestras b5
#listado de DBS laboratorio Jalapa solicitado por Anaite, muestras covid colectadas y fecha de envío a emory
  td_dbs %>%  transmute(
  `Sample ID`=Sample_ID, MATRIX="DBS",
                      `VOL (mL)`=paste0("5 total, ",validas," valid"),
                      `Package #`=card_container_id,  `BOX POSITION`=NA ) %>% mutate(
                        visita=substr(`Sample ID`,7,8),
                        id=substr(`Sample ID`,1,5)
                      ) %>% filter(visita=="C7") %>% left_join(
                        gt_hapin_II_data %>% filter(b10_tc_spots=="1") %>% select(
                          id, visit,b10_date, b10_tc_b_code, validas_segun_redcap=b10_tc_spots_num
                        ) %>% filter(visit=="b5") %>% arrange(b10_date)
                      )  %>% writexl::write_xlsx("output/manifiesto_db_24m.xlsx")

gt_hapin_II_data %>% filter(b10_tc_spots=="1") %>% select(
  id, visit,b10_date, Sample_ID=b10_tc_b_code, validas_segun_redcap=b10_tc_spots_num
) %>% filter(visit=="b5") %>% arrange(b10_date) %>% left_join(
  td_dbs
) %>% mutate(
  flag1=if_else(
    validas_segun_redcap!=validas,1,0
  )
) %>% writexl::write_xlsx("output/manifiesto_b5.xlsx")

#manifiesto muestras de orina
td_urine %>% left_join(
  gt_hapin_II_data %>% filter()
)
  transmute(`Sample ID`=Sample_ID, MATRIX="Urine",
                       `VOL (mL)`=paste0(volumen),
                       `Package #`=aliquot_container_id,`BOX POSITION`=NA ) 
  
  
  #mutate(id=substr(Sample_ID,1,5)) %>% mutate(u=substr(Sample_ID,10,12)) %>% arrange(u) %>% writexl::write_xlsx("output/orinas_revision.xlsx")
