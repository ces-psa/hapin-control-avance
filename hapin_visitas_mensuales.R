# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

library("tidyverse")

#sacar comunidades
cat_comunidades<-read_csv("data/dictionaries/cat_comunidades.csv")
comunidades<-gt_participants %>% select(record_id,id_estudio, codigo=com_jalapa) %>%
    mutate(house_id=if_else(condition = grepl("^G[0-9]{4}",id_estudio),
                            true = record_id,
                            false = id_estudio
    )) %>%mutate(id_tamizaje=if_else(condition = grepl("^G[0-9]{4}",record_id),
                                     true = record_id,
                                     false = id_estudio
    )) %>% 
    left_join(cat_comunidades %>% mutate(codigo=as.character(codigo))) %>% select(id=house_id, id_tamizaje, comunidad)


# los datos como en control avance
gt_emory_data %>%
    # los nacimientos
    filter(!is.na(c30_date)) %>%
    select(id, c30_date) %>%
    # quitar las salidas
    left_join(
        gt_emory_data %>%
            filter(!is.na(e3_date_c)) %>%
            select(id, e3_date_c)
    ) %>% {
        # si hay salidas, mostrarlas
        if(any(!is.na(.$e3_date_c))) filter(., !is.na(e3_date_c)) %>% print()
        # seguir con todos los datos
        .
    } %>%
     filter(is.na(e3_date_c)) %>% 
         select(-e3_date_c) %>%
    # agregar variable del mes
    mutate(mes = NA) %>%
    # agregar fecha de cada mes de edad
    complete(
        # usando solo las combinaciones existentes de id y c30_date
        nesting(id, c30_date),
        # agregar todos los meses del estudio
        mes = 1:12
    ) %>%
    # calcular fecha de visitas mensuales
    mutate(
        fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
        visita = case_when(
            # visitas b1, b2, b3, b4
            mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
            # resto de visitas mensuales
            TRUE ~ paste0("m", mes)
        ),
        # ¿se debe de visitar esta semana?
        flag_visita = lubridate::floor_date(Sys.Date()+7, unit = "weeks") == lubridate::floor_date(fecha_visita, unit = "weeks")
    ) %>%  filter(flag_visita=="TRUE") %>% 
    #eliminar los c31 que ya se hicieron
    left_join(
        gt_emory_data %>% select(id,c31_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(c31_date))
    ) %>% mutate(
                visit=if_else(
                        condition = is.na(visit), 
                        true = "pendiente", 
                        false = as.character(visit)
                        )
                 ) %>% filter(visita!=visit) %>% 
    #agregar id de tamizaje y comunidad
     left_join(
        comunidades
         ) %>% 
    #agregar fechas limite para visita
    mutate(
             limite_inferior= fecha_visita - lubridate::days(3),
             limite_superior= fecha_visita + lubridate::days(3),
             dias=as.numeric(limite_superior - Sys.Date())
         ) %>% 
    #generar listado
        select(house_id=id, fecha_nacimiento=c30_date, mes, fecha_visita, tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias, id_tamizaje, comunidad) %>% 
                    writexl::write_xlsx("output/visitas_c31_semanal_pendiente.xlsx")



#-------------------------------------------
#VISITAS DE H40
#-------------------------------------------

##tabla de H40 todas las visitas
dots_setup_rc <- gt_emory_data %>%
    mutate(irc = "guatemala") %>%
    select(
        irc, HHID = id, redcap_event_name,
        matches("h40_.*(visit|time|date|dot|area|stove|add_stove|data)"),
        # remove because we are not using it but was included because it matched dot
        -matches("complete|problem|action|other")
    ) %>%
    mutate_all(as.character) %>%
    #----------------------------------------------------------------------------*
    # Reshape and subset the data to the variables that are needed
    #----------------------------------------------------------------------------*
    gather(
        key = column, value = value,
        matches("visit|time|date|dot|area|stove|add_stove|data"),
        -matches("h40_date"),
        na.rm = TRUE
    ) %>%
    mutate(
        # explicit crf_copy on variable names
        column = if_else(
            condition = !grepl("_v[0-9]$", column),
            true = paste0(column, "_v1"),
            false = column
        ),
        # Fix non-standard variablenames
        column = gsub(
            pattern = "countinue",
            replacement = "continue",
            column
        )
    ) %>%
    # separate variable context
    extract(
        col = column,
        into = c("crf", "variable", "dot_correlative", "crf_copy"),
        regex = "(^[^_]+)_([^0-9]+)([0-9]+)?_v([0-9]+$)"
    ) %>%
    spread(key = variable, value = value) %>%
    # keep the correct date given the crf copy
    mutate(
        crf_date = if_else(
            condition = crf_copy == 1,
            true = h40_date,
            false = h40_date_v2
        )
    ) %>%
    arrange(HHID, crf_date) %>%
    # collect the data depending on the visit type
    select(
        irc, HHID, redcap_event_name, crf_date,
        crf_copy, dot_id = dot, visit,
        everything(), -crf, -h40_date, -h40_date_v2
    ) %>%
    gather(key = variable, value = value, matches("(ins|dl|stop)_(date|time)")) %>%
    separate(
        col = variable,
        into = c("step", "variable")
    ) %>%
    filter(!is.na(value)) %>%
    spread(key = variable, value = value) %>%
    rename(step_date = date) %>%
    arrange(HHID, dot_id, crf_date) %>% print()

dots_setup_rc %>% count(add_stove) %>% print()


h40_intervencion<-dots_setup_rc %>% left_join(
    gt_emory_data %>% select(HHID=id,s6_date, s6_arm) %>% filter(!is.na(s6_date))
) %>%filter(step=="ins") %>% filter(s6_arm=="1")



h40_intervencion %>% 
    select(id=HHID, h40_date=crf_date,redcap_event_name, dot_id, dot_correlative, continue_dot, stove, step, visit) %>% 
# agregar variable del mes
mutate(mes = NA) %>%
    # agregar fecha de cada mes de edad
    complete(
        # usando solo las combinaciones existentes de id y c30_date
        nesting(id, h40_date),
        # agregar todos los meses del estudio
        mes = 1:36
    ) %>%
    # calcular fecha de visitas mensuales
    mutate(
        fecha_visita = h40_date + lubridate::days(round(365.25/12 * mes)),
        visita = case_when(
            # visitas b1, b2, b3, b4
            mes %in% c(3, 6, 9, 12, 15, 18) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12, 15, 18) == .x) %>% ifelse(length(.), ., 0L))),
            # resto de visitas mensuales
            TRUE ~ paste0("m", mes)
        ),
        # ¿se debe de visitar esta semana?
        flag_visita = lubridate::floor_date(Sys.Date(), unit = "weeks") == lubridate::floor_date(fecha_visita, unit = "weeks")
    ) %>%  filter(flag_visita=="TRUE") %>% 
    left_join(
        gt_emory_data %>% select(id,h40_date, visit) %>% filter(!is.na(h40_date))
    ) %>% mutate(
        visit=if_else(
            condition = is.na(visit), 
            true = "pendiente", 
            false = as.character(visit)
        )
    ) %>% filter(visita!=visit)



#----------------------------------------------------------
#VISITAS MENSUALES DE ERICK
#----------------------------------------------------------
#seleccionando las que tienen nacimientos
gt_emory_data %>% select(id, c30_date, c30_dob) %>% 
        filter(!is.na(c30_date)) %>% 
    # quitar las salidas
    left_join(
        gt_emory_data %>%
            filter(!is.na(e3_date_c)) %>%
            select(id, e3_date_c)
    ) %>% {
        # si hay salidas, mostrarlas
        if(any(!is.na(.$e3_date_c))) filter(., !is.na(e3_date_c)) %>% print()
        # seguir con todos los datos
        .
    } %>%
    filter(is.na(e3_date_c)) %>% 
    select(-e3_date_c) %>% left_join(
    #eliminar de listado los que ya tienen h41 en B1
    gt_emory_data %>% select(id, h41_date, redcap_event_name) %>% 
            filter(!is.na(h41_date)) %>% filter(redcap_event_name=="b1_arm_2")
    )  %>% filter(is.na(redcap_event_name)) %>% 
    mutate(
        edad_dias=Sys.Date()- as.Date(c30_dob),
        edad_meses=as.numeric(edad_dias)%/%30,
        residuo_dias=as.numeric(edad_dias)%%30
    ) %>% left_join(comunidades) %>% 
    select(hh_id=id,id_tamizaje,comunidad, "fecha de nacimiento"=c30_dob,  "Edad en: meses"=edad_meses, "dias"=residuo_dias) %>% 
    writexl::write_xlsx(paste("output/b1_exposicion",Sys.Date(),".xlsx"))

h41<-gt_emory_data %>% select(id, h41_date, redcap_event_name) %>% filter(!is.na(h41_date)) %>% filter(redcap_event_name=="b1_arm_2")
table(h41$redcap_event_name)




