library("tidyverse")
imagenes<-read_csv("D:/HAPIN/graficas_oscar/duplicate_imgs.csv")
imagenes<-imagenes %>% mutate(nombre=paste0(hash,".png"))
#http://144.217.19.236/hapinlascar/imgs/

tbl_lascar_graphic<-imagenes %>%mutate(id=hash) %>% select(id, nombre=hash)

if (!require("RMySQL"))    {install.packages("RMySQL");   library("RMySQL")}
con <- dbConnect(MySQL(), user="redcapdb", password="4S3rr3dc4p.2017++", dbname="hapin_exposure", host="144.217.19.237") 
#df_respuestas <- as.data.frame(tbl(con, sql("SELECT id, id_image, P1_CO, P2_CO_describe, comentarios, usuario FROM tbl_respuestas")))
#df_imagenes <- as.data.frame(tbl(con, sql("SELECT row_names, id FROM tbl_lascar_graphic ")))

#dbWriteTable(con, name="tbl_lascar_graphic", value = tbl_lascar_graphic,  overwrite=TRUE)

dbDisconnect(con)

if (!require("RMySQL"))    {install.packages("RMySQL");   library("RMySQL")}
con <- dbConnect(MySQL(), user="redcapdb", password="4S3rr3dc4p.2017++", dbname="hapin_exposure", host="144.217.19.237") 
df_respuestas <- as.data.frame(tbl(con, sql("SELECT id, id_image, P1_CO, P2_CO_describe, comentarios, usuario FROM tbl_respuestas")))
df_imagenes <- as.data.frame(tbl(con, sql("SELECT row_names, id FROM tbl_lascar_graphic ")))

df_respuestas_update<-df_respuestas %>% filter(usuario=="alex")

tbl_respuestas<-df_respuestas_update %>% full_join(bind_rows(list(
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="jkremer"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="apillarisetti"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="rpiedrahita"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="lunderhill"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="emollinedo"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="jmccracken"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="ksteenland"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="odeleon"
  ),
  df_imagenes %>% select(row_names) %>% mutate_all(as.numeric) %>% select(id_image=row_names) %>% mutate(
    comentario=" ",
    usuario="monitor"
  )
)
  
)
)


#dbWriteTable(con, name="tbl_respuestas", value = tbl_respuestas,  overwrite=TRUE)

dbDisconnect(con)
