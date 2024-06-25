library("tidyverse")
 library("sas7bdat")
read.sas7bdat("c:/temp/hapin_pneumonia_expotrt_20230331.sas7bdat")
read.sas7bdat(file = "c:/temp/hapin_pneumonia_expotrt_20230331.sas7bdat")



convert_sas <- function(file, write_path, to_ext = "rds") {
  if(!file.exists(write_path)) dir.create(write_path, recursive = T)
  fn <- basename(file)
  df <- haven::read_sas(file)
  df <- dplyr::rename_all(df, toupper)
  if (to_ext == "rds"){
    readr::write_rds(
      df,
      paste0(write_path, "/", tools::file_path_sans_ext(fn), ".rds")
    )
  } else if (to_ext == "csv"){
    readr::write_csv(
      df,
      paste0(write_path, "/", tools::file_path_sans_ext(fn), ".csv")
    )
  }
  return(invisible(NULL))
}

convert_sas("c:/temp/hapin_pneumonia_expotrt_20230331.sas7bdat", "output/data_sas", to_ext = "rds")

dt_sas_laura<-readRDS("output/data_sas/hapin_pneumonia_expotrt_20230331.rds")

dt_sas_laura
