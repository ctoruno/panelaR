devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

#-----------------------------------------------------------------------
#---- DOWNLOADING ECU-ENEMDU RESOURCES
#-----------------------------------------------------------------------
download_source_enemdu(
  file.path(
    path_to_data, 
    "ECU-ENEMDU", "source-raw"
  )
)
# From Jan-2021 to May-2021, data is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# From Jun-2022 to Jul-2022, data is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# Data for Dec-2022 (CSV) is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# Data for Jul-2023 (SPSS) is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/

dest_folder <- file.path(
  path_to_data, "ECU-ENEMDU", "source"
)
dir.create(dest_folder, showWarnings = FALSE)

enemdu_csv_files <- list.files(
  c(
    file.path(path_to_data, "ECU-ENEMDU", "source-raw"),
    file.path(path_to_data, "ECU-ENEMDU", "manual_download")
  ), 
  pattern = "\\.csv$", 
  recursive = TRUE, 
  full.names = TRUE,
  ignore.case = TRUE
)

for (source_file in enemdu_csv_files){
  file.copy(from = source_file, to = dest_folder, overwrite = FALSE)
  message(
    glue::glue("[info] File: {source_file} copied!!")
  )
}
  

#-----------------------------------------------------------------------
#---- DOWNLOADING CHL-ENE RESOURCES
#-----------------------------------------------------------------------
download_source_ene(
  file.path(
    path_to_data, "CHL-ENE", "source-raw"
  )
)


#-----------------------------------------------------------------------
#---- DOWNLOADING MEX-ENOE RESOURCES
#-----------------------------------------------------------------------
download_source_enoe(
  file.path(
    path_to_data, "MEX-ENOE", "source-raw"
  )
)

dest_folder <- file.path(
  path_to_data, "MEX-ENOE", "source"
)
dir.create(dest_folder, showWarnings = FALSE)

enoe_csv_files <- list.files(
  c(
    file.path(path_to_data, "MEX-ENOE", "source-raw")
  ), 
  pattern = "\\.csv$", 
  recursive = TRUE, 
  full.names = TRUE,
  ignore.case = TRUE
)

for (source_file in enoe_csv_files){
  file.copy(from = source_file, to = dest_folder, overwrite = FALSE)
  message(
    glue::glue("[info] File: {source_file} copied!!")
  )
}


#-----------------------------------------------------------------------
#---- DOWNLOADING RESOURCES FROM KAGGLE
#-----------------------------------------------------------------------
surveys <- c("enemdu", "ene", "enoe")
for (survey in surveys) {
  download_data(
    survey = survey,
    output_dir = file.path(
      path_to_data, "kgl-data"
    )
  )
}

# Function returns the final path of the data
data_dirs <- lapply(
  surveys,
  \(survey) {
    download_data(
      survey = survey,
      output_dir = file.path(
        path_to_data, "kgl-data"
      ),
      overwrite = TRUE
    )
  }
)
print(data_dirs)
