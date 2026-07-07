devtools::load_all()
path_to_data <- Sys.getenv("path_to_data")

#-----------------------------------------------------------------------
#---- DOWNLOADING ECU-ENEMDU RESOURCES
#-----------------------------------------------------------------------
enemdu_download_source(
  file.path(
    path_to_data, "ECU-ENEMDU", "source-raw"
  )
)
# From Jan-2021 to May-2021, data is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# From Jun-2022 to Jul-2022, data is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# Data for Dec-2022 is not found and has to be manually downloaded from website: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/
# Data for Jan-2024 and July-2025 need to be unzipped manually.

dir.create(
  file.path(
    path_to_data, "ECU-ENEMDU", "source"
  ),
  showWarnings = FALSE
)
for (year in seq(2021, 2026)){
  for (month in seq(1,12)){

    month_2digs <- sprintf("%02d", month)

    source_file <- file.path(
      path_to_data, "ECU-ENEMDU", "source-raw", 
      glue::glue("{year}_{month_2digs}"),
      glue::glue("enemdu_persona_{year}_{month_2digs}.csv")
    )
    dest_folder <- file.path(
      path_to_data, "ECU-ENEMDU", "source"
    )
    
    if (file.exists(source_file)){
      file.copy(from = source_file, to = dest_folder)
      message(
        glue::glue("[info] File: enemdu_persona_{year}_{month_2digs} copied!!")
      )
    } else {
      message(
        glue::glue("[error] File: {source_file} does not exists")
      )
    }

  }
}
  

#-----------------------------------------------------------------------
#---- DOWNLOADING CHL-ENE RESOURCES
#-----------------------------------------------------------------------
download_ene_csv(
  file.path(
    path_to_data, "CHL-ENE", "source"
  )
)