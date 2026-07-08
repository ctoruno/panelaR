devtools::load_all()
path_to_data <- Sys.getenv("path_to_data")

# Loading data
periods <- lapply(
  seq(2021, 2026),
  \(year) {
    lapply(
      sprintf("%02d", 1:12),
      \(month){
        glue::glue("{year}-{month}")
      }
    )
  }
) |>
  unlist()
periods <- periods[1:(length(periods)-8)]

ene_raw_data <- read_data(  # panelaR::read_data
  survey = "ene", 
  path_to_data = path_to_data,
  periods = periods
)
ene_full_data <- dplyr::bind_rows(ene_raw_data) |>
  dplyr::filter(mes_central == mes_encuesta)  # We drop duplicated values from the mobile-quarter

# Assessing the theoretical overlap between quarters (50%)
overlap_tables <- lapply(
  c(
    "households" = "id_identificacion",
    "respondents" = "idrph"
  ),
  \(id){

    purrr::map_dfr(
      periods,
      \(period){

        year  <- as.integer(stringr::str_sub(period, 1, 4))
        month <- as.integer(stringr::str_sub(period, 6, 7))
        prevy <- if (month %in% c(1,2,3)) year-1 else year
        prevm <- if (month %in% c(1,2,3)) month+9 else month-3

        estimate_overlap(   # panelaR::estimate_overlap
          ene_full_data,
          id = id,
          curr_year = year, curr_period = month,
          prev_year = prevy, prev_period = prevm
        )

      }
    )

  }
)

id_diagnostic <- purrr::map_dfr(
  periods,
  \(period){

    year  <- as.integer(stringr::str_sub(period, 1, 4))
    month <- as.integer(stringr::str_sub(period, 6, 7))
    prevy <- if (month %in% c(1,2,3)) year-1 else year
    prevm <- if (month %in% c(1,2,3)) month+9 else month-3

    linked_data <- verify_person_link(  # panelaR::verify_person_link
      data = ene_full_data,
      id_respondent = "idrph",
      curr_year = year, curr_period = month,
      prev_year = prevy, prev_period = prevm,
      sex = "sexo", 
      age = "edad", 
      line = "parentesco"
    )

    link_diagnostics(linked_data)  # panelaR::link_diagnostics
  }
)
