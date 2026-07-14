devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

# Loading data
periods <- lapply(
  seq(2023, 2026),
  \(year) {
    lapply(
      sprintf("%01d", 1:4),
      \(quarter){
        glue::glue("{year}_{quarter}")
      }
    )
  }
) |>
  unlist()
periods <- periods[1:(length(periods)-3)]

enoe_raw_data <- read_data(  # panelaR::read_data
  path_to_data = file.path(
    path_to_data, "kgl-data", "enoe"
  )
)
sdem_data <- enoe_raw_data[
  stringr::str_detect(names(enoe_raw_data), "SDEM")
]
sdem_full_data <- dplyr::bind_rows(sdem_data)

# ENOE does not come with keys
sdem_full_data_with_ids <- sdem_full_data |> 
  dplyr::mutate(
    eda = as.integer(eda),
    ent = dplyr::if_else(
      is.na(ent), cve_ent, ent  # For some rows the ENT column is filled with a CVE_ENT value instead (no idea)
    )
  ) |> 
  tidyr::unite(
    "id_dwelling", cd_a, ent, con, v_sel, 
    remove = FALSE
  ) |> 
  tidyr::unite(
    "id_household", cd_a, ent, con, v_sel, n_hog, h_mud, 
    remove = FALSE
  ) |> 
  tidyr::unite(
    "id_respondent", cd_a, ent, con, v_sel, n_hog, h_mud, n_ren,
    remove = FALSE
  )

# Assessing the theoretical overlap between quarters (80%)
overlap_tables <- lapply(
  c(
    "dwellings" = "id_dwelling",
    "households" = "id_household",
    "respondents" = "id_respondent"
  ),
  \(id){

    purrr::map_dfr(
      periods,
      \(period){

        year <- as.integer(stringr::str_sub(period, 1, 4))
        quarter <- as.integer(stringr::str_sub(period, 6, 6))
        prevy <- if (quarter == 1) year-1 else year
        prevq <- if (quarter == 1) 4 else quarter-1

        estimate_overlap(   # panelaR::estimate_overlap
          sdem_full_data_with_ids,
          id = id,
          curr_year = year, curr_period = quarter,
          prev_year = prevy, prev_period = prevq
        )

      }
    )

  }
)
readr::write_csv(
  overlap_tables$dwellings,
  here::here("outputs", "diagnostics", "enoe_overlap_dwellings.csv")
)

id_diagnostic <- purrr::map_dfr(
  periods,
  \(period){

    year <- as.integer(stringr::str_sub(period, 1, 4))
    quarter <- as.integer(stringr::str_sub(period, 6, 6))
    prevy <- if (quarter == 1) year-1 else year
    prevq <- if (quarter == 1) 4 else quarter-1

    linked_data <- verify_person_link(  # panelaR::verify_person_link
      data = sdem_full_data_with_ids,
      id_respondent = "id_respondent",
      curr_year = year, curr_period = quarter,
      prev_year = prevy, prev_period = prevq,
      sex = "sex", 
      age = "eda", 
      line = "par_c"
    )

    link_diagnostics(linked_data)  # panelaR::link_diagnostics
  }
)
