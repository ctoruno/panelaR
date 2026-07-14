devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

# Loading data
periods <- lapply(
  seq(2021, 2026),
  \(year) {
    lapply(
      sprintf("%02d", 1:12),
      \(month){
        glue::glue("{year}_{month}")
      }
    )
  }
) |>
  unlist()
periods <- periods[1:(length(periods)-7)]

enemdu_raw_data <- read_data(  # panelaR::read_data
  path_to_data = file.path(
    path_to_data, "kgl-data", "enemdu"
  )
)
enemdu_full_data <- dplyr::bind_rows(enemdu_raw_data)

# Assessing the theoretical overlap between quarters (50%)
overlap_tables <- lapply(
  c(
    "dwellings" = "id_vivienda",
    "households" = "id_hogar",
    "respondents" = "id_persona"
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
          enemdu_full_data,
          id = id,
          curr_year = year, curr_period = month,
          prev_year = prevy, prev_period = prevm
        )

      }
    )

  }
)

readr::write_csv(
  overlap_tables$dwellings,
  here::here("outputs", "diagnostics", "enemdu_overlap_dwellings.csv")
)

# NOTE: this is partially right, this is following the inter-quarter logic of the
# theoretical overlap. But a person might have been out for two quarters and then re-entered.
# REEVALUATE -> maybe prev_year and prev_period should accept multiple entries

id_diagnostic <- purrr::map_dfr(
  periods,
  \(period){

    year  <- as.integer(stringr::str_sub(period, 1, 4))
    month <- as.integer(stringr::str_sub(period, 6, 7))
    prevy <- if (month %in% c(1,2,3)) year-1 else year
    prevm <- if (month %in% c(1,2,3)) month+9 else month-3

    linked_data <- verify_person_link(  # panelaR::verify_person_link
      data = enemdu_full_data,
      id_respondent = "id_persona",
      curr_year = year, curr_period = month,
      prev_year = prevy, prev_period = prevm,
      sex = "p02", 
      age = "p03", 
      line = "p04"
    )

    link_diagnostics(linked_data)  # panelaR::link_diagnostics
  }
)

readr::write_csv(
  id_diagnostic,
  here::here("outputs", "diagnostics", "enemdu_id_diagnostic.csv")
)

linked_all <- purrr::map(
  periods, 
  \(period){
    year  <- as.integer(stringr::str_sub(period, 1, 4))
    month <- as.integer(stringr::str_sub(period, 6, 7))
    prevy <- if (month %in% c(1,2,3)) year-1 else year
    prevm <- if (month %in% c(1,2,3)) month+9 else month-3

    res <- link_persons_tiered(  # panelaR::link_persons_tiered
      data = enemdu_full_data,
      curr_year = year, curr_period = month,
      prev_year = prevy, prev_period = prevm,
      id_dwelling = "id_vivienda",
      id_household = "id_hogar",
      id_person = "id_persona",
      sex = "p02", 
      age = "p03", 
      line = "p04"
    )
  }
) |>
  purrr::set_names(periods)

persons_panel   <- purrr::map(linked_all, "persons") |> 
  purrr::list_rbind(names_to = "period_pair")
hh_support_list <- purrr::imap(
  linked_all,
  \(res, period){

    year  <- as.integer(stringr::str_sub(period, 1, 4))
    month <- as.integer(stringr::str_sub(period, 6, 7))
    prevy <- if (month %in% c(1, 2, 3)) year - 1 else year
    prevm <- if (month %in% c(1, 2, 3)) month + 9 else month - 3

    roster <- \(y, m) {
      enemdu_full_data |>
        dplyr::filter(year == y, period == m) |>
        dplyr::select(
          id_dwelling  = id_vivienda,
          id_household = id_hogar,
          id_person    = id_persona
        )
    }
    roster_prev <- roster(prevy, prevm)
    roster_curr <- roster(year, month)

    res$persons |>
      augment_tier1_ids(roster_prev) |>          # panelaR::augment_tier1_ids
      household_link_support(                    # panelaR::household_link_support
        roster_prev = roster_prev,
        roster_curr = roster_curr
      )
  }
)
hh_support <- purrr::list_rbind(hh_support_list, names_to = "period_pair")

matched_id_diagnostic <- purrr::map2(
  purrr::map(linked_all, "persons"),
  hh_support_list,
  link_diagnostics_tiered
) |>
  purrr::list_rbind()
