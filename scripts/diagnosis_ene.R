devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

# Loading data
ene_raw_data <- read_data(  # panelaR::read_data
  path_to_data = file.path(
    path_to_data, "kgl-data", "ene"
  )
)
ene_full_data <- dplyr::bind_rows(ene_raw_data) |>
  dplyr::filter(mes_central == mes_encuesta)  # drop the mobile-quarter duplicates

rm(ene_raw_data) # To clear memory usage

# ---- Input sanity: one idrph per wave ----------------------------------------
# ENE ships each respondent once per mobile quarter, so mes_central == mes_encuesta 
# should enforce uniqueness.

id_period_duplicates <- check_unique_id_per_period(  # panelaR::check_unique_id_per_period
  ene_full_data, "idrph", "ENE person file"
)

# ---- Wave grid ---------------------------------------------------------------
# ENE's panel yields a single look-back: a respondent interviewed at t was last
# interviewed at t-3 months.
prev_lags <- 3
observed_waves <- dplyr::distinct(ene_full_data, year, period)

wave_pairs <- ene_full_data |>
  dplyr::distinct(curr_year = year, curr_period = period) |>
  tidyr::expand_grid(prev_lag = prev_lags) |>
  dplyr::mutate(prev = shift_t(curr_year, curr_period, prev_lag, by = "month")) |>  # panelaR::shift_t
  tidyr::unpack(prev, names_sep = "_") |>
  dplyr::filter(has_wave(prev_year, prev_period, observed_waves)) |>  # panelaR::has_wave
  dplyr::mutate(
    curr_wave = sprintf("%d-%02d", curr_year, curr_period),
    prev_wave = sprintf("%d-%02d", prev_year, prev_period)
  ) |>
  dplyr::arrange(curr_year, curr_period, prev_lag)

pair_ids <- sprintf("%s_t-%d", wave_pairs$curr_wave, wave_pairs$prev_lag)

meta_rows <- wave_pairs |>
  dplyr::select(curr_wave, prev_wave, prev_lag) |>
  purrr::pmap(\(...) tibble::tibble(...))

# ---- Raw overlap -------------------------------------------------------------
# Theoretical inter-quarter overlap for ENE is 83.33%.
overlap_tables <- lapply(
  c(
    "households"  = "id_identificacion",
    "respondents" = "idrph"
  ),
  \(id) {

    purrr::pmap(
      wave_pairs,
      \(curr_year, curr_period, prev_lag, prev_year, prev_period, ...) {

        estimate_overlap(   # panelaR::estimate_overlap
          data = ene_full_data,
          id = id,
          curr_year = curr_year, curr_period = curr_period,
          prev_year = prev_year, prev_period = prev_period
        ) |>
          dplyr::mutate(prev_lag = prev_lag, .before = 1)

      }
    ) |>
      purrr::list_rbind()

  }
)

purrr::iwalk(
  overlap_tables,
  \(tbl, unit) readr::write_csv(
    tbl,
    here::here("outputs", "diagnostics", glue::glue("ene_overlap_{unit}.csv"))
  )
)

# ---- Tier 1: ID-based verification -------------------------------------------
# ENE has no household renumbering to recover, so the ID-based tier is the whole
# story: there is no dwelling-fuzzy residual tier.
linked_by_wave <- purrr::pmap(
  wave_pairs,
  \(curr_year, curr_period, prev_lag, prev_year, prev_period, curr_wave, prev_wave) {

    verify_person_link(  # panelaR::verify_person_link
      data = ene_full_data,
      id_respondent = "idrph",
      curr_year = curr_year, curr_period = curr_period,
      prev_year = prev_year, prev_period = prev_period,
      sex = "sexo",
      age = "edad",
      line = "parentesco"
    )

  }
) |>
  purrr::set_names(pair_ids)

id_diagnostic <- purrr::map2(
  meta_rows,
  linked_by_wave,
  \(meta, linked) {
    if (nrow(linked) == 0) {
      return(dplyr::bind_cols(meta, tibble::tibble(n_slot_matched = 0L)))
    }
    dplyr::bind_cols(
      meta,
      link_diagnostics(linked) |>   # panelaR::link_diagnostics
        dplyr::select(-period)      # curr_wave already carries the label
    )
  }
) |>
  purrr::list_rbind()

readr::write_csv(
  id_diagnostic,
  here::here("outputs", "diagnostics", "ene_id_diagnostic.csv")
)
