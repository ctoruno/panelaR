devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

# Loading data
enoe_raw_data <- read_data(  # panelaR::read_data
  path_to_data = file.path(
    path_to_data, "kgl-data", "enoe"
  )
)
viv_data <- enoe_raw_data[
  stringr::str_detect(names(enoe_raw_data), "VIV")
]
sdem_data <- enoe_raw_data[
  stringr::str_detect(names(enoe_raw_data), "SDEM")
]
viv_full_data <- dplyr::bind_rows(viv_data)
sdem_full_data <- dplyr::bind_rows(sdem_data)

# ENOE does not ship stable panel keys, so we build them by concatenating identifier columns.
viv_full_data_with_ids <- viv_full_data |>
  dplyr::mutate(
    ent = dplyr::if_else(
      is.na(ent), cve_ent, ent  # For some rows the ENT column is filled with a CVE_ENT value instead (no idea)
    )
  ) |>
  tidyr::unite(
    "id_dwelling", cd_a, ent, con, v_sel, tipo,
    remove = FALSE
  )
sdem_full_data_with_ids <- sdem_full_data |>
  dplyr::mutate(
    eda = as.integer(eda),
    ent = dplyr::if_else(
      is.na(ent), cve_ent, ent  # For some rows the ENT column is filled with a CVE_ENT value instead (no idea)
    )
  ) |>
  tidyr::unite(
    "id_dwelling", cd_a, ent, con, v_sel, tipo,
    remove = FALSE
  ) |>
  tidyr::unite(
    "id_household", cd_a, ent, con, v_sel, n_hog, h_mud, tipo,
    remove = FALSE
  ) |>
  tidyr::unite(
    "id_respondent", cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo,
    remove = FALSE
  )

rm(enoe_raw_data, viv_data, viv_full_data, sdem_data, sdem_full_data) # To clear memory usage

# ---- Input sanity: one id per wave -------------------------------------------
# Every downstream join assumes its key is unique within a (year, period): the
# person key for the SDEM roster (verify_person_link / the fuzzy matcher) and
# the dwelling key for the VIV table. If a concatenation is not fine-grained
# enough, the join fans out and every count (overlap rates, tier-1 matches,
# household support) is silently inflated.
check_unique_id_per_period <- function(data, id_col, label) {
  dups <- data |>
    dplyr::count(year, period, id = .data[[id_col]], name = "n_rows") |>
    dplyr::filter(n_rows > 1)

  if (nrow(dups) > 0) {
    n_waves <- dplyr::n_distinct(dups$year, dups$period)
    warning(glue::glue(
      "{id_col} is NOT unique within period in {label}: {nrow(dups)} duplicated ",
      "id-period combination(s) across {n_waves} wave(s). Downstream joins will ",
      "fan out and inflate every count. Revisit the ID concatenation."
    ))
  } else {
    message(glue::glue("[ok] {id_col} is unique within every period in {label}."))
  }

  invisible(dups)
}

sdem_id_duplicates <- check_unique_id_per_period(
  sdem_full_data_with_ids, "id_respondent", "SDEM roster"
)
viv_id_duplicates <- check_unique_id_per_period(
  viv_full_data_with_ids, "id_dwelling", "VIV table"
)

# ---- Wave grid ---------------------------------------------------------------
# ENOE tracks dwellings across CONSECUTIVE quarters, so there is a single
# look-back: a respondent interviewed in quarter t was last interviewed in t-1
# quarter.
prev_lags <- 1

shift_quarter <- function(year, quarter, back) {
  idx <- (year * 4 + (quarter - 1)) - back
  tibble::tibble(year = idx %/% 4, period = idx %% 4 + 1)
}

waves_observed <- sdem_full_data_with_ids |>
  dplyr::distinct(year, period) |>
  dplyr::mutate(key = year * 10 + period) |>
  dplyr::pull(key)

has_wave <- function(y, p) (y * 10 + p) %in% waves_observed

wave_pairs <- sdem_full_data_with_ids |>
  dplyr::distinct(curr_year = year, curr_period = period) |>
  tidyr::expand_grid(prev_lag = prev_lags) |>
  dplyr::mutate(prev = shift_quarter(curr_year, curr_period, prev_lag)) |>
  tidyr::unpack(prev, names_sep = "_") |>
  dplyr::filter(has_wave(prev_year, prev_period)) |>
  dplyr::mutate(
    curr_wave = sprintf("%d-Q%d", curr_year, curr_period),
    prev_wave = sprintf("%d-Q%d", prev_year, prev_period)
  ) |>
  dplyr::arrange(curr_year, curr_period, prev_lag)

pair_ids <- sprintf("%s_t-%dq", wave_pairs$curr_wave, wave_pairs$prev_lag)

meta_rows <- wave_pairs |>
  dplyr::select(curr_wave, prev_wave, prev_lag) |>
  purrr::pmap(\(...) tibble::tibble(...))

# ---- Raw overlap -------------------------------------------------------------
# Theoretical inter-quarter overlap for ENOE is 80%.
overlap_tables <- lapply(
  c(
    "dwellings"   = "id_dwelling",
    "households"  = "id_household",
    "respondents" = "id_respondent"
  ),
  \(id) {

    purrr::pmap(
      wave_pairs,
      \(curr_year, curr_period, prev_lag, prev_year, prev_period, ...) {

        estimate_overlap(   # panelaR::estimate_overlap
          data = sdem_full_data_with_ids,
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
    here::here("outputs", "diagnostics", glue::glue("enoe_overlap_{unit}.csv"))
  )
)

# ---- Tier 1: ID-based verification -------------------------------------------
linked_by_wave <- purrr::pmap(
  wave_pairs,
  \(curr_year, curr_period, prev_lag, prev_year, prev_period, curr_wave, prev_wave) {

    verify_person_link(  # panelaR::verify_person_link
      data = sdem_full_data_with_ids,
      id_respondent = "id_respondent",
      curr_year = curr_year, curr_period = curr_period,
      prev_year = prev_year, prev_period = prev_period,
      sex = "sex",
      age = "eda",
      line = "par_c"
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
  here::here("outputs", "diagnostics", "enoe_id_diagnostic.csv")
)

# ---- Tier 1 + Tier 2: two-tier linkage ---------------------------------------
roster_of <- function(y, p) {
  sdem_full_data_with_ids |>
    dplyr::filter(year == y, period == p) |>
    dplyr::select(
      id_dwelling,
      id_household,
      id_person = id_respondent
    )
}

linked_all <- purrr::pmap(
  wave_pairs,
  \(curr_year, curr_period, prev_lag, prev_year, prev_period, curr_wave, prev_wave) {

    res <- link_persons_tiered(  # panelaR::link_persons_tiered
      data = sdem_full_data_with_ids,
      curr_year = curr_year, curr_period = curr_period,
      prev_year = prev_year, prev_period = prev_period,
      id_dwelling  = "id_dwelling",
      id_household = "id_household",
      id_person    = "id_respondent",
      sex = "sex",
      age = "eda",
      line = "par_c"
    )

    tag <- \(d) dplyr::mutate(
      d,
      prev_lag  = prev_lag,
      curr_wave = curr_wave,
      prev_wave = prev_wave
    )

    roster_prev <- roster_of(prev_year, prev_period)
    roster_curr <- roster_of(curr_year, curr_period)

    # No links at all would mean both id_respondent and id_dwelling are
    # reassigned across the quarter -- a finding, not a reason to crash.
    if (nrow(res$persons) == 0) {
      return(list(
        persons    = tag(res$persons),
        households = tag(res$households),
        support    = NULL
      ))
    }

    support <- res$persons |>
      augment_tier1_ids(roster_prev) |>   # panelaR::augment_tier1_ids
      household_link_support(             # panelaR::household_link_support
        roster_prev = roster_prev,
        roster_curr = roster_curr
      )

    list(
      persons    = tag(res$persons),
      households = tag(res$households),
      support    = tag(support)
    )

  }
) |>
  purrr::set_names(pair_ids)

persons_panel <- purrr::map(linked_all, "persons") |>
  purrr::list_rbind(names_to = "period_pair")

hh_support <- purrr::map(linked_all, "support") |>
  purrr::compact() |>
  purrr::list_rbind(names_to = "period_pair")

matched_id_diagnostic <- purrr::map2(
  meta_rows,
  linked_all,
  \(meta, x) {
    if (is.null(x$support) || nrow(x$persons) == 0) {
      return(dplyr::bind_cols(meta, tibble::tibble(n_linked = 0L)))
    }
    dplyr::bind_cols(
      meta,
      link_diagnostics_tiered(x$persons, x$support) |>  # panelaR::link_diagnostics_tiered
        dplyr::select(-period)
    )
  }
) |>
  purrr::list_rbind()

readr::write_csv(
  matched_id_diagnostic,
  here::here("outputs", "diagnostics", "enoe_matched_id_diagnostic.csv")
)
