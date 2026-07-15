devtools::load_all()
path_to_data <- Sys.getenv("PATH_TO_DATA")

# Loading data
enemdu_raw_data <- read_data(  # panelaR::read_data
  path_to_data = file.path(
    path_to_data, "kgl-data", "enemdu"
  )
)
person_data <- enemdu_raw_data[
  stringr::str_detect(names(enemdu_raw_data), "_persona_\\d{4}_\\d{2}$") # I adjusted this to capture only "persona", there is another file that has "persona_tics" and that one introduces duplicated ids
]
person_full_data <- dplyr::bind_rows(person_data)

rm(enemdu_raw_data) # To clear memory usage

# ---- Input sanity: one id_persona per wave -----------------------------------
# Both the ID-based and the fuzzy matcher assume id_persona is unique within a
# (year, period). If it is not, the person-level inner join fans out and every
# downstream count (overlap rates, tier-1 matches, household support) is
# silently inflated. The usual cause is stacking a sub-module onto the person
# file -> e.g. persona_tics, which shares id_persona (see the filter above).
id_period_duplicates <- person_full_data |>
  dplyr::count(year, period, id_persona, name = "n_rows") |>
  dplyr::filter(n_rows > 1)

if (nrow(id_period_duplicates) > 0) {
  n_waves <- id_period_duplicates |>
    dplyr::distinct(year, period) |>
    nrow()
  warning(glue::glue(
    "id_persona is NOT unique within period: {nrow(id_period_duplicates)} ",
    "duplicated id-period combination(s) across {n_waves} wave(s). The ",
    "person-level join will fan out and inflate every downstream count. ",
    "Inspect `id_period_duplicates`."
  ))
} else {
  message("[ok] id_persona is unique within every period.")
}

# ---- Wave grid ---------------------------------------------------------------
# ENEMDU's rotating panel takes a dwelling out of the sample for two quarters,
# so a respondent interviewed at t was last interviewed at t-3 OR t-9 months,
# never both. Every step below therefore runs once per (current wave, lag) pair 
# and tags its output with `prev_lag`.

prev_lags <- c(3, 9)
shift_month <- function(year, month, back) {
  idx <- (year * 12 + (month - 1)) - back
  tibble::tibble(year = idx %/% 12, period = idx %% 12 + 1)
}
waves_observed <- person_full_data |>
  dplyr::distinct(year, period) |>
  dplyr::mutate(key = year * 100 + period) |>
  dplyr::pull(key)
has_wave <- function(y, p) (y * 100 + p) %in% waves_observed

wave_pairs <- person_full_data |>
  dplyr::distinct(curr_year = year, curr_period = period) |>
  tidyr::expand_grid(prev_lag = prev_lags) |>
  dplyr::mutate(prev = shift_month(curr_year, curr_period, prev_lag)) |>
  tidyr::unpack(prev, names_sep = "_") |>
  dplyr::filter(has_wave(prev_year, prev_period)) |>
  dplyr::mutate(
    curr_wave = sprintf("%d-%02d", curr_year, curr_period),
    prev_wave = sprintf("%d-%02d", prev_year, prev_period)
  ) |>
  dplyr::arrange(curr_year, curr_period, prev_lag)

pair_ids <- sprintf("%s_t-%d", wave_pairs$curr_wave, wave_pairs$prev_lag)

meta_rows <- wave_pairs |>
  dplyr::select(curr_wave, prev_wave, prev_lag) |>
  purrr::pmap(\(...) tibble::tibble(...))

# ---- Rotation check ----------------------------------------------------------
# Everything below assumes the two lags *partition* the current sample: a
# respondent present at t appears in t-3 OR t-9, never both. That is what lets 
# the two runs simply be stacked with no de-duplication.
#
#   * Tier 1 needs the person-level version of the claim, or bind_rows()
#     double-counts respondents.
#   * Tier 2 needs the *dwelling*-level version, because
#     match_residual_within_dwelling() joins on id_vivienda and never looks at
#     id_persona. A dwelling present in both previous waves would let the t-3
#     and t-9 runs fuzzy-match the same current person independently, and
#     neither run knows about the other's exclusions.
#
# This script exists to test whether ENEMDU's data matches its design, so the
# assumption gets asserted rather than trusted.

ids_in_wave <- function(y, p, id) {
  keep <- person_full_data$year == y & person_full_data$period == p
  unique(person_full_data[[id]][keep])
}

rotation_check <- wave_pairs |>
  dplyr::count(curr_year, curr_period, curr_wave) |>
  dplyr::filter(n == length(prev_lags)) |>   # both lags exist -> checkable
  dplyr::select(-n) |>
  purrr::pmap(
    \(curr_year, curr_period, curr_wave) {

      w3 <- shift_month(curr_year, curr_period, 3)
      w9 <- shift_month(curr_year, curr_period, 9)

      n_in_both <- function(id) {
        length(Reduce(
          intersect,
          list(
            ids_in_wave(curr_year, curr_period, id),
            ids_in_wave(w3$year, w3$period, id),
            ids_in_wave(w9$year, w9$period, id)
          )
        ))
      }

      tibble::tibble(
        curr_wave        = curr_wave,
        n_dwellings_both = n_in_both("id_vivienda"),
        n_persons_both   = n_in_both("id_persona")
      )
    }
  ) |>
  purrr::list_rbind()

rotation_violations <- rotation_check |>
  dplyr::filter(n_dwellings_both > 0 | n_persons_both > 0)

if (nrow(rotation_violations) > 0) {
  warning(glue::glue(
    "Rotation assumption VIOLATED in {nrow(rotation_violations)} of ",
    "{nrow(rotation_check)} checkable waves: some dwellings/respondents appear ",
    "in BOTH t-3 and t-9. The two lags are not disjoint, so the links stacked ",
    "below are double-counted. See `rotation_violations`."
  ))
  readr::write_csv(
    rotation_check,
    here::here("outputs", "diagnostics", "enemdu_rotation_check.csv")
  )
} else {
  message(glue::glue(
    "[ok] Rotation holds in all {nrow(rotation_check)} checkable waves: t-3 and ",
    "t-9 are disjoint at the dwelling and the person level."
  ))
}

# ---- Raw overlap, by lag -----------------------------------------------------
# Theoretical inter-quarter overlap is 50%. Computing it at t-9 as well is what
# turns the rotation design from an assumption into an observation.
overlap_tables <- lapply(
  c(
    "dwellings"   = "id_vivienda",
    "households"  = "id_hogar",
    "respondents" = "id_persona"
  ),
  \(id) {

    purrr::pmap(
      wave_pairs,
      \(curr_year, curr_period, prev_lag, prev_year, prev_period, ...) {

        estimate_overlap(   # panelaR::estimate_overlap
          data = person_full_data,
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
    here::here("outputs", "diagnostics", glue::glue("enemdu_overlap_{unit}.csv"))
  )
)

# ---- Tier 1: ID-based verification -------------------------------------------
linked_by_lag <- purrr::pmap(
  wave_pairs,
  \(curr_year, curr_period, prev_lag, prev_year, prev_period, curr_wave, prev_wave) {

    verify_person_link(  # panelaR::verify_person_link
      data = person_full_data,
      id_respondent = "id_persona",
      curr_year = curr_year, curr_period = curr_period,
      prev_year = prev_year, prev_period = prev_period,
      sex = "p02",
      age = "p03",
      line = "p04"
    ) |>
      dplyr::mutate(prev_lag = prev_lag, prev_wave = prev_wave)

  }
) |>
  purrr::set_names(pair_ids)

id_diagnostic <- purrr::map2(
  meta_rows,
  linked_by_lag,
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
  here::here("outputs", "diagnostics", "enemdu_id_diagnostic.csv")
)

# ---- Tier 1 + Tier 2: two-tier linkage ---------------------------------------
roster_of <- function(y, p) {
  person_full_data |>
    dplyr::filter(year == y, period == p) |>
    dplyr::select(
      id_dwelling  = id_vivienda,
      id_household = id_hogar,
      id_person    = id_persona
    )
}

linked_all <- purrr::pmap(
  wave_pairs,
  \(curr_year, curr_period, prev_lag, prev_year, prev_period, curr_wave, prev_wave) {

    res <- link_persons_tiered(  # panelaR::link_persons_tiered
      data = person_full_data,
      curr_year = curr_year, curr_period = curr_period,
      prev_year = prev_year, prev_period = prev_period,
      id_dwelling  = "id_vivienda",
      id_household = "id_hogar",
      id_person    = "id_persona",
      sex = "p02",
      age = "p03",
      line = "p04"
    )

    tag <- \(d) dplyr::mutate(
      d,
      prev_lag  = prev_lag,
      curr_wave = curr_wave,
      prev_wave = prev_wave
    )

    roster_prev <- roster_of(prev_year, prev_period)
    roster_curr <- roster_of(curr_year, curr_period)

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
  here::here("outputs", "diagnostics", "enemdu_matched_id_diagnostic.csv")
)
