#' Fuzzy-match persons within dwellings across two ENEMDU periods
#'
#' Matches persons across two survey waves using the dwelling
#' (\code{id_vivienda}) as the only hard anchor, so that household
#' renumbering within a dwelling and person-line reshuffling within a
#' household cannot break the link. Household number and roster line are
#' used as \emph{soft} evidence in the match score, not as join keys.
#'
#' Intended to run as a residual tier on top of the ID-based match from
#' \code{\link{verify_person_link}}: pass the already-verified person IDs
#' via \code{exclude_prev_ids} / \code{exclude_curr_ids} so the fuzzy
#' matcher only sees the leftovers.
#'
#' Hard constraints: same dwelling, same sex, age gap within
#' \code{age_gap_window} (default \code{c(-1, 2)} for a 3-month gap:
#' expected aging is 0 or 1 completed year, plus one year of reporting
#' noise on either side).
#'
#' Score (higher = better):
#' \itemize{
#'   \item age gap of 0 or 1 (expected over a quarter): +4; each extra
#'     year of deviation: -2
#'   \item same relationship to household head (\code{line}): +3
#'   \item same household number within the dwelling: +2
#'   \item same roster line number (if supplied): +2
#' }
#'
#' Assignment is one-to-one within each dwelling (greedy on descending
#' score; near-optimal for household-sized groups). Matches are flagged
#' \code{ambiguous} when a competing feasible pair for either person came
#' within \code{ambiguity_margin} score points of the selected pair —
#' the demographic-twin case (same-sex, similar-age siblings).
#'
#' @param data Pooled microdata with integer columns \code{year},
#'   \code{period}, plus the ID and demographic columns named below.
#' @param curr_year,curr_period,prev_year,prev_period Integer. Wave pair.
#' @param exclude_prev_ids,exclude_curr_ids Character vectors of
#'   \code{id_person} values already matched by the ID-based tier, to be
#'   removed from the candidate pool on each side.
#' @param id_dwelling,id_household,id_person Character. Column names.
#' @param sex,age,line Character. Column names (ENEMDU: p02, p03, p04).
#' @param age_gap_window Integer length-2. Inclusive feasible range for
#'   \code{age_curr - age_prev}.
#' @param ambiguity_margin Numeric. Score margin below which a competing
#'   candidate triggers the \code{ambiguous} flag.
#'
#' @return One row per matched pair: dwelling, prev/curr household and
#'   person IDs, the scoring inputs, \code{score}, \code{n_hh_links}
#'   (number of matched pairs sharing the same prev/curr household pair
#'   in the dwelling — 1 means an unsupported single-person match), and
#'   \code{ambiguous}.
#'
#' @export
match_residual_within_dwelling <- function(
  data,
  curr_year, curr_period,
  prev_year, prev_period,
  exclude_prev_ids = character(0),
  exclude_curr_ids = character(0),
  id_dwelling,
  id_household,
  id_person,
  sex,
  age,
  line,
  age_gap_window = c(-1, 2),
  ambiguity_margin = 1
) {

  prev <- .extract_dwelling_roster(
    data, prev_year, prev_period,
    id_dwelling, id_household, id_person, sex, age, line
  ) |>
    dplyr::filter(!id_person %in% exclude_prev_ids)

  curr <- .extract_dwelling_roster(
    data, curr_year, curr_period,
    id_dwelling, id_household, id_person, sex, age, line
  ) |>
    dplyr::filter(!id_person %in% exclude_curr_ids)

  candidates <- dplyr::inner_join(
    prev, curr,
    by = "id_dwelling",
    suffix = c("_prev", "_curr"),
    relationship = "many-to-many"
  ) |>
    dplyr::mutate(age_gap = age_curr - age_prev) |>
    dplyr::filter(
      sex_prev == sex_curr,
      age_gap >= age_gap_window[1],
      age_gap <= age_gap_window[2]
    ) |>
    dplyr::mutate(
      score =
        4 - 2 * pmax(0, abs(age_gap - 0.5) - 0.5) +   # peak at gap 0 or 1
        3 * (line_prev == line_curr) +
        2 * (id_household_prev == id_household_curr)
    )

  if (nrow(candidates) == 0) {
    return(
      candidates |>
        dplyr::mutate(
          ambiguous  = logical(0),
          n_hh_links = integer(0),
          period     = character(0)
        )
    )
  }

  # Second-best competing score (what if the household has twins?)
  candidates <- candidates |>
    dplyr::group_by(id_dwelling, id_person_prev) |>
    dplyr::mutate(rival_prev = .second_best(score)) |>
    dplyr::group_by(id_dwelling, id_person_curr) |>
    dplyr::mutate(rival_curr = .second_best(score)) |>
    dplyr::ungroup()

  matched <- candidates |>
    dplyr::group_split(id_dwelling) |>
    purrr::map(.greedy_assign) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      ambiguous =
        (score - dplyr::coalesce(rival_prev, -Inf) < ambiguity_margin) |
        (score - dplyr::coalesce(rival_curr, -Inf) < ambiguity_margin)
    ) |>
    dplyr::select(-rival_prev, -rival_curr)

  # Corroboration: single-person fuzzy matches are weaker evidence than
  # matches embedded in a household block that moved together.
  matched |>
    dplyr::add_count(
      id_dwelling, id_household_prev, id_household_curr,
      name = "n_hh_links"
    ) |>
    dplyr::mutate(period = glue::glue("{curr_year}-{curr_period}"))
}

#' Extract a dwelling-level roster for one period
#' @keywords internal
#' @noRd
.extract_dwelling_roster <- function(
  data, y, p,
  id_dwelling, id_household, id_person, sex, age, line
) {
  out <- data |>
    dplyr::filter(year == y, period == p) |>
    dplyr::select(
      id_dwelling  = dplyr::all_of(id_dwelling),
      id_household = dplyr::all_of(id_household),
      id_person    = dplyr::all_of(id_person),
      sex  = dplyr::all_of(sex),
      age  = dplyr::all_of(age),
      line = dplyr::all_of(line)
    )
  out
}

#' Score of the best competitor (2nd highest value), NA if none
#' @keywords internal
#' @noRd
.second_best <- function(score) {
  if (length(score) < 2) return(NA_real_)
  vapply(
    seq_along(score),
    \(i) max(score[-i]),
    numeric(1)
  )
}

#' Greedy one-to-one assignment within a dwelling
#'
#' Sorts candidate pairs by descending score and keeps a pair only if
#' neither person has been used. For optimal assignment on larger groups
#' use \code{clue::solve_LSAP}; for household-sized rosters greedy is
#' equivalent in practice and dependency-free.
#' @keywords internal
#' @noRd
.greedy_assign <- function(pairs) {
  pairs <- pairs[order(-pairs$score), ]
  used_prev <- character(0)
  used_curr <- character(0)
  keep <- logical(nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    if (
      !(pairs$id_person_prev[i] %in% used_prev) &&
      !(pairs$id_person_curr[i] %in% used_curr)
    ) {
      keep[i] <- TRUE
      used_prev <- c(used_prev, pairs$id_person_prev[i])
      used_curr <- c(used_curr, pairs$id_person_curr[i])
    }
  }
  pairs[keep, ]
}