#' Derive a household crosswalk from person-level matches
#'
#' Majority vote: household h in the previous wave maps to the household
#' in the current wave that absorbed most of its matched members. Detects
#' the "household 01 in January became household 02 in April" case.
#'
#' @param person_matches Output of
#'   \code{\link{match_residual_within_dwelling}} (optionally combined
#'   with ID-verified matches carrying the same columns).
#'
#' @return One row per (dwelling, prev household): the modal current
#'   household, the number of supporting person links, and
#'   \code{renumbered} indicating the household number changed.
#'
#' @export
derive_household_crosswalk <- function(person_matches) {
  person_matches |>
    dplyr::count(
      id_dwelling, id_household_prev, id_household_curr,
      name = "n_person_links"
    ) |>
    dplyr::group_by(id_dwelling, id_household_prev) |>
    dplyr::slice_max(n_person_links, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(renumbered = id_household_prev != id_household_curr)
}