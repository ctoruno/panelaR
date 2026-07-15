#' Two-tier person linkage: verified IDs first, dwelling fuzzy residual
#'
#' Tier 1 trusts the survey's own person identifier where the sex/age
#' verification passes (\code{\link{verify_person_link}}). Tier 2
#' fuzzy-matches the residual within dwellings
#' (\code{\link{match_residual_within_dwelling}}). The \code{match_tier}
#' column lets every downstream statistic be computed with and without the
#' fuzzy tier as a robustness check.
#'
#' Both tiers run at their defaults: the tier-1 \code{age_gap_tolerance} and
#' the tier-2 \code{age_gap_window} / \code{ambiguity_margin} are not exposed
#' here. Call the two matchers directly to tune them.
#'
#' @inheritParams match_residual_within_dwelling
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{persons}}{Tier-1 and tier-2 matches stacked, with
#'       \code{match_tier} in \code{c("id_verified", "dwelling_fuzzy")}.
#'       The tiers carry different columns, so the stack is ragged: tier-1
#'       rows are \code{NA} for \code{id_dwelling}, the household ID pair,
#'       \code{score}, \code{ambiguous} and \code{n_hh_links}; tier-2 rows
#'       are \code{NA} for \code{sex_ok}, \code{age_ok}, \code{line_ok} and
#'       \code{verified}. Use \code{\link{augment_tier1_ids}} to recover the
#'       dwelling and household IDs on tier-1 rows.}
#'     \item{\code{households}}{Household crosswalk from
#'       \code{\link{derive_household_crosswalk}}, computed on the tier-2
#'       matches \emph{only} — tier-1 rows carry no household columns to vote
#'       with, so support is understated for households whose members were
#'       mostly recovered by tier 1. Prefer
#'       \code{\link{household_link_support}} (after
#'       \code{\link{augment_tier1_ids}}), which counts both tiers and scales
#'       the counts by household size.}
#'   }
#'
#' @seealso \code{\link{augment_tier1_ids}} and
#'   \code{\link{household_link_support}} for tier-aware household analysis.
#'
#' @export
link_persons_tiered <- function(
  data,
  curr_year, curr_period,
  prev_year, prev_period,
  id_dwelling ,
  id_household,
  id_person,
  sex,
  age,
  line
) {

  tier1 <- verify_person_link(
    data = data,
    id_respondent = id_person,
    curr_year = curr_year, curr_period = curr_period,
    prev_year = prev_year, prev_period = prev_period,
    sex = sex, age = age, line = line
  ) |>
    dplyr::filter(verified)

  tier2 <- match_residual_within_dwelling(
    data = data,
    curr_year = curr_year, curr_period = curr_period,
    prev_year = prev_year, prev_period = prev_period,
    exclude_prev_ids = tier1$id_respondent,
    exclude_curr_ids = tier1$id_respondent,
    id_dwelling = id_dwelling, 
    id_household = id_household,
    id_person = id_person,
    sex = sex, age = age, line = line
  )

  persons <- dplyr::bind_rows(
    tier1 |>
      dplyr::mutate(
        match_tier = "id_verified",
        id_person_prev = id_respondent,
        id_person_curr = id_respondent
      ),
    tier2 |>
      dplyr::mutate(match_tier = "dwelling_fuzzy")
  )

  households <- tier2 |>
    derive_household_crosswalk()

  list(persons = persons, households = households)
}