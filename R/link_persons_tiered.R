#' Two-tier person linkage: verified IDs first, dwelling fuzzy residual
#'
#' Tier 1 trusts INEC's \code{id_persona} where the sex/age verification
#' passes (\code{\link{verify_person_link}}). Tier 2 fuzzy-matches the
#' residual within dwellings. The \code{match_tier} column lets every
#' downstream statistic be computed with and without the fuzzy tier as a
#' robustness check.
#'
#' @inheritParams match_residual_within_dwelling
#' @param ... Passed on to \code{match_residual_within_dwelling}.
#'
#' @return List with \code{persons} (stacked matches with
#'   \code{match_tier} in \code{c("id_verified", "dwelling_fuzzy")}) and
#'   \code{households} (crosswalk from
#'   \code{\link{derive_household_crosswalk}}).
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