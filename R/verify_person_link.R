#' Verify panel linkage for individual respondents across two periods
#'
#' Extracts a subset of sociodemographic variables for a respondent ID column
#' in two survey periods (current and previous), then performs an inner join to
#' identify matched respondents. A match is considered \emph{verified} when sex
#' is identical and the age difference between periods is at most
#' \code{age_gap_tolerance} years.
#' Lineage to the household head (\code{line}) is joined and exposed for
#' inspection but is intentionally excluded from the verification criterion,
#' since a respondent's relationship to the household head can legitimately
#' change between waves.
#'
#' @param data Data frame. Pooled microdata containing at least the columns
#'   named in \code{id_respondent}, \code{sex}, \code{age}, \code{line}, plus
#'   integer columns \code{year} and \code{period}.
#' @param id_respondent Character. Name of the column in \code{data} that holds
#'   the unique respondent identifier used to link records across periods.
#' @param curr_year Integer. Year of the current (later) period.
#' @param curr_period Integer or character. Sub-annual period code (e.g. quarter
#'   or month) for the current observation.
#' @param prev_year Integer. Year of the previous (earlier) period.
#' @param prev_period Integer or character. Sub-annual period code for the
#'   previous observation.
#' @param sex Character. Name of the column in \code{data} containing sex.
#' @param age Character. Name of the column in \code{data} containing age.
#' @param line Character. Name of the column in \code{data} containing lineage
#'   to the household head.
#' @param age_gap_tolerance Integer. Maximum absolute age gap displayed by an
#'   individual to be considered linked. Defaults to 1: over a quarterly gap
#'   the expected aging is 0 or 1 completed year.
#'
#' @return A data frame with one row per respondent ID present in \emph{both}
#'   periods. Columns include the suffixed sociodemographic variables
#'   (\code{_prev} / \code{_curr}) and the following derived indicators:
#'   \describe{
#'     \item{\code{period}}{Label of the current period in
#'       \code{"<year>-<period>"} format.}
#'     \item{\code{sex_ok}}{Logical. \code{TRUE} when sex matches across
#'       periods.}
#'     \item{\code{age_gap}}{Integer. Difference \code{age_curr - age_prev}.}
#'     \item{\code{age_ok}}{Logical. \code{TRUE} when
#'       \code{abs(age_gap) <= age_gap_tolerance}.}
#'     \item{\code{line_ok}}{Logical. \code{TRUE} when lineage to household head
#'       matches (informational only — not used in \code{verified}).}
#'     \item{\code{verified}}{Logical. \code{TRUE} when both \code{sex_ok} and
#'       \code{age_ok} are \code{TRUE}.}
#'   }
#'
#' @seealso \code{.extract_respondents_data} for the internal helper that
#'   filters and renames columns for a single period.
#'
#' @export
verify_person_link <- function(
  data, id_respondent,
  curr_year, curr_period,
  prev_year, prev_period,
  sex, age, line,
  age_gap_tolerance = 1
) {
 
  curr <- .extract_respondents_data(id_respondent, data, curr_year, curr_period, sex, age, line)
  prev <- .extract_respondents_data(id_respondent, data, prev_year, prev_period, sex, age, line)
 
  dplyr::inner_join(
    prev, curr, 
    by = "id_respondent",
    suffix = c("_prev", "_curr")
  ) |>
    dplyr::mutate(
      period  = glue::glue("{curr_year}-{curr_period}"),
      sex_ok  = sex_prev == sex_curr,
      age_gap = age_curr - age_prev,
      age_ok  = abs(age_gap) <= age_gap_tolerance,
      line_ok = line_prev == line_curr,
      verified  = sex_ok & age_ok
    )
}

#' Extract and rename respondent sociodemographics for a single period
#'
#' Internal helper used by \code{\link{verify_person_link}}. Filters
#' \code{data} to the requested year/period and returns a narrow data frame
#' with four columns renamed to stable names so the subsequent inner join and
#' verification logic are column-name agnostic.
#'
#' @param id_respondent Character. Name of the ID column to rename to
#'   \code{id_respondent}.
#' @param data Data frame. Pooled microdata (see \code{\link{verify_person_link}}).
#' @param y Integer. Target year.
#' @param p Integer or character. Target sub-annual period code.
#' @param sex Character. Name of the sex column.
#' @param age Character. Name of the age column.
#' @param line Character. Name of the lineage-to-household-head column.
#'
#' @return A data frame filtered to \code{year == y & period == p} with columns
#'   \code{id_respondent}, \code{sex}, \code{age}, and \code{line}.
#'
#' @keywords internal
.extract_respondents_data <- function(id_respondent, data, y, p, sex, age, line) {
  data |>
    dplyr::filter(year == y, period == p) |>
    dplyr::select(
      id_respondent = dplyr::all_of(id_respondent),
      sex = dplyr::all_of(sex),
      age = dplyr::all_of(age),
      line = dplyr::all_of(line) # lineage to household head
    )
}