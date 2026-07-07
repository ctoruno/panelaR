#' Summarise panel-linkage verification diagnostics for a single period
#'
#' Computes period-level match-quality statistics from the output of
#' \code{\link{verify_person_link}}. The result is a one-row summary showing
#' how many respondents were matched and what share passed each verification
#' criterion.
#'
#' @param linked Data frame. Output of \code{\link{verify_person_link}} for a
#'   single current/previous period pair. Must contain the columns produced by
#'   that function: \code{period} (character), \code{sex_ok} (logical),
#'   \code{age_ok} (logical), \code{line_ok} (logical), and \code{verified}
#'   (logical).
#'
#' @return A one-row data frame with the following columns:
#'   \describe{
#'     \item{\code{period}}{Period label inherited from \code{linked}
#'       (format \code{"<year>-<period>"}).}
#'     \item{\code{n_slot_matched}}{Integer. Total number of respondents whose
#'       ID appeared in both the current and previous period (i.e. the number
#'       of rows in \code{linked}).}
#'     \item{\code{pct_sex_ok}}{Double. Share of matched respondents with
#'       identical sex across periods.}
#'     \item{\code{pct_age_ok}}{Double. Share of matched respondents whose age
#'       difference across periods is at most 2 years.}
#'     \item{\code{pct_lineage_ok}}{Double. Share of matched respondents with
#'       identical lineage to the household head (informational — not part of
#'       the \code{verified} criterion).}
#'     \item{\code{pct_verified}}{Double. Share of matched respondents that
#'       pass the full verification check (\code{sex_ok & age_ok}).}
#'   }
#'
#' @seealso \code{\link{verify_person_link}} which produces the \code{linked}
#'   input.
#'
#' @export
link_diagnostics <- function(linked) {
  linked |>
    dplyr::summarise(
      period = dplyr::first(period),
      n_slot_matched = dplyr::n(),
      pct_sex_ok     = mean(sex_ok, na.rm = TRUE),
      pct_age_ok     = mean(age_ok, na.rm = TRUE),
      pct_lineage_ok = mean(line_ok, na.rm = TRUE),
      pct_verified   = mean(verified, na.rm = TRUE)
    )
}