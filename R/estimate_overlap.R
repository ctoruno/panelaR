#' Estimate the overlap rate between two survey waves
#'
#' Computes the share of respondent IDs present in an earlier wave that are
#' also observed in a later wave, providing a raw measure of panel retention
#' before any demographic verification.
#'
#' @param data Data frame. Pooled microdata containing at least the column
#'   named in \code{id} plus integer columns \code{year} and \code{period}.
#' @param id Character. Name of the column in \code{data} that holds the
#'   respondent identifier.
#' @param curr_year Integer. Year of the current (later) wave.
#' @param curr_period Integer. Sub-annual period code (e.g. quarter or month)
#'   of the current wave.
#' @param prev_year Integer. Year of the previous (earlier) wave.
#' @param prev_period Integer. Sub-annual period code of the previous wave.
#'
#' @return A one-row \code{\link[tibble]{tibble}} with the following columns:
#'   \describe{
#'     \item{\code{prev_wave}}{Character. Label of the earlier wave in
#'       \code{"<year>-<period>"} format.}
#'     \item{\code{curr_wave}}{Character. Label of the later wave in
#'       \code{"<year>-<period>"} format.}
#'     \item{\code{n_prev}}{Integer. Number of unique IDs in the previous wave.}
#'     \item{\code{n_curr}}{Integer. Number of unique IDs in the current wave.}
#'     \item{\code{n_matched}}{Integer. Number of IDs present in both waves.}
#'     \item{\code{overlap_rate}}{Double. Share of the previous wave's IDs that
#'       are also found in the current wave (\code{n_matched / n_prev}).}
#'   }
#'
#' @seealso \code{.extract_ids} for the internal helper that pulls the ID
#'   vector for a single wave.
#'
#' @export
estimate_overlap <- function(
  data, id,
  curr_year, curr_period,
  prev_year, prev_period
) {

  curr <- .extract_ids(data, curr_year, curr_period, id)
  prev <- .extract_ids(data, prev_year, prev_period, id)
  matched <- intersect(curr, prev)

  tibble::tibble(
    prev_wave    = sprintf("%d-%02d", prev_year, prev_period),
    curr_wave    = sprintf("%d-%02d", curr_year, curr_period),
    n_prev       = length(prev),
    n_curr       = length(curr),
    n_matched    = length(matched),
    overlap_rate = length(matched) / length(prev)   # share of earlier wave retained
  )
}

#' Extract unique respondent IDs for a single wave
#'
#' Internal helper used by \code{\link{estimate_overlap}}. Filters
#' \code{data} to the requested year/period and returns the distinct values of
#' the ID column as a plain vector.
#'
#' @param data Data frame. Pooled microdata (see \code{\link{estimate_overlap}}).
#' @param y Integer. Target year.
#' @param p Integer. Target sub-annual period code.
#' @param id Character. Name of the column holding the respondent identifier.
#'
#' @return A vector of unique respondent IDs present in the \code{year == y &
#'   period == p} subset of \code{data}.
#'
#' @keywords internal
.extract_ids <- function(data, y, p, id) {
  data |>
    dplyr::filter(year == y, period == p) |>
    dplyr::distinct(!!rlang::sym(id)) |>
    dplyr::pull(id)
}
