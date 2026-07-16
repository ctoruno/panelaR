# Bare column names used with dplyr data-masking below. Declared so R CMD check
# does not flag them as undefined global variables.
utils::globalVariables(c("year", "period", "n_rows"))

#' Shift a survey wave backwards in time
#'
#' Moves a \code{(year, period)} wave back by \code{back} sub-annual steps,
#' rolling correctly across the year boundary (e.g. Q1 minus one quarter is Q4
#' of the previous year). Unifies the month-based look-back used by ENEMDU/ENE
#' with the quarter-based one used by ENOE.
#'
#' @param year Integer vector. Wave year(s).
#' @param period Integer vector. Sub-annual period(s): month \code{1-12} when
#'   \code{by = "month"}, quarter \code{1-4} when \code{by = "quarter"}.
#' @param back Integer. Number of periods to shift backwards.
#' @param by Character. Period granularity, one of \code{"month"} (default) or
#'   \code{"quarter"}.
#'
#' @return A \code{\link[tibble]{tibble}} with integer columns \code{year} and
#'   \code{period}, one row per element of the (recycled) inputs.
#'
#' @examples
#' shift_t(2024, 2, back = 3, by = "month")    # 2023-11
#' shift_t(2024, 1, back = 1, by = "quarter")  # 2023 Q4
#'
#' @export
shift_t <- function(year, period, back, by = c("month", "quarter")) {
  by  <- match.arg(by)
  ppy <- switch(by, month = 12L, quarter = 4L)
  idx <- (year * ppy + (period - 1L)) - back
  tibble::tibble(year = idx %/% ppy, period = idx %% ppy + 1L)
}

#' Test whether a wave exists in a reference set
#'
#' Vectorised membership test: for each \code{(year, period)} pair, is that wave
#' present in \code{reference}? Used to keep only computable wave pairs when
#' building a look-back grid, so no downstream step is handed a previous wave
#' absent from the data. Matches on the pair itself (not an arithmetic key), so
#' it is agnostic to month- vs quarter-coded periods.
#'
#' @param year,period Integer vectors of equal length. Waves to test.
#' @param reference Data frame with (at least) integer columns \code{year} and
#'   \code{period}. Typically the pooled microdata or its distinct waves.
#'
#' @return A logical vector the length of \code{year}: \code{TRUE} where the
#'   wave appears in \code{reference}.
#'
#' @export
has_wave <- function(year, period, reference) {
  ref_keys <- unique(paste(reference[["year"]], reference[["period"]], sep = "-"))
  paste(year, period, sep = "-") %in% ref_keys
}

#' Distinct respondent IDs present in a single wave
#'
#' Filters \code{data} to one \code{(year, period)} wave and returns the unique
#' values of the ID column \code{id} as a plain vector. Used by the ENEMDU
#' rotation check to intersect the ID sets of three waves.
#'
#' @param data Data frame with integer columns \code{year}, \code{period} and
#'   the column named in \code{id}.
#' @param y,p Integer. Target year and period.
#' @param id Character. Name of the ID column to extract.
#'
#' @return A vector of the distinct \code{id} values in the requested wave.
#'
#' @export
ids_in_wave <- function(data, y, p, id) {
  keep <- data[["year"]] == y & data[["period"]] == p
  unique(data[[id]][keep])
}

#' Extract a dwelling/household/person roster for a single wave
#'
#' Filters \code{data} to one \code{(year, period)} wave and returns the three
#' key columns renamed to the stable names \code{id_dwelling},
#' \code{id_household}, \code{id_person} expected by
#' \code{\link{augment_tier1_ids}} and \code{\link{household_link_support}}.
#'
#' @param data Data frame with integer columns \code{year}, \code{period} plus
#'   the three ID columns named below.
#' @param y,p Integer. Target year and period.
#' @param id_dwelling,id_household,id_person Character. Column names to map onto
#'   \code{id_dwelling}, \code{id_household}, \code{id_person}.
#'
#' @return A data frame with columns \code{id_dwelling}, \code{id_household},
#'   \code{id_person} for the requested wave.
#'
#' @export
roster_of <- function(data, y, p, id_dwelling, id_household, id_person) {
  data |>
    dplyr::filter(year == y, period == p) |>
    dplyr::select(
      id_dwelling  = dplyr::all_of(id_dwelling),
      id_household = dplyr::all_of(id_household),
      id_person    = dplyr::all_of(id_person)
    )
}

#' Check that an ID is unique within every wave
#'
#' Data-quality guard for the linkage pipeline: every join assumes its key is
#' unique within a \code{(year, period)} wave. If it is not, the join fans out
#' and every downstream count (overlap rates, tier-1 matches, household
#' support) is silently inflated. Emits a \code{warning} (not an error) listing
#' the offending waves, so a diagnostic run still completes and the problem
#' stays visible; the duplicated combinations are returned invisibly for
#' inspection.
#'
#' @param data Data frame with integer columns \code{year}, \code{period} and
#'   the column named in \code{id_col}.
#' @param id_col Character. Name of the ID column that should be unique within
#'   each wave (e.g. \code{"id_persona"}, \code{"idrph"}, \code{"id_dwelling"}).
#' @param label Character. Human-readable name of the table, used in messages.
#'   Defaults to \code{id_col}.
#'
#' @return Invisibly, a \code{\link[tibble]{tibble}} of the duplicated
#'   \code{(year, period, id)} combinations with their row counts
#'   (\code{n_rows}); zero rows when the ID is unique in every wave.
#'
#' @importFrom rlang .data
#' @export
check_unique_id_per_period <- function(data, id_col, label = id_col) {
  dups <- data |>
    dplyr::count(year, period, id = .data[[id_col]], name = "n_rows") |>
    dplyr::filter(n_rows > 1)

  if (nrow(dups) > 0) {
    n_waves <- dplyr::n_distinct(dups$year, dups$period)
    warning(
      glue::glue(
        "{id_col} is NOT unique within period in {label}: {nrow(dups)} ",
        "duplicated id-period combination(s) across {n_waves} wave(s). The ",
        "join keyed on it will fan out and inflate every downstream count. ",
        "Revisit the ID definition."
      ),
      call. = FALSE
    )
  } else {
    message(glue::glue("[ok] {id_col} is unique within every period in {label}."))
  }

  invisible(dups)
}
