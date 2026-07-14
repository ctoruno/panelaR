#' Download ENE CSV files from the INE source
#'
#' Iterates over every rolling moving-quarter period between \code{start_year}
#' and \code{end_year} and downloads the corresponding CSV microdata file from
#' the Chilean National Statistics Institute (INE) public repository. Files
#' that are not yet published (e.g. future months of the current year) are
#' silently skipped after a HEAD check. Already-downloaded files are skipped
#' unless \code{overwrite = TRUE}.
#'
#' @param output_dir Character. Path to the directory where downloaded CSV
#'   files will be saved. Created recursively if it does not exist.
#' @param start_year Integer. First year to download. Defaults to \code{2020}.
#' @param end_year Integer. Last year to download. Defaults to the current
#'   calendar year.
#' @param overwrite Logical. If \code{FALSE} (default), files that already
#'   exist in \code{output_dir} are skipped. Set to \code{TRUE} to
#'   re-download and overwrite them.
#'
#' @return A character vector of file paths for all files that were downloaded
#'   or already present on disk, returned invisibly.
#'
#' @export
download_source_ene <- function(
  output_dir,
  start_year = 2020,
  end_year   = as.integer(format(Sys.Date(), "%Y")),
  overwrite  = FALSE
) {

  codes <- c("01-def", "02-efm", "03-fma", "04-mam", "05-amj", "06-mjj",
             "07-jja", "08-jas", "09-aso", "10-son", "11-ond", "12-nde")

  base_url <- paste0(
    "https://www.ine.gob.cl/docs/default-source/",
    "ocupacion-y-desocupacion/bbdd"
  )

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  url_exists <- function(url) {
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) < 400
  }

  downloaded <- character(0)

  for (year in start_year:end_year) {
    for (code in codes) {
      file_name <- sprintf("ene-%d-%s.csv", year, code)
      url       <- sprintf("%s/%d/csv/%s", base_url, year, file_name)
      dest      <- file.path(output_dir, file_name)

      if (file.exists(dest) && !overwrite) {
        message(sprintf("Skipping (already exists): %s", file_name))
        downloaded <- c(downloaded, dest)
        next
      }

      if (!url_exists(url)) {
        next  # File not found (maybe unpublished)
      }

      message(sprintf("Downloading: %s", file_name))
      httr2::request(url) |>
        httr2::req_progress() |>
        httr2::req_perform(path = dest)

      downloaded <- c(downloaded, dest)
    }
  }

  message(sprintf("\nDone. %d file(s) in %s", length(downloaded), output_dir))
  invisible(downloaded)
}
