#' Download and extract ENOE microdata source files from INEGI
#'
#' Downloads the zipped ENOE (Encuesta Nacional de Ocupación y Empleo,
#' population aged 15 and over) microdata files published by INEGI for the
#' requested quarters, then extracts each archive into a per-period
#' subdirectory of \code{output_dir}. For each quarter it can retrieve the CSV
#' and/or DTA (Stata) archives. The correct file-naming convention is resolved
#' automatically, since INEGI has used three different schemes over time:
#' \itemize{
#'   \item 2005 Q1 - 2020 Q2: \code{2019trim4_csv.zip}
#'   \item 2020 Q3 - 2022 Q4: \code{enoe_n_2021_trim3_csv.zip}
#'   \item 2023 Q1 onward:     \code{enoe_2024_trim1_csv.zip}
#' }
#'
#' @param output_dir Character. Root directory where per-period subfolders are
#'   created. Created recursively if it does not exist.
#' @param periods Optional character vector of periods to download, each of the
#'   form \code{"YYYY_QQ"} where \code{QQ} is the quarter \code{01}-\code{04}
#'   (e.g. \code{c("2022_01", "2024_04")}). If supplied, \code{start_year} and
#'   \code{end_year} are ignored.
#' @param start_year Integer. First year to download when \code{periods} is not
#'   given. Defaults to \code{2023}.
#' @param end_year Integer. Last year to download when \code{periods} is not
#'   given. Defaults to the current calendar year.
#' @param formats Character vector of formats to download. Any of
#'   \code{"csv"} and \code{"dta"}. Defaults to both.
#' @param overwrite Logical. If \code{FALSE} (default), periods whose extracted
#'   folder already contains files are skipped.
#'
#' @return Invisibly, a character vector of the per-period directories that
#'   were populated.
#'
#' @examples
#' \dontrun{
#' download_source_enoe("enoe_data", periods = c("2022_01", "2024_04"))
#' download_source_enoe("enoe_data", formats = "csv")
#' }
#'
#' @export
download_source_enoe <- function(
  output_dir,
  periods    = NULL,
  start_year = 2023,
  end_year   = as.integer(format(Sys.Date(), "%Y")),
  formats    = c("csv", "dta"),
  overwrite  = FALSE
) {

  formats <- match.arg(formats, choices = c("csv", "dta"), several.ok = TRUE)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (is.null(periods)) {
    periods <- as.vector(outer(
      start_year:end_year,
      sprintf("%02d", 1:4),
      FUN = function(y, q) sprintf("%d_%s", y, q)
    ))
  }

  build_file_name <- function(year, quarter, fmt) {
    q <- as.integer(quarter)
    if (year >= 2023) {
      sprintf("enoe_%d_trim%d_%s.zip", year, q, fmt)
    } else if (year > 2020 || (year == 2020 && q >= 3)) {
      sprintf("enoe_n_%d_trim%d_%s.zip", year, q, fmt)
    } else {
      sprintf("%dtrim%d_%s.zip", year, q, fmt)
    }
  }

  url_exists <- function(url) {
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_headers("User-Agent" = .enoe_user_agent) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) < 400
  }

  populated <- character(0)

  for (period in periods) {

    parts   <- strsplit(period, "_", fixed = TRUE)[[1]]
    year    <- as.integer(parts[1])
    quarter <- parts[2]

    if (is.na(year) || !quarter %in% sprintf("%02d", 1:4)) {
      warning(sprintf("Skipping malformed period: '%s'", period))
      next
    }

    period_dir <- file.path(output_dir, period)

    for (fmt in formats) {

      label     <- sprintf("%s (%s)", period, fmt)
      file_name <- build_file_name(year, quarter, fmt)
      url       <- sprintf("%s/%s", .enoe_base_url, file_name)

      dest_dir <- file.path(period_dir, fmt)
      zipfile  <- file.path(period_dir, file_name)

      if (dir.exists(dest_dir) &&
          length(list.files(dest_dir)) > 0 && !overwrite) {
        message(sprintf("Skipping (already extracted): %s", label))
        populated <- c(populated, dest_dir)
        next
      }

      if (!url_exists(url)) {
        next
      }

      if (!dir.exists(period_dir)) dir.create(period_dir, recursive = TRUE)

      message(sprintf("Downloading: %s", label))
      if (!.enoe.download_file(url, zipfile)) {
        message(sprintf("[FAIL] %s: download failed", label))
        next
      }

      if (.enoe.unzip_and_clean(zipfile, dest_dir)) {
        message(sprintf("  extracted to %s", dest_dir))
        populated <- c(populated, dest_dir)
      } else {
        message(
          sprintf(
            "[FAIL] %s: could not extract (ZIP kept at %s)",
            label, zipfile
          )
        )
      }
    }
  }

  message(
    sprintf(
      "\nDone. %d folder(s) populated in %s",
      length(populated), output_dir
    )
  )
  invisible(populated)
}

#' Download a file with retries, streaming to disk
#'
#' Streams the response body to \code{<dest>.part} and renames to
#' \code{dest} only on success. Retries with linear backoff
#' (2s, 4s, 6s) up to \code{.enoe_retries} attempts.
#'
#' @param url Character URL to download.
#' @param dest Character destination file path.
#' @return Logical scalar: TRUE on success, FALSE otherwise.
#' @noRd
.enoe.download_file <- function(url, dest) {
  tmp <- paste0(dest, ".part")
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = .enoe_user_agent) |>
      httr2::req_timeout(.enoe_timeout) |>
      httr2::req_retry(max_tries = .enoe_retries, backoff = function(i) 2 * i) |>
      httr2::req_perform(path = tmp),
    error = function(e) {
      message("    download failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(resp) && httr2::resp_status(resp) == 200) {
    file.rename(tmp, dest)
    return(TRUE)
  }
  if (file.exists(tmp)) file.remove(tmp)
  FALSE
}


#' Extract a ZIP archive and remove it on success
#'
#' Unzips \code{zipfile} into \code{target_dir} (created recursively if
#' needed). If extraction succeeds, the original ZIP is deleted; otherwise it
#' is kept so the download is not lost.
#'
#' @param zipfile Character path to the ZIP archive.
#' @param target_dir Character directory to extract into.
#' @return Logical scalar: TRUE if extraction succeeded, FALSE otherwise.
#' @noRd
.enoe.unzip_and_clean <- function(zipfile, target_dir) {
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  extracted <- tryCatch(
    utils::unzip(zipfile, exdir = target_dir),
    warning = function(w) character(0),
    error   = function(e) character(0)
  )
  if (length(extracted) > 0) {
    if (file.exists(zipfile)) file.remove(zipfile)
    return(TRUE)
  }
  FALSE
}

# ---- Internal constants ------------------------------------------------------
.enoe_user_agent <- "Mozilla/5.0 (research data download)"
.enoe_timeout    <- 600
.enoe_retries    <- 3

.enoe_base_url <- paste0(
  "https://en.www.inegi.org.mx/contenidos/programas/",
  "enoe/15ymas/microdatos"
)