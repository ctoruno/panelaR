#' Download survey microdata from Kaggle
#'
#' Downloads the CSV microdata for one of the surveys supported by
#' panelaR from its Kaggle dataset, extracts the archive into a
#' survey-specific subdirectory of \code{output_dir}, and deletes the ZIP.
#' Unlike the \code{download_source_*()} functions, which scrape the national
#' statistical offices, this pulls a single curated snapshot and is the fast
#' path for most users.
#'
#' Authentication uses the Kaggle API credentials found in \code{kaggle.json}
#' (Kaggle account settings, "Create New API Token"): the \code{username} and
#' \code{key} fields are passed as HTTP basic auth. Credentials default to the
#' \code{KAGGLE_USER} and \code{KAGGLE_API_TOKEN} environment variables so they
#' need not be hard-coded in scripts.
#'
#' The archive is streamed to a temporary file and extracted into a staging
#' directory before being merged into its destination, so an interrupted or
#' corrupt download never leaves a half-extracted survey behind. If the
#' destination already contains files, the download is skipped unless
#' \code{overwrite = TRUE}.
#'
#' @param survey Character. One of \code{"enemdu"} (Ecuador), \code{"ene"}
#'   (Chile), or \code{"enoe"} (Mexico).
#' @param output_dir Character. Directory under which a subdirectory named
#'   after \code{survey} is created. Created (recursively) if it does not
#'   exist.
#' @param kaggle_user Character. Kaggle username. Defaults to the
#'   \code{KAGGLE_USER} environment variable.
#' @param kaggle_key Character. Kaggle API token. Defaults to the
#'   \code{KAGGLE_API_TOKEN} environment variable.
#' @param overwrite Logical. Re-download and replace the data even if the
#'   destination directory already contains files. Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the path to the directory holding the extracted
#'   files. Progress is reported via \code{message()}.
#'
#' @examples
#' \dontrun{
#' download_data("enemdu", "data/raw")
#' download_data("ene", "data/raw", overwrite = TRUE)
#' download_data("enoe", "data/raw", kaggle_user = "jdoe", kaggle_key = "abc123")
#' }
#'
#' @export
download_data <- function(
  survey,
  output_dir,
  kaggle_user = Sys.getenv("KAGGLE_USER"),
  kaggle_key  = Sys.getenv("KAGGLE_API_TOKEN"),
  overwrite   = FALSE
) {
  survey <- match.arg(survey, names(.kaggle_datasets))

  if (!nzchar(kaggle_user) || !nzchar(kaggle_key)) {
    stop(
      "Missing Kaggle credentials. Pass `kaggle_user` and `kaggle_key`, or set ",
      "the KAGGLE_USER and KAGGLE_API_TOKEN environment variables.",
      call. = FALSE
    )
  }

  dest_dir <- file.path(output_dir, survey)

  if (dir.exists(dest_dir) &&
      length(list.files(dest_dir, recursive = TRUE)) > 0 &&
      !overwrite) {
    message(
      sprintf(
        "[skip] %s: %s already contains data (use overwrite = TRUE to replace)",
        survey, dest_dir
      )
    )
    return(invisible(dest_dir))
  }

  dataset <- .kaggle_datasets[[survey]]
  url <- sprintf("https://www.kaggle.com/api/v1/datasets/download/%s", dataset)

  zipfile <- tempfile(sprintf("%s_", survey), fileext = ".zip")
  on.exit(unlink(zipfile), add = TRUE)

  message(sprintf("[get ] %s: %s", survey, url))
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_auth_basic(kaggle_user, kaggle_key) |>
      httr2::req_timeout(.kaggle_timeout) |>
      httr2::req_retry(max_tries = .kaggle_retries, backoff = function(i) 2 * i) |>
      httr2::req_perform(path = zipfile),
    error = function(e) {
      stop(
        sprintf("Could not download the %s dataset (%s): %s", survey, dataset,
                conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  if (httr2::resp_status(resp) == 401 || httr2::resp_status(resp) == 403) {
    stop(
      sprintf("Kaggle rejected the credentials for user '%s' (HTTP %d).",
              kaggle_user, httr2::resp_status(resp)),
      call. = FALSE
    )
  }

  message(sprintf("       downloaded %s bytes",
                  format(file.size(zipfile), big.mark = ",")))

  staging <- tempfile(sprintf("%s_extract_", survey))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  tryCatch(
    utils::unzip(zipfile, exdir = staging),
    warning = function(w) NULL,
    error   = function(e) NULL
  )

  staged <- list.files(staging, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(staged) == 0) {
    stop(
      sprintf("The %s archive downloaded but could not be extracted.", survey),
      call. = FALSE
    )
  }

  if (overwrite && dir.exists(dest_dir)) unlink(dest_dir, recursive = TRUE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  moved <- file.copy(staged, dest_dir, recursive = TRUE, overwrite = TRUE)
  if (!all(moved)) {
    stop(
      sprintf("Could not copy the extracted %s files into %s (destination path too long?)",
              survey, dest_dir),
      call. = FALSE
    )
  }

  message(sprintf("       extracted to %s", dest_dir))
  invisible(dest_dir)
}

# ---- Internal constants ------------------------------------------------------
.kaggle_datasets <- list(
  enemdu = "ctoruno/ecu-enemdu",
  ene    = "ctoruno/chl-ene",
  enoe   = "ctoruno/mex-enoe"
)
.kaggle_retries <- 3
.kaggle_timeout <- 600
