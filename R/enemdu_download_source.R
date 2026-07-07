#' Download and extract ENEMDU "Datos Abiertos" monthly files from INEC
#'
#' Downloads every monthly ENEMDU (Encuesta Nacional de Empleo, Desempleo y
#' Subempleo) "Datos Abiertos" CSV ZIP published by INEC on Ecuador en Cifras,
#' from \code{start_year} up to (and including) the current calendar month.
#' Each ZIP is extracted into its own \code{YYYY_MM} subdirectory of
#' \code{output_dir} and the ZIP is deleted afterwards. Months not yet
#' published are skipped, and months whose extraction folder already exists
#' (non-empty) are not re-downloaded, so the function is safe to re-run
#' incrementally.
#'
#' Downloads are streamed to a temporary \code{.part} file, and the ZIP is
#' deleted only after successful extraction, so an interrupted run never
#' leaves a month in a corrupt half-extracted state that would be skipped
#' on the next run.
#'
#' @param output_dir Character. Directory under which one \code{YYYY_MM}
#'   subdirectory per month is created. Created (recursively) if it does
#'   not exist.
#' @param start_year Integer. First year to download. Defaults to
#'   \code{2021}, the first year of the post-redesign ENEMDU series;
#'   earlier years use different folder conventions on the INEC site and
#'   will not resolve with this URL pattern.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   downloading and extracting files; progress is reported via
#'   \code{message()}.
#'
#' @examples
#' \dontrun{
#' enemdu_download_datos_abiertos("data/raw/enemdu")
#' enemdu_download_datos_abiertos("data/raw/enemdu", start_year = 2024)
#' }
#'
#' @export
enemdu_download_source <- function(output_dir, start_year = 2021) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  end_year  <- as.integer(format(Sys.Date(), "%Y"))
  end_month <- as.integer(format(Sys.Date(), "%m"))

  for (year in start_year:end_year) {
    last_month <- if (year == end_year) end_month else 12L

    for (month in seq_len(last_month)) {
      urls <- .enemdu.build_url(year, month)
      url  <- NULL
      for (u in urls) {
        if (.enemdu.url_exists(u)) {
          url <- u
          break
        }
      }
      mes_dir <- file.path(output_dir, sprintf("%d_%02d", year, month))
      zipfile <- file.path(
        output_dir,
        sprintf("ENEMDU_DATOS_ABIERTOS_%d_%02d_CSV.zip", year, month)
      )
      label <- sprintf("%s %d", .enemdu_meses[month], year)

      if (dir.exists(mes_dir) &&
          length(list.files(mes_dir, recursive = TRUE)) > 0) {
        message(sprintf("[skip] %s: already extracted", label))
        next
      }

      if (is.null(url)) {
        message(sprintf("[--  ] %s: not published yet / not found", label))
        next
      }

      message(sprintf("[get ] %s: %s", label, url))
      if (!.enemdu.download_file(url, zipfile)) {
        message(sprintf("[FAIL] %s: could not download", label))
        next
      }
      message(sprintf("       saved %s (%s bytes)",
                      zipfile, format(file.size(zipfile), big.mark = ",")))

      if (.enemdu.unzip_and_clean(zipfile, mes_dir)) {
        message(sprintf("       extracted to %s", mes_dir))
      } else {
        message(sprintf("[FAIL] %s: could not extract (ZIP kept at %s)",
                        label, zipfile))
      }

      Sys.sleep(1)
    }
  }

  message("\nDone.")
  invisible(NULL)
}

#' Build candidate INEC download URLs for a given year and month
#'
#' Two sources of naming variation exist on the INEC server:
#' (a) the month folder separator changed from "Mes-Año" (hyphen, used
#'     through mid-2022) to "Mes_Año" (underscore, used since); and
#' (b) some months (notably the recalculated Sep 2020 - May 2021 series)
#'     are published with a "_RECALCULADO" suffix in the filename.
#' Returns all four combinations, most likely candidates first; the
#' caller should use the first URL that resolves.
#'
#' @param year Integer year.
#' @param month Integer month (1-12).
#' @return Character vector of candidate URLs.
#' @noRd
.enemdu.build_url <- function(year, month) {
  mes <- .enemdu_meses[month]
  mm  <- sprintf("%02d", month)

  filenames <- c(
    sprintf("2_BDD_DATOS_ABIERTOS_ENEMDU_%d_%s_CSV.zip", year, mm),
    sprintf("2_BDD_DATOS_ABIERTOS_ENEMDU_%d_%s_CSV_RECALCULADO.zip", year, mm)
  )
  seps <- c("_", "-")

  combos <- expand.grid(
    sep = seps, 
    filename = filenames,
    stringsAsFactors = FALSE
  )

  sprintf(
    "%s/%d/%s%s%d/%s",
    .enemdu_base, year, mes, combos$sep, year, combos$filename
  )
}

#' Check whether a URL is published (HEAD request)
#'
#' Returns FALSE both for HTTP errors (e.g. 404 for unpublished months)
#' and for network-level failures, mirroring the behavior of a simple
#' existence probe.
#'
#' @param url Character URL to check.
#' @return Logical scalar.
#' @noRd
.enemdu.url_exists <- function(url) {
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = .enemdu_user_agent) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  !is.null(resp) && httr2::resp_status(resp) == 200
}

#' Download a file with retries, streaming to disk
#'
#' Streams the response body to \code{<dest>.part} and renames to
#' \code{dest} only on success. Retries with linear backoff
#' (2s, 4s, 6s) up to \code{.enemdu_retries} attempts.
#'
#' @param url Character URL to download.
#' @param dest Character destination file path.
#' @return Logical scalar: TRUE on success, FALSE otherwise.
#' @noRd
.enemdu.download_file <- function(url, dest) {
  tmp <- paste0(dest, ".part")
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = .enemdu_user_agent) |>
      httr2::req_timeout(.enemdu_timeout) |>
      httr2::req_retry(max_tries = .enemdu_retries, backoff = function(i) 2 * i) |>
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

#' Extract a ZIP into a target directory and delete the ZIP on success
#'
#' Extracts into the target directory and verifies that at least one file
#' was produced before deleting the ZIP. If extraction fails, the ZIP is
#' kept (for manual inspection) and any half-created extraction directory
#' is removed so the month is retried on the next run.
#'
#' @param zipfile Character path to the ZIP file.
#' @param exdir Character extraction directory.
#' @return Logical scalar: TRUE on success, FALSE otherwise.
#' @noRd
.enemdu.unzip_and_clean <- function(zipfile, exdir) {
  extracted <- tryCatch(
    utils::unzip(zipfile, exdir = exdir),
    warning = function(w) character(0),  # unzip signals corrupt ZIPs as warnings
    error   = function(e) character(0)
  )
  if (length(extracted) > 0) {
    file.remove(zipfile)
    return(TRUE)
  }
  # Failed extraction: remove any partial output so the skip check
  # doesn't treat this month as done, but keep the ZIP for inspection
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE)
  FALSE
}

# ---- Internal constants ------------------------------------------------------
.enemdu_base <- "https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO"
.enemdu_meses <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)
.enemdu_user_agent <- "Mozilla/5.0 (research data downloader)"
.enemdu_retries    <- 3
.enemdu_timeout    <- 120