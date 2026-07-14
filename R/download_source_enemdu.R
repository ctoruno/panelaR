#' Download and extract ENEMDU monthly microdata files from INEC
#'
#' Downloads every monthly ENEMDU (Encuesta Nacional de Empleo, Desempleo y
#' Subempleo) microdata ZIP published by INEC on Ecuador en Cifras, from
#' \code{start_year} up to (and including) the current calendar month. For
#' each month the requested \code{format}(s) are fetched: the "Datos
#' Abiertos" CSV package, the SPSS package, or both. Each ZIP is extracted
#' into a shared \code{YYYY_MM} subdirectory of \code{output_dir} and the ZIP
#' is deleted afterwards. Months not yet published are skipped, and a format
#' whose extracted files already exist is not re-downloaded, so the function
#' is safe to re-run incrementally.
#'
#' Downloads are streamed to a temporary \code{.part} file, and each ZIP is
#' extracted into a staging directory that is merged into its \code{YYYY_MM}
#' folder only after a successful, complete extraction. An interrupted or
#' corrupt run therefore never leaves a month half-extracted, and never
#' disturbs a format already downloaded into the same shared folder.
#'
#' @param output_dir Character. Directory under which one \code{YYYY_MM}
#'   subdirectory per month is created. Created (recursively) if it does
#'   not exist.
#' @param start_year Integer. First year to download. Defaults to
#'   \code{2021}, the first year of the post-redesign ENEMDU series;
#'   earlier years use different folder conventions on the INEC site and
#'   will not resolve with this URL pattern.
#' @param format Character. One or both of \code{"csv"} and \code{"spss"},
#'   selecting which microdata package(s) to download. Defaults to both.
#'   When both are requested, the CSV (\code{.csv}) and SPSS (\code{.sav})
#'   files share each month's \code{YYYY_MM} folder.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   downloading and extracting files; progress is reported via
#'   \code{message()}.
#'
#' @examples
#' \dontrun{
#' download_source_enemdu("data/raw/enemdu")
#' download_source_enemdu("data/raw/enemdu", start_year = 2024)
#' download_source_enemdu("data/raw/enemdu", format = "spss")
#' }
#'
#' @export
download_source_enemdu <- function(
  output_dir, 
  start_year = 2021,
  format = c("csv", "spss")
) {
  formats <- match.arg(format, several.ok = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  end_year  <- as.integer(format(Sys.Date(), "%Y"))
  end_month <- as.integer(format(Sys.Date(), "%m"))

  for (year in start_year:end_year) {
    last_month <- if (year == end_year) end_month else 12L
    for (month in seq_len(last_month)) {
      mes_dir <- file.path(output_dir, sprintf("%d_%02d", year, month))

      for (fmt in formats) {
        label <- sprintf("%s %d (%s)", .enemdu_meses[month], year, toupper(fmt))
        ext   <- .enemdu_ext[[fmt]]

        if (dir.exists(mes_dir) &&
            length(list.files(mes_dir, pattern = sprintf("\\.%s$", ext),
                              recursive = TRUE, ignore.case = TRUE)) > 0) {
          message(sprintf("[skip] %s: already extracted", label))
          next
        }

        urls <- .enemdu.build_url(year, month, fmt)
        url  <- NULL
        for (u in urls) {
          if (.enemdu.url_exists(u)) {
            url <- u
            break
          }
        }

        if (is.null(url)) {
          message(sprintf("[--  ] %s: not published yet / not found", label))
          next
        }

        zipfile <- file.path(
          output_dir,
          sprintf("ENEMDU_%s_%d_%02d.zip", toupper(fmt), year, month)
        )

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
          message(
            sprintf(
              "[FAIL] %s: could not extract (ZIP kept at %s)",
              label, zipfile
            )
          )
        }

        Sys.sleep(1)
      }
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
.enemdu.build_url <- function(year, month, format = "csv") {
  mes <- .enemdu_meses[month]
  mm  <- sprintf("%02d", month)

  filenames <- if (format == "spss") {
    c(
      sprintf("1_BDD_ENEMDU_%d_%s_SPSS.zip", year, mm),
      sprintf("1_BDD_ENEMDU_%d_%s_SPSS_RECALCULADO.zip", year, mm)
    )
  } else {
    c(
      sprintf("2_BDD_DATOS_ABIERTOS_ENEMDU_%d_%s_CSV.zip", year, mm),
      sprintf("2_BDD_DATOS_ABIERTOS_ENEMDU_%d_%s_CSV_RECALCULADO.zip", year, mm)
    )
  }

  seps <- c("_", "-")
  combos <- expand.grid(sep = seps, filename = filenames, stringsAsFactors = FALSE)

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

#' Extract a ZIP into a shared target directory and delete the ZIP on success
#'
#' Extracts into a staging directory under \code{tempdir()} and only merges
#' the result into \code{exdir} once a non-empty extraction is confirmed,
#' then deletes the ZIP. Staging in the (short) session temp directory
#' matters on Windows: \code{output_dir} typically lives under a long
#' OneDrive path, and extracting in place can push member paths past the
#' 260-character MAX_PATH limit, where extraction fails silently unless
#' long-path support is enabled system-wide. Extraction tries R's internal
#' reader first and falls back to system extractors (bsdtar, then unzip)
#' for archives the internal reader cannot handle, such as the INEC SPSS
#' ZIPs with CP437-encoded member names. Because \code{exdir} is shared
#' between the CSV and SPSS formats, a failed or corrupt extraction must not
#' touch it: on failure only the staging directory is removed (leaving any
#' other format already present intact) and the ZIP is kept for inspection.
#'
#' @param zipfile Character path to the ZIP file.
#' @param exdir Character extraction directory (the shared \code{YYYY_MM} dir).
#' @return Logical scalar: TRUE on success, FALSE otherwise.
#' @noRd
.enemdu.unzip_and_clean <- function(zipfile, exdir) {
  staging <- tempfile("enemdu_extract_")
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  # R's internal reader aborts the WHOLE extraction with "invalid multibyte
  # string" when any member name is non-UTF-8 -- the INEC SPSS ZIPs ship a
  # PDF guide with a CP437-encoded name ("Guía de usuario") -- and, worse,
  # that error leaks the open ZIP handle for the rest of the R session, so
  # the file can never be deleted afterwards. Pre-check the member names
  # (list = TRUE does not error on them) and only let the internal reader
  # touch archives it can handle; everything else goes straight to system
  # extractors invoked via system2(): bsdtar (ships with Windows 10+/macOS),
  # then an unzip binary (Linux, Git for Windows). utils::unzip's `unzip=`
  # argument is ignored on Windows, so it cannot be used for this. Success
  # is judged by files actually landing in the staging dir, not by return
  # values.
  member_names <- tryCatch(
    utils::unzip(zipfile, list = TRUE)$Name,
    warning = function(w) NULL,
    error   = function(e) NULL
  )
  if (!is.null(member_names) && all(validUTF8(member_names))) {
    tryCatch(
      utils::unzip(zipfile, exdir = staging),
      warning = function(w) NULL,  # unzip signals corrupt ZIPs as warnings
      error   = function(e) NULL
    )
  }
  if (length(list.files(staging, recursive = TRUE)) == 0 &&
      nzchar(Sys.which("tar"))) {
    suppressWarnings(system2(
      Sys.which("tar"), c("-xf", shQuote(zipfile), "-C", shQuote(staging)),
      stdout = FALSE, stderr = FALSE
    ))
  }
  if (length(list.files(staging, recursive = TRUE)) == 0 &&
      nzchar(Sys.which("unzip"))) {
    suppressWarnings(system2(
      Sys.which("unzip"), c("-o", "-q", shQuote(zipfile), "-d", shQuote(staging)),
      stdout = FALSE, stderr = FALSE
    ))
  }

  staged <- list.files(
    staging, 
    full.names = TRUE, 
    all.files = TRUE,
    no.. = TRUE
  )
  if (length(staged) == 0) {
    # Failed extraction: keep the ZIP for manual inspection. Shared month directory untouched.
    return(FALSE)
  }

  # Merge the staged files into the (possibly shared) month directory,
  # preserving the ZIP's internal layout. A FALSE from file.copy here most
  # likely means a destination path over the Windows MAX_PATH limit.
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  moved <- file.copy(staged, exdir, recursive = TRUE, overwrite = TRUE)

  if (all(moved)) {
    # Extraction succeeded even if the ZIP itself cannot be removed (e.g.
    # a lingering lock from antivirus scanning); leaving it behind is
    # harmless because the skip check looks at extracted files, not ZIPs.
    if (!suppressWarnings(file.remove(zipfile))) {
      message("       note: could not delete ", zipfile)
    }
    return(TRUE)
  }
  message("       could not copy extracted file(s) into ", exdir,
          " (destination path too long?)")
  FALSE
}

# ---- Internal constants ------------------------------------------------------
.enemdu_base <- "https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO"
.enemdu_meses <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)
# Extracted-file extension used to detect whether a given format is already
# present in a month's shared folder (the skip check in download_source_enemdu).
.enemdu_ext <- c(csv = "csv", spss = "sav")
.enemdu_user_agent <- "Mozilla/5.0 (research data downloader)"
.enemdu_retries    <- 3
.enemdu_timeout    <- 120