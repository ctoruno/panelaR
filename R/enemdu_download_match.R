#' Download ENEMDU Matriz de Transicion Laboral matched bases (post-redesign)
#'
#' Downloads INEC's official quarterly matched bases (Base Match / MTL),
#' which link the same dwellings and persons across the same quarter of
#' consecutive years (e.g. T4 2022 - T4 2023), following the annual
#' revisit implied by the 2(2)2 rotation design. Each ZIP is extracted
#' into a "match_T{q}_{y1}_{y2}" subdirectory and deleted. Also downloads
#' the accompanying longitudinal sampling-design PDF when available.
#'
#' @param output_dir Character. Directory for the extracted matched bases.
#' @param start_end_year Integer. First *end year* of the match pairs to
#'   try. Defaults to 2023 (the first post-redesign release pairs
#'   quarters of 2022 with 2023).
#'
#' @return Invisibly returns NULL.
#' @export
enemdu_download_match <- function(output_dir, start_end_year = 2023) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  romans   <- c("I", "II", "III", "IV")
  end_year <- as.integer(format(Sys.Date(), "%Y"))

  for (y2 in start_end_year:end_year) {
    y1 <- y2 - 1L
    for (q in 1:4) {
      folder <- sprintf("%s/%d/Matrices_%s_Trimestre",
                        .enemdu_base, y2, romans[q])
      zipname <- sprintf("02_BDD_ENEMDU_MATCH_T%d_%d_T%d_%d_CSV.zip",
                         q, y1, q, y2)
      url     <- sprintf("%s/%s", folder, zipname)
      label   <- sprintf("T%d %d - T%d %d", q, y1, q, y2)
      mes_dir <- file.path(output_dir, sprintf("match_T%d_%d_%d", q, y1, y2))
      zipfile <- file.path(output_dir, zipname)

      if (dir.exists(mes_dir) &&
          length(list.files(mes_dir, recursive = TRUE)) > 0) {
        message(sprintf("[skip] %s: already extracted", label))
        next
      }

      if (!.enemdu.url_exists(url)) {
        message(sprintf("[--  ] %s: not published / not found", label))
        next
      }

      message(sprintf("[get ] %s: %s", label, url))
      if (!.enemdu.download_file(url, zipfile)) {
        message(sprintf("[FAIL] %s: could not download", label))
        next
      }

      if (.enemdu.unzip_and_clean(zipfile, mes_dir)) {
        message(sprintf("       extracted to %s", mes_dir))
      } else {
        message(sprintf("[FAIL] %s: could not extract (ZIP kept)", label))
        next
      }

      # Longitudinal sampling-design PDF (best effort; naming uses the
      # Roman numeral, e.g. ..._TIV_2022_2023.pdf)
      pdf_url <- sprintf(
        "%s/Disenio_muestral_ENEMDU_Trimestral_Longitudinal_T%s_%d_%d.pdf",
        folder, romans[q], y1, y2
      )
      pdf_dest <- file.path(mes_dir, basename(pdf_url))
      if (.enemdu.url_exists(pdf_url)) {
        .enemdu.download_file(pdf_url, pdf_dest)
      }

      Sys.sleep(1)
    }
  }

  message("\nDone.")
  invisible(NULL)
}