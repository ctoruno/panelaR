
#' Read raw survey microdata
#'
#' Reads every \code{.csv} file in \code{path_to_data}, infers which survey each
#' file belongs to from its file name, and loads it with survey-specific column
#' types. Files whose names match no known survey pattern are skipped with a
#' message. Each recognised file gains standardised \code{year}, \code{period},
#' and \code{quarter} columns.
#'
#' The survey is detected from the file name using these patterns:
#'   \describe{
#'     \item{\code{"enemdu"}}{Encuesta Nacional de Empleo, Desempleo y Subempleo
#'       — Ecuador (INEC). Files named \code{enemdu_persona_YYYY_MM.csv}.}
#'     \item{\code{"ene"}}{Encuesta Nacional de Empleo — Chile (INE). Files named
#'       \code{ene-YYYY-MM-MMM.csv}.}
#'     \item{\code{"enoe"}}{Encuesta Nacional de Ocupación y Empleo — Mexico
#'       (INEGI). Files named \code{ENOE_<TABLE>T<Q><YY>.csv}. (In progress.)}
#'   }
#'
#' This function expects the directory layout produced by
#' \code{\link{download_data}}: \code{path_to_data} should point at the survey
#' sub-folder (e.g. \code{data/raw/enemdu}) holding the CSV files.
#'
#' @param path_to_data Character. Path to the directory containing the raw
#'   \code{.csv} microdata files. Searched non-recursively.
#' @param col_types Extra column-type overrides, merged into the built-in
#'   defaults. Accepts either form:
#'   \describe{
#'     \item{a \code{readr::cols()} spec}{applied to every survey — the usual
#'       case, since \code{path_to_data} normally holds a single survey.}
#'     \item{a named list of \code{readr::cols()} specs}{keyed by survey
#'       (\code{"enemdu"}, \code{"ene"}, \code{"enoe"}), to override a
#'       particular survey only.}
#'   }
#'   User-supplied columns take precedence over the built-in default for the
#'   same column; every other default is preserved, and any column mentioned by
#'   neither is still type-guessed by \code{readr}. Defaults to \code{NULL}
#'   (built-in defaults only).
#'
#' @return A named list of data frames, one element per \code{.csv} file, named
#'   after the file without its \code{.csv} extension (e.g.
#'   \code{"enemdu_persona_2024_01"}). Files that match no known pattern
#'   contribute a \code{NULL} element. Each recognised data frame holds the raw
#'   microdata plus the derived columns \code{year} (integer), \code{period}
#'   (integer month), and \code{quarter} (character, e.g. \code{"Q1"}).
#'
#' @examples
#' \dontrun{
#' data <- read_data(path_to_data = "data/raw/enemdu")
#'
#' # Force an extra column to be read as character (applies to every survey)
#' enoe <- read_data(
#'   path_to_data = "data/raw/enoe",
#'   col_types = readr::cols(r_def = readr::col_character())
#' )
#'
#' # Override a single survey explicitly
#' data <- read_data(
#'   path_to_data = "data/raw",
#'   col_types = list(enemdu = readr::cols(area = readr::col_character()))
#' )
#' }
#'
#' @export
read_data <- function(
  path_to_data,
  col_types = NULL
){
  files <- list.files(path_to_data, pattern = "\\.csv$", full.names = FALSE)
  names_no_ext <- tools::file_path_sans_ext(files)

  message(
    sprintf("[info] Directory has a total of %d files", 
    length(files))
  )
  
  patterns <- c(
    enemdu = "^enemdu_(ambiental|armonia|confianza_delito|consumidor|persona|persona_tics|uso_tiempo|vivienda_hogar|vivienda_hogar_tics)_\\d{4}_\\d{2}$",
    ene    = "^ene-\\d{4}-\\d{2}-[A-Za-z]{3}$",
    enoe   = "^ENOE_(COE1|COE2|HOG|SDEM|VIV)T[1-4]\\d{2}$"
  )

  matches <- sapply(patterns, function(p) stringr::str_detect(names_no_ext, p))
  matched_pattern <- apply(matches, 1, function(row) {
    hit <- names(patterns)[row]
    if (length(hit) == 0) NA_character_ else hit[1]
  })

  files_to_read <- purrr::pmap(
    list(files, matched_pattern),
    \(f, s) {
      list(file_name = f, survey = s)
    }
  )
  names(files_to_read) <- names_no_ext

  # Columns that I want to read with a specific format
  surveys_col_types <- list(
    enemdu = readr::cols(
      ciudad = readr::col_character(),
      conglomerado = readr::col_character(),
      panelm = readr::col_character(),
      vivienda = readr::col_character(),
      hogar = readr::col_character(),
      id_vivienda = readr::col_character(),
      id_hogar = readr::col_character(),
      id_persona = readr::col_character(),
      periodo = readr::col_character(),
      fexp = readr::col_double()
    ),
    ene = readr::cols(
      r_p_c = readr::col_character(),
      estrato = readr::col_character(),
      conglomerado = readr::col_character(),
      id_identificacion = readr::col_character(),
      idrph = readr::col_character(),
      mig5_cod = readr::col_character(),
      b16_otro = readr::col_character(),
      fact_cal = readr::col_double()
    ),
    enoe = readr::cols(
      fac_tri = readr::col_double(),
      fac_men = readr::col_double(),
      .default = readr::col_character()
    )
  )

  user_col_types <- .normalise_col_types(col_types, names(surveys_col_types))

  raw_data_list <- lapply(
    files_to_read,
    \(file){

      file_name <- file[["file_name"]]

      if (is.na(file[["survey"]])){
        message(
          sprintf(
            "[info] File %s does not belong to neither of the expected patterns: enemdu, ene, enoe", 
            file_name
          )
        )
        return(NULL)
      }

      full_path <- file.path(
        path_to_data, file_name
      )

      fixed_col_types <- surveys_col_types[[ file[["survey"]] ]]

      # Merge any user-supplied overrides for this survey into the defaults.
      overrides <- user_col_types[[ file[["survey"]] ]]
      if (!is.null(overrides)) {
        merged_cols <- fixed_col_types$cols
        merged_cols[names(overrides$cols)] <- overrides$cols
        fixed_col_types <- do.call(readr::cols, merged_cols)
      }

      if (file[["survey"]] == "enemdu"){
        # m <- stringr::str_match(file_name, "^enemdu_persona_(\\d{4})_(\\d{2})\\.csv$")
        # year  <- as.integer(m[2])
        # month <- as.integer(m[3])
        # month_str <- m[3]

        raw_data <- readr::read_delim(
          full_path,
          delim = ";",
          guess_max = 5000,
          show_col_types = FALSE,
          col_types = fixed_col_types
        )

        raw_data_with_std_vars <- raw_data |>
          dplyr::mutate(
            year = as.integer(stringr::str_sub(periodo, 1, 4)),
            period = as.integer(stringr::str_sub(periodo, 5, 6)),
            quarter = dplyr::case_when(
              period %in% c(1,2,3) ~ "Q1",
              period %in% c(4,5,6) ~ "Q2",
              period %in% c(7,8,9) ~ "Q3",
              period %in% c(10,11,12) ~ "Q4"
            )
          )
      }

      if (file[["survey"]] == "ene"){
        # m <- stringr::str_match(file_name, "^ene-(\\d{4})-(\\d{2})-[A-Za-z]{3}\\.csv$")
        # year  <- as.integer(m[2])
        # month <- as.integer(m[3])
        # month_str <- m[3]

        raw_data <- readr::read_delim(
          full_path,
          delim = ";",
          guess_max = 5000,
          show_col_types = FALSE,
          col_types = fixed_col_types
        )
        
        raw_data_with_std_vars <- raw_data |>
          dplyr::mutate(
            year = as.integer(ano_trimestre),
            period = as.integer(mes_central),
            quarter = dplyr::case_when(
              period %in% c(1,2,3) ~ "Q1",
              period %in% c(4,5,6) ~ "Q2",
              period %in% c(7,8,9) ~ "Q3",
              period %in% c(10,11,12) ~ "Q4"
            )
          )
      }

      if (file[["survey"]] == "enoe"){
        # m <- stringr::str_match(file_name, "^ENOE_(?:COE1|COE2|HOGT|SDEM|VIV)(T[1-4])(\\d{2})\\.csv$")
        # year  <- as.integer(m[3])
        # trimester <- as.integer(stringr::str_sub(m[2],2))
        
        raw_data <- readr::read_csv(
          full_path,
          guess_max = 5000,
          show_col_types = FALSE,
          col_types = fixed_col_types
        )
        
        raw_data_with_std_vars <- raw_data |>
          dplyr::mutate(
            year = as.integer(stringr::str_sub(per,2,3))+2000,
            period = as.integer(stringr::str_sub(per,1,1)),
            quarter = dplyr::case_when(
              period %in% c(1) ~ "Q1",
              period %in% c(2) ~ "Q2",
              period %in% c(3) ~ "Q3",
              period %in% c(4) ~ "Q4"
            )
          )
      }

      message(sprintf("[--  ] %s: File successfully loaded", file_name))
      return(raw_data_with_std_vars)
  })

  return(raw_data_list)
}


# Normalise the user-facing `col_types` argument into a list with one entry per
# known survey (NULL where the user supplied no override). Accepts a bare
# readr::cols() spec (applied to every survey) or a named list of specs keyed by
# survey, and errors on anything else rather than silently ignoring it.
.normalise_col_types <- function(col_types, known_surveys) {
  empty <- stats::setNames(vector("list", length(known_surveys)), known_surveys)

  if (is.null(col_types)) {
    return(empty)
  }

  if (inherits(col_types, "col_spec")) {
    return(stats::setNames(
      rep(list(col_types), length(known_surveys)),
      known_surveys
    ))
  }

  if (!is.list(col_types) || is.null(names(col_types)) || !all(nzchar(names(col_types)))) {
    stop(
      "`col_types` must be a readr::cols() spec (applied to every survey), or a ",
      "named list of readr::cols() specs keyed by survey (",
      paste(known_surveys, collapse = ", "), ").",
      call. = FALSE
    )
  }

  unknown <- setdiff(names(col_types), known_surveys)
  if (length(unknown) > 0) {
    stop(
      sprintf(
        "Unknown survey name(s) in `col_types`: %s. Expected one of: %s.",
        paste(unknown, collapse = ", "),
        paste(known_surveys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  not_spec <- names(col_types)[
    !vapply(col_types, inherits, logical(1), "col_spec")
  ]
  if (length(not_spec) > 0) {
    stop(
      sprintf(
        "Each `col_types` entry must be a readr::cols() spec; these are not: %s.",
        paste(not_spec, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  empty[names(col_types)] <- col_types
  empty
}
