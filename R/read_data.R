
#' Read raw survey microdata
#'
#' Loads individual-level microdata from one of the supported Latin American
#' household labour surveys and returns a nested list organised by year and
#' month, with standardised \code{year}, \code{month}, and \code{quarter}
#' columns appended to each data frame.
#'
#' @param survey Character. Survey identifier (case-insensitive). One of:
#'   \describe{
#'     \item{\code{"eph"}}{Encuesta Permanente de Hogares — Argentina (INDEC)}
#'     \item{\code{"pnadc"}}{Pesquisa Nacional por Amostra de Domicílios Contínua — Brazil (IBGE)}
#'     \item{\code{"enoe"}}{Encuesta Nacional de Ocupación y Empleo — Mexico (INEGI)}
#'     \item{\code{"enemdu"}}{Encuesta Nacional de Empleo, Desempleo y Subempleo — Ecuador (INEC)}
#'     \item{\code{"ene"}}{Encuesta Nacional de Empleo — Chile (INE)}
#'   }
#' @param path_to_data Character. Path to the root directory that contains the
#'   country-specific sub-folders with the raw microdata files.
#'
#' @return A nested list of data frames: the outer list is indexed by year, the
#'   inner list by month. Each data frame contains the raw microdata for that
#'   period plus the derived columns \code{year} (integer), \code{month}
#'   (integer), and \code{quarter} (character, e.g. \code{"Q1"}).
#'
#' @examples
#' \dontrun{
#' data <- read_data(survey = "enemdu", path_to_data = "data/raw")
#' }
#'
#' @export
read_data <- function(
    survey,
    path_to_data,
    periods
){

    if (survey %in% c("enemdu")){

      raw_data_list <- lapply(
        periods,
        \(period) {

          year  <- as.integer(stringr::str_sub(period, 1, 4))
          month <- as.integer(stringr::str_sub(period, 6, 7))
          month_str <- stringr::str_sub(period, 6, 7)
          csv_file <- glue::glue("enemdu_persona_{year}_{month_str}.csv")

          file_path <- file.path(
            path_to_data,
            "ECU-ENEMDU", "source",
            # glue::glue("{year}_{month_str}"),
            csv_file
          )

          if (file.exists(file_path)) {

            raw_data <- readr::read_delim(
              file_path,
              delim = ";",
              guess_max = 5000,
              show_col_types = FALSE,
              col_types = readr::cols(
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
              )
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
            
            message(sprintf("[--  ] %s: File successfully loaded", csv_file))

          } else {
            message(sprintf("[--  ] %s: File not found", file_path))
            raw_data_with_std_vars <- NULL
          }

          raw_data_with_std_vars
        }
      )
    }
  
  return(raw_data_list)
}