#' Per-period diagnostics for two-tier person linkage
#'
#' @param linked The `persons` element returned by `link_persons_tiered()`
#'   for a single period pair.
#' @param crosswalk The `households` element for the same period pair.
#'
#' @return One-row data frame.
#' @export
link_diagnostics_tiered <- function(linked, crosswalk) {

  tier2 <- dplyr::filter(linked, match_tier == "dwelling_fuzzy")

  dplyr::tibble(
    period            = dplyr::first(linked$period),
    n_linked          = nrow(linked),
    n_id_verified     = sum(linked$match_tier == "id_verified"),
    n_dwelling_fuzzy  = nrow(tier2),
    pct_fuzzy         = nrow(tier2) / nrow(linked),
    pct_fuzzy_ambiguous = if (nrow(tier2) == 0) NA_real_ else mean(tier2$ambiguous),
    pct_fuzzy_uncorroborated = if (nrow(tier2) == 0) NA_real_ else mean(tier2$n_hh_links == 1),
    n_hh_crosswalk    = nrow(crosswalk),
    pct_hh_renumbered = mean(crosswalk$renumbered)
  )
}