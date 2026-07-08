#' Per-period diagnostics for two-tier person linkage
#'
#' Summarizes one linked period pair (t-3 -> t) produced by
#' [link_persons_tiered()] into a single row, combining person-level and
#' household-level linkage quality metrics.
#'
#' @section Units and denominators:
#' The columns mix three units of observation, each with its own
#' denominator. Shares are **not** comparable across blocks without
#' conversion:
#' \describe{
#'   \item{Person links}{one row of `linked` = one person matched across
#'     the two waves. Denominator: `n_linked`.}
#'   \item{Tier-2 (fuzzy) person links}{the subset of person links
#'     recovered by dwelling-level fuzzy matching. Denominator:
#'     `n_dwelling_fuzzy`.}
#'   \item{Household mappings}{one row of `support` = one observed
#'     (dwelling, hogar_prev, hogar_curr) correspondence, typically
#'     aggregating ~3 person links. Denominator: `n_hh_mappings`, except
#'     where noted.}
#' }
#'
#' @param linked Data frame. The `persons` element of
#'   [link_persons_tiered()] for a single period pair, after
#'   [augment_tier1_ids()] (tier-1 rows must carry dwelling/household
#'   IDs; otherwise household-level metrics silently undercount).
#' @param support Data frame. Output of [household_link_support()] for
#'   the same period pair, built from the same augmented `linked` input.
#'
#' @return A one-row tibble:
#' \describe{
#'   \item{period}{Character. Current-wave label of the period pair
#'     (`"YYYY-M"`).}
#'
#'   \item{n_linked}{Integer. Person links across the two waves, both
#'     tiers combined.}
#'   \item{n_id_verified}{Integer. Person links via unchanged
#'     `id_persona` passing sex/age verification (tier 1).}
#'   \item{n_dwelling_fuzzy}{Integer. Person links recovered by fuzzy
#'     matching within dwellings (tier 2).}
#'   \item{pct_fuzzy}{Share of *person links* from tier 2
#'     (`n_dwelling_fuzzy / n_linked`). Measures how much of the panel
#'     depends on fuzzy matching rather than INEC identifiers.}
#'
#'   \item{pct_fuzzy_ambiguous}{Share of *tier-2 links* whose selected
#'     match had a competing candidate within the ambiguity margin
#'     (demographic-twin risk). Conditional on tier 2: multiply by
#'     `pct_fuzzy` for the panel-wide exposure. `NA` when
#'     `n_dwelling_fuzzy == 0`.}
#'
#'   \item{n_hh_mappings}{Integer. Distinct (dwelling, hogar_prev,
#'     hogar_curr) mappings observed across all person links.}
#'   \item{pct_hh_conflicted}{Share of *household mappings* whose prev-
#'     or curr-household also appears in another mapping in the same
#'     dwelling — household splits, merges, or fuzzy false positives.}
#'   \item{pct_hh_renumbered}{Share of **non-conflicted** household
#'     mappings (note the restricted denominator) where the hogar
#'     number changed across waves. The unconditional measure of INEC
#'     household renumbering, with splits/merges excluded from both
#'     numerator and denominator.}
#'   \item{pct_hh_fuzzy_involved}{Share of *household mappings*
#'     containing at least one tier-2 person link — i.e., linked
#'     households that would have been partially or wholly lost using
#'     INEC IDs alone.}
#'   \item{med_coverage_prev}{Median across *household mappings* of
#'     `n_links / hh_size_prev`: for the typical linked household, the
#'     share of its previous-wave members that were person-linked.
#'     Values around 0.7–0.9 are typical; membership churn keeps it
#'     below 1.}
#'   \item{pct_weak_support}{Share of *household mappings* resting on a
#'     single person link that covers less than half the household on
#'     both sides (`n_links == 1` and
#'     `min(coverage_prev, coverage_curr) < 0.5`) — the size-aware flag
#'     for possibly coincidental links.}
#' }
#'
#' @seealso [link_persons_tiered()], [household_link_support()],
#'   [augment_tier1_ids()]
#'
#' @examples
#' \dontrun{
#' res     <- link_persons_tiered(enemdu_full_data, 2021, 7, 2021, 4)
#' persons <- augment_tier1_ids(res$persons, roster_prev)
#' support <- household_link_support(persons, roster_prev, roster_curr)
#' link_diagnostics_tiered(persons, support)
#' }
#'
#' @export
link_diagnostics_tiered <- function(linked, support) {

  tier2 <- dplyr::filter(linked, match_tier == "dwelling_fuzzy")
  clean <- dplyr::filter(support, !conflicted)

  dplyr::tibble(
    period = dplyr::first(linked$period),

    n_linked         = nrow(linked),
    n_id_verified    = sum(linked$match_tier == "id_verified"),
    n_dwelling_fuzzy = nrow(tier2),
    pct_fuzzy        = nrow(tier2) / nrow(linked),

    pct_fuzzy_ambiguous = if (nrow(tier2) == 0) NA_real_ else mean(tier2$ambiguous),

    n_hh_mappings     = nrow(support),
    pct_hh_conflicted = mean(support$conflicted),
    pct_hh_renumbered = mean(clean$renumbered),
    pct_hh_fuzzy_involved = mean(support$n_dwelling_fuzzy > 0),

    med_coverage_prev = stats::median(support$coverage_prev, na.rm = TRUE),
    pct_weak_support  = mean(
      pmin(support$coverage_prev, support$coverage_curr) < 0.5 &
        support$n_links == 1
    )
  )
}