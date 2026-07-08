#' Household-level support for cross-wave household mappings
#'
#' For every (dwelling, prev household, curr household) mapping observed in
#' the person-level matches, counts how many person links support it — split
#' by match tier — and expresses that support relative to household size in
#' each wave. This replaces the crude \code{n_hh_links} corroboration count,
#' which (i) ignored household size and (ii) counted only tier-2 links,
#' understating support for households where tier 1 recovered most members.
#'
#' Interpretation guide:
#' \itemize{
#'   \item \code{coverage_prev} / \code{coverage_curr}: share of the
#'     household (in each wave) accounted for by links supporting this
#'     mapping. Membership churn caps this below 1 even for true matches;
#'     ~0.5 on the smaller side is a reasonable acceptance floor, ideally
#'     calibrated against a placebo (impossible-overlap) match rate.
#'   \item \code{renumbered}: hogar number changed across waves. Note that a
#'     renumbered household mechanically has \code{n_id_verified = 0},
#'     because ENEMDU's \code{id_persona} embeds the hogar number.
#'   \item \code{conflicted}: the prev household maps to more than one curr
#'     household (or vice versa) — a split/merge or a false positive; treat
#'     renumbering claims from conflicted mappings with suspicion.
#' }
#'
#' @param persons Data frame of combined person matches (tier 1 + tier 2)
#'   with columns \code{id_dwelling}, \code{id_household_prev},
#'   \code{id_household_curr}, \code{match_tier}. Tier-1 rows must already
#'   carry their dwelling/household IDs — see
#'   \code{\link{augment_tier1_ids}}.
#' @param roster_prev,roster_curr Full rosters (all persons, not just the
#'   fuzzy residual) for the two waves, with columns \code{id_dwelling},
#'   \code{id_household}, \code{id_person}.
#'
#' @return One row per observed (dwelling, hogar_prev, hogar_curr) mapping
#'   with link counts by tier, household sizes, coverage shares, and the
#'   \code{renumbered} / \code{conflicted} flags.
#'
#' @export
household_link_support <- function(persons, roster_prev, roster_curr) {

  size_prev <- roster_prev |>
    dplyr::count(
      id_dwelling, id_household_prev = id_household,
      name = "hh_size_prev"
    )
  size_curr <- roster_curr |>
    dplyr::count(
      id_dwelling, id_household_curr = id_household,
      name = "hh_size_curr"
    )

  persons |>
    dplyr::mutate(
      match_tier = factor(
        match_tier,
        levels = c("id_verified", "dwelling_fuzzy")
      )
    ) |>
    dplyr::count(
      id_dwelling, id_household_prev, id_household_curr, match_tier,
      .drop = FALSE
    ) |>
    tidyr::pivot_wider(
      names_from = match_tier,
      values_from = n,
      values_fill = 0L,
      names_prefix = "n_",
      names_expand = TRUE
    ) |>
    dplyr::filter(n_id_verified + n_dwelling_fuzzy > 0) |>
    dplyr::mutate(n_links = n_id_verified + n_dwelling_fuzzy) |>
    dplyr::left_join(size_prev, by = c("id_dwelling", "id_household_prev")) |>
    dplyr::left_join(size_curr, by = c("id_dwelling", "id_household_curr")) |>
    dplyr::mutate(
      coverage_prev = n_links / hh_size_prev,
      coverage_curr = n_links / hh_size_curr,
      renumbered    = id_household_prev != id_household_curr
    ) |>
    dplyr::add_count(id_dwelling, id_household_prev, name = "n_map_prev") |>
    dplyr::add_count(id_dwelling, id_household_curr, name = "n_map_curr") |>
    dplyr::mutate(conflicted = n_map_prev > 1 | n_map_curr > 1) |>
    dplyr::select(-n_map_prev, -n_map_curr)
}

#' Attach dwelling/household IDs to tier-1 person matches
#'
#' \code{\link{verify_person_link}} returns only the respondent ID and
#' sociodemographics, so tier-1 rows in the output of
#' \code{\link{link_persons_tiered}} lack \code{id_dwelling} and the
#' household ID pair. Because a tier-1 match means \code{id_persona} was
#' identical in both waves — and ENEMDU's \code{id_persona} embeds the
#' dwelling and hogar numbers — the IDs are recoverable from the previous
#' wave's roster alone, and \code{id_household_curr == id_household_prev}
#' holds by construction.
#'
#' Survey-specific caveat: this recovery relies on ENEMDU's slot-encoded
#' \code{id_persona}. For surveys with a person key independent of household
#' numbering (e.g. Chile's ENE \code{idrph}), tier-1 IDs must instead be
#' joined from both rosters, since dwelling/household may legitimately
#' differ across waves for the same person.
#'
#' @param persons Output \code{persons} element of
#'   \code{\link{link_persons_tiered}}.
#' @param roster_prev Full previous-wave roster with \code{id_dwelling},
#'   \code{id_household}, \code{id_person}.
#'
#' @return \code{persons} with \code{id_dwelling},
#'   \code{id_household_prev}, \code{id_household_curr} populated on tier-1
#'   rows (tier-2 rows already carry them).
#'
#' @export
augment_tier1_ids <- function(persons, roster_prev) {

  tier1 <- persons |>
    dplyr::filter(match_tier == "id_verified") |>
    dplyr::select(
      -dplyr::any_of(c("id_dwelling", "id_household_prev", "id_household_curr"))
    ) |>
    dplyr::left_join(
      roster_prev |>
        dplyr::select(
          id_person_prev = id_person,
          id_dwelling,
          id_household_prev = id_household
        ),
      by = "id_person_prev"
    ) |>
    dplyr::mutate(id_household_curr = id_household_prev)

  dplyr::bind_rows(
    tier1,
    dplyr::filter(persons, match_tier == "dwelling_fuzzy")
  )
}