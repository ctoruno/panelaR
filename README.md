# panelaR <img src="man/figures/logo.svg" align="right" height="139" alt="" />

> Diagnose the panel (rotating) structure of Latin American labor-force surveys.

`panelaR` is an R package for **linking respondents across waves** of rotating-panel
household surveys and **measuring how well that linkage holds**. Many national
labor-force surveys re-interview the same dwellings over several waves, but the
resulting panel is rarely documented or validated: identifiers get reused,
households get renumbered, and roster lines get reshuffled. This package builds
the cross-wave links, verifies them against demographic evidence, and reports
diagnostics so you can judge whether a survey's panel matches its own design.

It currently targets three surveys:

| Survey | Country | Producer | Periodicity | Panel design |
|--------|---------|----------|-------------|--------------|
| **ENEMDU** | Ecuador | INEC  | Monthly / quarterly | Dwelling rotates out for two quarters — a respondent seen at *t* was last seen at *t‑3* **or** *t‑9* months, never both |
| **ENE**    | Chile   | INE   | Mobile quarters     | Single look-back — a respondent seen at *t* was last seen at *t‑3* months |
| **ENOE**   | Mexico  | INEGI | Quarterly           | Dwellings tracked across consecutive quarters — look-back of one quarter (*t‑1*) |

## Installation

```r
# install.packages("devtools")
devtools::install_github("ctoruno/panelaR")
```

Or, for local development, from the project root:

```r
devtools::load_all()
```

## The idea

For any two waves (a *current* wave and a *previous* wave), linkage runs in two tiers:

1. **Tier 1 — ID-verified.** Trust the survey's own person identifier, but only
   where a demographic check passes: same sex, and an age gap consistent with
   the elapsed time (over any sub-annual gap, reported age advances 0 or 1
   completed year). Relationship-to-head is reported but *not* required, since
   it can legitimately change between waves.

2. **Tier 2 — dwelling fuzzy residual.** For the respondents Tier 1 could not
   match, fall back to the **dwelling** as the only hard anchor and fuzzy-match
   within it, so household renumbering and roster reshuffling can't break the
   link. Household number, roster line and age gap become *soft* evidence in a
   match score rather than join keys.

Keeping the tiers separate (via a `match_tier` column) lets every downstream
statistic be computed with and without the fuzzy tier as a robustness check.

## Function reference

**Data acquisition & reading**

| Function | Purpose |
|----------|---------|
| `download_data()` | Pull a curated survey snapshot from Kaggle (the fast path) |
| `download_source_enemdu()`, `download_source_ene()`, `download_source_enoe()` | Scrape microdata directly from the national statistical offices |
| `read_data()` | Read raw CSVs, detect the survey from file names, add standardized `year` / `period` / `quarter` |

**Linkage**

| Function | Purpose |
|----------|---------|
| `estimate_overlap()` | Raw share of previous-wave IDs also seen in the current wave, before any verification |
| `verify_person_link()` | Tier 1: inner-join on the person ID, verified by sex + age gap |
| `match_residual_within_dwelling()` | Tier 2: fuzzy match the residual within each dwelling |
| `link_persons_tiered()` | Run both tiers and stack them |
| `augment_tier1_ids()` | Recover dwelling/household IDs onto Tier‑1 rows |
| `derive_household_crosswalk()` | Majority-vote map of prev → curr household numbers (detects renumbering) |
| `household_link_support()` | Household-level support for each mapping, relative to household size |

**Diagnostics**

| Function | Purpose |
|----------|---------|
| `link_diagnostics()` | One-row per-wave summary of Tier‑1 match quality |
| `link_diagnostics_tiered()` | One-row per-wave summary combining person- and household-level metrics |

**Wave utilities** (in `R/diagnosis_utils.R`)

| Function | Purpose |
|----------|---------|
| `shift_t()` | Shift a `(year, period)` wave backwards by *n* months or quarters, rolling across the year boundary |
| `has_wave()` | Test whether a `(year, period)` pair exists in a reference set |
| `ids_in_wave()` | Distinct IDs present in a single wave |
| `roster_of()` | Extract a dwelling/household/person roster for one wave, with standardized column names |
| `check_unique_id_per_period()` | Data-quality guard: warn if an ID is not unique within a wave (every join assumes it is) |

## Diagnostic workflow

Each survey has an end-to-end diagnostic script under [`scripts/`](scripts/) —
[`diagnosis_enemdu.R`](scripts/diagnosis_enemdu.R),
[`diagnosis_ene.R`](scripts/diagnosis_ene.R),
[`diagnosis_enoe.R`](scripts/diagnosis_enoe.R). They all follow the same shape:

1. **Read & pool** the survey's CSVs, building panel keys where the survey
   ships none (ENOE concatenates dwelling/household/person identifiers).
2. **Input sanity** — `check_unique_id_per_period()` confirms the person key is
   unique within every wave; a fan-out here would silently inflate every count
   downstream.
3. **Build the wave grid** — enumerate `(current wave, previous wave)` pairs
   from the survey's look-back design with `shift_t()` + `has_wave()`.
   (ENEMDU also asserts its *t‑3* / *t‑9* rotation actually partitions the
   sample before stacking the two lags.)
4. **Estimate raw overlap** with `estimate_overlap()`.
5. **Link** — Tier 1 for ENE (no household renumbering to recover), full two-tier
   for ENEMDU and ENOE.
6. **Write diagnostics** to `outputs/diagnostics/`.

A script is driven by a single environment variable pointing at the data root:

```r
Sys.setenv(PATH_TO_DATA = "path/to/data")
source("scripts/diagnosis_enemdu.R")
```

## Project layout

```
R/                     package functions (see reference above)
scripts/               per-survey end-to-end diagnostic scripts
vignettes/articles/    background on the surveys and how to download data
outputs/diagnostics/   CSV diagnostics written by the scripts
man/                   generated documentation
```

## Learn more

- `vignette("about-the-surveys")` — the three surveys and their panel designs
- `vignette("downloading-data")` — obtaining the microdata

## Notes

Work in progress; the ENOE reader and its two-tier diagnostic are the most
recently added and least battle-tested.
