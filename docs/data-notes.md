# Data Notes

## ENEMDU

The INEC's monthly ENEMDU microdata is distributed in two CSV versions: **"datos abiertos"** (raw open data) and **"datos abiertos recalculado"** (recalculated open data). The difference stems from a methodological disruption that occurred between 2020 and May 2021, during which changes to sample size, representativeness, and the construction of expansion factors broke the historical comparability of the series (INEC, 2024). Specifically, the expansion factors for that period were not calculated at the Primary Sampling Unit (PSU) level, as required by the standard methodology. To restore comparability, INEC recomputed the expansion factors using the traditional PSU-based weighting scheme for the affected months (September 2020–May 2021), releasing this corrected version as the *recalculado* file (INEC, 2021). **For any analysis involving time series or cross-period comparisons, the "datos abiertos recalculado" file should be used**, as it ensures consistency across the full historical series. The original file is only preferable when exact replication of figures published at the time of release is required.

**References**

- INEC. (2021). *Estadísticas Laborales – Empleo junio 2021*. Instituto Nacional de Estadística y Censos del Ecuador. https://www.ecuadorencifras.gob.ec/empleo-junio-2021/
- INEC. (2024). *ENEMDU 2024*. Instituto Nacional de Estadística y Censos del Ecuador. https://www.ecuadorencifras.gob.ec/enemdu-2024/