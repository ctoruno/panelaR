# Data Notes

## ENEMDU

The INEC's monthly ENEMDU microdata is distributed in two CSV versions: **"datos abiertos"** (raw open data) and **"datos abiertos recalculado"** (recalculated open data). The difference stems from a methodological disruption that occurred between 2020 and May 2021, during which changes to sample size, representativeness, and the construction of expansion factors broke the historical comparability of the series (INEC, 2024). Specifically, the expansion factors for that period were not calculated at the Primary Sampling Unit (PSU) level, as required by the standard methodology. To restore comparability, INEC recomputed the expansion factors using the traditional PSU-based weighting scheme for the affected months (September 2020–May 2021), releasing this corrected version as the *recalculado* file (INEC, 2021). **For any analysis involving time series or cross-period comparisons, the "datos abiertos recalculado" file should be used**, as it ensures consistency across the full historical series. The original file is only preferable when exact replication of figures published at the time of release is required.

**References**

- INEC. (2021). *Estadísticas Laborales – Empleo junio 2021*. Instituto Nacional de Estadística y Censos del Ecuador. https://www.ecuadorencifras.gob.ec/empleo-junio-2021/
- INEC. (2024). *ENEMDU 2024*. Instituto Nacional de Estadística y Censos del Ecuador. https://www.ecuadorencifras.gob.ec/enemdu-2024/


## What to write about every survey:

The description for each survey should be able to answer the following questions:

1. For how long has the survey been running in the country?
2. How often is data collected? Monthly? Quarters?
3. When was the last methodology or sample redesign? Also, since which year and month/quarter is the data comparable due to these redesigns?
4. If the last methodology/sample redesigned occured before or during the the global COVID pandemic, did the pandemic affect the data collection and/or comparability of the data? If yes, what is suggested to do and which months/quarters/years need to be considered?
5. Do we have any data issues to consider for the current series (since last methodological/survey update)? For example, the RECALCULADO issue witgh ENEMDU in Ecuador
6. What is the rotation scheme of the survey? Explain as clear as possible.
7. What is the observation unit for the rotation? Dwellings? Households? People?


Add quotes from the methodological documents for answering these questions whenever possible.