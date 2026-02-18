# Replication: Firpo, Ponczek & Sanfelice (2015)

Replication of "The relationship between federal budget amendments and local electoral power" (*Journal of Development Economics*, 116: 186-198).

## Paper Summary

The paper studies two questions about Brazilian federal legislators and budget amendments (*emendas orçamentárias individuais*):

1. **Supply side**: Do legislators direct amendments to municipalities that supported them electorally?
2. **Demand side**: Do voters reward legislators who channel amendments to their municipality?

Key methodological contribution: defining "associated candidates" using the Herfindahl-Hirschman Index (HHI) to compute the effective number of candidates per municipality, since Brazil has no formal districts in its open-list proportional representation system.

## How to Run

### Prerequisites

1. **Google Cloud project** (free) for BigQuery access via Base dos Dados.
   - Create at: https://console.cloud.google.com/
   - Free tier: 1 TB of queries per month (more than enough)
   - Set your project ID: `Sys.setenv(BD_PROJECT_ID = "your-project-id")`

2. **R packages**:
```r
install.packages(c(
  "tidyverse", "data.table", "janitor", "here", "glue",
  "basedosdados",
  "fixest", "modelsummary", "kableExtra"
))
```

### Run scripts in order

```r
source("research/01_download_data.R")       # Query Base dos Dados (BigQuery)
source("research/02_clean_data.R")           # Clean and standardize
source("research/03_construct_variables.R")  # Build HHI, associated candidates, panel
source("research/04_analysis.R")             # Main regressions (original period)
source("research/05_extension.R")            # Extend to 2014-2022
```

## Data Sources (all via Base dos Dados)

All data is accessed through [Base dos Dados](https://basedosdados.org/) (BigQuery), which provides standardized `id_municipio` columns across all tables — no municipality code crosswalks needed.

| Data | BigQuery Table | Content |
|------|----------------|---------|
| Electoral results | `br_tse_eleicoes.resultados_candidato_municipio` | Deputado Federal votes by municipality |
| Candidate info | `br_tse_eleicoes.candidatos` | Elected status, party, etc. |
| Budget amendments | `br_cgu_emendas_parlamentares.microdados` | Individual amendments by legislator × municipality |
| Population | `br_ibge_populacao.municipio` | Municipal population estimates |
| Municipality directory | `br_bd_diretorios_brasil.municipio` | Names, states, codes |

**Original data sources**: TSE (Tribunal Superior Eleitoral), CGU/Portal da Transparência, IBGE.

### Fallbacks (if BigQuery is unavailable)

- Electoral data: `electionsBR` R package (downloads directly from TSE)
- Budget amendments: Manual download from [Portal da Transparência](https://portaldatransparencia.gov.br/emendas) or [SIGA Brasil](https://www12.senado.leg.br/orcamento/sigabrasil)
- Population: `sidrar` R package (IBGE API)

## Project Structure

```
research/
├── _common.R                  # Shared configuration, BigQuery setup, helpers
├── 01_download_data.R         # Data acquisition from Base dos Dados
├── 02_clean_data.R            # Data cleaning and standardization
├── 03_construct_variables.R   # Variable construction and panel assembly
├── 04_analysis.R              # Main regressions (original period)
├── 05_extension.R             # Extension to 2014-2022
├── README.md
├── .gitignore                 # Excludes data/ and output/
├── data/
│   ├── raw/                   # Cached BigQuery results (.rds)
│   └── clean/                 # Processed datasets
└── output/                    # Regression tables and figures
```

## Known Limitations

1. **Budget data attribution**: Pre-2014, ~30% of amendments shared functional-programmatic classifications across legislators, making attribution imperfect.

2. **Historical coverage**: Portal da Transparência data (used by Base dos Dados) may not cover the full 1999-2010 period. If the emendas table starts later, earliest terms may need supplementary data from SIGA Brasil.

3. **Legislator matching**: Linking legislators across TSE and emendas data relies on name matching (no common ID). The scripts normalize names for matching.

4. **Table schema**: Exact BigQuery column names may change. The download script includes a schema discovery step (`LIMIT 5` query) and clear error messages for debugging.

## References

Firpo, S., Ponczek, V., & Sanfelice, V. (2015). The relationship between federal budget amendments and local electoral power. *Journal of Development Economics*, 116, 186-198.
