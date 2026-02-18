# Replication: Firpo, Ponczek & Sanfelice (2015)

Replication of "The relationship between federal budget amendments and local electoral power" (*Journal of Development Economics*, 116: 186-198).

## Paper Summary

The paper studies two questions about Brazilian federal legislators and budget amendments (*emendas orçamentárias individuais*):

1. **Supply side**: Do legislators direct amendments to municipalities that supported them electorally?
2. **Demand side**: Do voters reward legislators who channel amendments to their municipality?

Key methodological contribution: defining "associated candidates" using the Herfindahl-Hirschman Index (HHI) to compute the effective number of candidates per municipality, since Brazil has no formal districts in its open-list proportional representation system.

## How to Run

Run scripts in order:

```r
source("research/01_download_data.R")    # Download raw data
source("research/02_clean_data.R")       # Clean and standardize
source("research/03_construct_variables.R") # Build HHI, associated candidates, panel
source("research/04_analysis.R")         # Main regressions (original period)
source("research/05_extension.R")        # Extend to 2014-2022
```

## Data Sources

### Electoral Data (TSE)
- **Source**: Tribunal Superior Eleitoral, Dados Abertos
- **Access**: Downloaded automatically via `electionsBR` R package
- **Content**: Deputado Federal vote counts by municipality, candidate info, elected status
- **Years**: 1998, 2002, 2006, 2010 (original); 2014, 2018, 2022 (extension)
- **URL**: https://dadosabertos.tse.jus.br/

### Budget Amendment Data (Emendas Orçamentárias)
- **Source**: SIAFI / SIGA Brasil / Portal da Transparência
- **Access**: Requires manual download (see instructions below)
- **Content**: Individual budget amendments by legislator and beneficiary municipality
- **Years**: 1999-2022

**Manual download instructions:**

1. Go to [SIGA Brasil](https://www12.senado.leg.br/orcamento/sigabrasil)
2. Navigate to "Emendas Parlamentares"
3. Filter: Tipo = Individual, Autor = Deputado Federal
4. Select years needed (1999-2010 for original, 2015-2022 for extension)
5. Download as CSV
6. Place in `research/data/raw/` as `budget_amendments_manual.csv`

Required columns (any naming convention): legislator name/ID, municipality IBGE code, amendment value, year.

Alternative sources:
- [Portal da Transparência](https://portaldatransparencia.gov.br/emendas)
- [Tesouro Transparente](https://www.tesourotransparente.gov.br)
- R package `orcamentoBR` (programmatic access to SIOP)

### Population Data (IBGE)
- **Source**: IBGE municipal population estimates
- **Access**: Downloaded automatically via `sidrar` R package
- **URL**: https://www.ibge.gov.br/

## R Dependencies

```r
install.packages(c(
  "tidyverse", "data.table", "janitor", "here",
  "electionsBR", "sidrar", "geobr",
  "fixest", "modelsummary", "kableExtra"
))

# Optional (for budget data):
install.packages("orcamentoBR")
```

## Project Structure

```
research/
├── _common.R                  # Shared configuration and helpers
├── 01_download_data.R         # Data acquisition
├── 02_clean_data.R            # Data cleaning and standardization
├── 03_construct_variables.R   # Variable construction and panel assembly
├── 04_analysis.R              # Main regressions (original period)
├── 05_extension.R             # Extension to 2014-2022
├── README.md                  # This file
├── .gitignore                 # Excludes data/ and output/
├── data/
│   ├── raw/                   # Raw downloaded files
│   └── clean/                 # Processed datasets
└── output/                    # Regression tables and figures
```

## Known Limitations

1. **Budget data attribution**: Pre-2014, ~30% of amendments shared functional-programmatic classifications across legislators, making attribution imperfect (documented in SIGA Brasil methodology).

2. **Legislator matching**: Linking legislators across TSE electoral data and budget amendment data relies on name matching since there is no common ID. The `02_clean_data.R` script normalizes names for matching.

3. **Municipality code harmonization**: TSE and IBGE use different municipality coding systems. The scripts handle this via a crosswalk from the `geobr` package.

## References

Firpo, S., Ponczek, V., & Sanfelice, V. (2015). The relationship between federal budget amendments and local electoral power. *Journal of Development Economics*, 116, 186-198.
