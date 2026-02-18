# =============================================================================
# Replication: Firpo, Ponczek & Sanfelice (2015)
# "The relationship between federal budget amendments and local electoral power"
# Journal of Development Economics 116: 186-198
#
# _common.R — Shared configuration, packages, paths, and helper functions
# =============================================================================

# --- Packages ----------------------------------------------------------------

required_packages <- c(
  # Data manipulation
  "tidyverse", "data.table", "janitor",
  # Base dos Dados (primary data source — queries BigQuery)
  "basedosdados",
  # Econometrics
  "fixest", "modelsummary",
  # Tables and output
  "kableExtra", "ggplot2"
)

# Install missing packages
missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# Load all
invisible(lapply(required_packages, library, character.only = TRUE))

# --- Base dos Dados setup ----------------------------------------------------
# You need a Google Cloud project (free) for BigQuery access.
# Free tier: 1 TB of queries per month.
# Set your billing project ID below or via environment variable.

bd_project_id <- Sys.getenv("BD_PROJECT_ID", unset = "")
if (bd_project_id == "") {
  message("NOTE: Set your Google Cloud billing project ID for Base dos Dados.")
  message("  Option 1: Sys.setenv(BD_PROJECT_ID = 'your-project-id')")
  message("  Option 2: basedosdados::set_billing_id('your-project-id')")
  message("  See: https://basedosdados.org/docs/access_data_bq")
} else {
  basedosdados::set_billing_id(bd_project_id)
}

# --- Paths -------------------------------------------------------------------

proj_root <- here::here("research")
if (!dir.exists(proj_root)) {
  proj_root <- dirname(sys.frame(1)$ofile)
}

data_raw   <- file.path(proj_root, "data", "raw")
data_clean <- file.path(proj_root, "data", "clean")
output_dir <- file.path(proj_root, "output")

for (d in c(data_raw, data_clean, output_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Constants ---------------------------------------------------------------

# Federal election years (Deputado Federal)
election_years_original <- c(1998, 2002, 2006, 2010)
election_years_extension <- c(2014, 2018, 2022)
election_years_all <- c(election_years_original, election_years_extension)

# Legislative terms: legislators elected in year t serve t+1 to t+4
legislative_terms <- tibble(
  election_year = c(1998, 2002, 2006, 2010, 2014, 2018, 2022),
  term_start    = c(1999, 2003, 2007, 2011, 2015, 2019, 2023),
  term_end      = c(2002, 2006, 2010, 2014, 2018, 2022, 2026)
)

# --- BigQuery table references -----------------------------------------------
# These are the expected table names on Base dos Dados.
# Verify via bdplyr("table_name") |> glimpse() if queries fail.

BQ_TSE_CANDIDATOS <- "br_tse_eleicoes.candidatos"
BQ_TSE_RESULTADOS <- "br_tse_eleicoes.resultados_candidato_municipio"
BQ_EMENDAS        <- "br_cgu_emendas_parlamentares.microdados"
BQ_POPULACAO      <- "br_ibge_populacao.municipio"
BQ_DIRETORIOS     <- "br_bd_diretorios_brasil.municipio"

# --- Helper functions --------------------------------------------------------

#' Compute the Herfindahl-Hirschman Index for a vector of vote shares
compute_hhi <- function(vote_shares) {
  sum(vote_shares^2, na.rm = TRUE)
}

#' Compute effective number of candidates from HHI
effective_n_candidates <- function(hhi) {
  ifelse(hhi > 0, 1 / hhi, NA_real_)
}

#' Helper to query Base dos Dados with caching
#' Saves query result to data_raw/ so we don't re-query BigQuery each run.
bd_query_cached <- function(query_sql, cache_name) {
  cache_path <- file.path(data_raw, paste0(cache_name, ".rds"))

  if (file.exists(cache_path)) {
    message("  Loading cached: ", cache_name)
    return(readRDS(cache_path))
  }

  message("  Querying BigQuery: ", cache_name, "...")
  result <- basedosdados::read_sql(query_sql)
  saveRDS(result, cache_path)
  message("    -> Saved: ", nrow(result), " rows")
  return(result)
}

message("=== Firpo et al. (2015) Replication — Configuration loaded ===")
