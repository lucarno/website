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
  # Electoral data
  "electionsBR",
  # Budget data
  "orcamentoBR",
  # Population / geography
  "sidrar", "geobr",
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

# --- Paths -------------------------------------------------------------------

# Project root (this file should be sourced from research/)
proj_root <- here::here("research")
if (!dir.exists(proj_root)) {
  # Fallback: use the directory where this script lives

  proj_root <- dirname(sys.frame(1)$ofile)
}

data_raw   <- file.path(proj_root, "data", "raw")
data_clean <- file.path(proj_root, "data", "clean")
output_dir <- file.path(proj_root, "output")

# Create directories if they don't exist
for (d in c(data_raw, data_clean, output_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Constants ---------------------------------------------------------------

# Federal election years (Deputado Federal)
election_years_original <- c(1998, 2002, 2006, 2010)
election_years_extension <- c(2014, 2018, 2022)
election_years_all <- c(election_years_original, election_years_extension)

# Legislative terms: each term spans 4 years after election
# Budget amendments are allocated during the legislative term
# E.g., legislators elected in 1998 serve 1999-2002 and allocate amendments then
legislative_terms <- tibble(
  election_year = c(1998, 2002, 2006, 2010, 2014, 2018, 2022),
  term_start    = c(1999, 2003, 2007, 2011, 2015, 2019, 2023),
  term_end      = c(2002, 2006, 2010, 2014, 2018, 2022, 2026)
)

# Position code for Deputado Federal in TSE data
POSITION_DEPUTADO_FEDERAL <- 6

# --- Helper functions --------------------------------------------------------

#' Convert TSE municipality code to IBGE municipality code
#' TSE uses 5-digit codes; IBGE uses 7-digit codes
#' The mapping is: TSE code = first 2 digits (state) + 3 digits (municipality)
#' IBGE code = 2 digits (state) + 5 digits (municipality, last digit is check)
#' We need a crosswalk table for accurate conversion.
#'
#' This function loads the crosswalk from geobr or a cached local file.
load_municipality_crosswalk <- function() {
  crosswalk_path <- file.path(data_clean, "municipality_crosswalk.rds")

  if (file.exists(crosswalk_path)) {
    return(readRDS(crosswalk_path))
  }

  message("Building TSE-IBGE municipality crosswalk from geobr...")

  # geobr provides municipality data with IBGE codes
  munis <- geobr::read_municipality(year = 2010, showProgress = FALSE) |>
    st_drop_geometry() |>
    select(code_muni, name_muni, code_state, abbrev_state) |>
    mutate(
      # IBGE 7-digit code
      ibge_code = as.character(code_muni),
      # IBGE 6-digit code (drop check digit)
      ibge_6 = substr(ibge_code, 1, 6)
    )

  saveRDS(munis, crosswalk_path)
  return(munis)
}

#' Compute the Herfindahl-Hirschman Index for a vector of vote shares
compute_hhi <- function(vote_shares) {
  # vote_shares should sum to ~1 (proportions)
  sum(vote_shares^2, na.rm = TRUE)
}

#' Compute effective number of candidates from HHI
effective_n_candidates <- function(hhi) {
  ifelse(hhi > 0, 1 / hhi, NA_real_)
}

message("=== Firpo et al. (2015) Replication — Configuration loaded ===")
