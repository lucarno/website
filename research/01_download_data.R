# =============================================================================
# 01_download_data.R
# Download raw data from Base dos Dados (BigQuery):
#   - TSE electoral results (Deputado Federal, by municipality)
#   - Budget amendments (emendas parlamentares individuais)
#   - IBGE municipal population
#
# All datasets use standardized id_municipio — no crosswalk needed.
# Requires: Google Cloud project (free). See _common.R for setup.
# =============================================================================

source(here::here("research", "_common.R"))

# =============================================================================
# STEP 0: Discover table schemas (run interactively to verify column names)
# =============================================================================
# Uncomment to inspect table schemas before running queries:
#
# bdplyr(BQ_TSE_RESULTADOS) |> glimpse()
# bdplyr(BQ_TSE_CANDIDATOS) |> glimpse()
# bdplyr(BQ_EMENDAS) |> glimpse()
# bdplyr(BQ_POPULACAO) |> glimpse()

# =============================================================================
# PART 1: Electoral Data (TSE) — Votes by candidate × municipality
# =============================================================================
# Table: br_tse_eleicoes.resultados_candidato_municipio
# Contains candidate-level vote counts at the municipality level.
# We filter to cargo = 'deputado federal' and our election years.

message("\n=== Downloading TSE electoral results (Deputado Federal) ===\n")

years_str <- paste(election_years_all, collapse = ", ")

votes_query <- glue::glue("
  SELECT
    ano,
    sigla_uf,
    id_municipio,
    id_candidato_bd,
    numero_candidato,
    nome_candidato,
    sigla_partido,
    votos,
    resultado
  FROM `basedosdados.{BQ_TSE_RESULTADOS}`
  WHERE cargo = 'deputado federal'
    AND ano IN ({years_str})
")

votes_raw <- bd_query_cached(votes_query, "tse_votes_depfed")

message("  Total vote observations: ", nrow(votes_raw))
message("  Years: ", paste(sort(unique(votes_raw$ano)), collapse = ", "))

# =============================================================================
# PART 2: Candidate data (for elected status if not in results table)
# =============================================================================
# Table: br_tse_eleicoes.candidatos
# Contains candidate metadata including elected status.

message("\n=== Downloading TSE candidate data ===\n")

candidates_query <- glue::glue("
  SELECT
    ano,
    id_candidato_bd,
    sequencial_candidato,
    numero_candidato,
    nome,
    nome_urna,
    sigla_partido,
    situacao,
    sigla_uf
  FROM `basedosdados.{BQ_TSE_CANDIDATOS}`
  WHERE cargo = 'deputado federal'
    AND ano IN ({years_str})
")

candidates_raw <- bd_query_cached(candidates_query, "tse_candidates_depfed")

message("  Total candidates: ", nrow(candidates_raw))

# =============================================================================
# PART 3: Budget Amendment Data (Emendas Parlamentares)
# =============================================================================
# Table: br_cgu_emendas_parlamentares.microdados
# Contains individual parliamentary amendments with author, municipality,
# and financial values.
#
# NOTE: Column names below are best guesses based on Portal da Transparência
# structure and Base dos Dados naming conventions. If the query fails,
# run bdplyr(BQ_EMENDAS) |> glimpse() to discover actual column names.

message("\n=== Downloading budget amendment data (emendas parlamentares) ===\n")

# First, try to discover the actual columns
message("  Checking emendas table schema...")

tryCatch({
  # Query a small sample to discover columns
  schema_query <- glue::glue("
    SELECT *
    FROM `basedosdados.{BQ_EMENDAS}`
    LIMIT 5
  ")
  schema_sample <- basedosdados::read_sql(schema_query)
  message("  Available columns: ", paste(names(schema_sample), collapse = ", "))

  # Save column names for reference
  saveRDS(names(schema_sample), file.path(data_raw, "emendas_column_names.rds"))

  # Now download the full dataset for our period
  # Adjust column names based on what we discovered
  emendas_query <- glue::glue("
    SELECT *
    FROM `basedosdados.{BQ_EMENDAS}`
    WHERE ano >= 1999 AND ano <= 2026
  ")

  emendas_raw <- bd_query_cached(emendas_query, "emendas_parlamentares")

  message("  Total emendas observations: ", nrow(emendas_raw))
  message("  Year range: ",
          min(emendas_raw$ano, na.rm = TRUE), " - ",
          max(emendas_raw$ano, na.rm = TRUE))

}, error = function(e) {
  message("  ERROR querying emendas: ", conditionMessage(e))
  message("")
  message("  The table name or schema may differ. To debug:")
  message("    1. Run: bdplyr('", BQ_EMENDAS, "') |> glimpse()")
  message("    2. Or search tables: basedosdados::read_sql(\"")
  message("         SELECT table_name FROM ")
  message("         `basedosdados.br_cgu_emendas_parlamentares.INFORMATION_SCHEMA.TABLES`\")")
  message("    3. Adjust BQ_EMENDAS in _common.R and re-run this script.")
  message("")
  message("  Fallback: download manually from Portal da Transparência:")
  message("    https://portaldatransparencia.gov.br/emendas")
  message("  Place CSV in: ", data_raw, "/budget_amendments_manual.csv")
})

# =============================================================================
# PART 4: Population Data (IBGE)
# =============================================================================
# Table: br_ibge_populacao.municipio
# Municipal population estimates by year.

message("\n=== Downloading IBGE population data ===\n")

tryCatch({
  pop_query <- glue::glue("
    SELECT
      ano,
      id_municipio,
      populacao
    FROM `basedosdados.{BQ_POPULACAO}`
    WHERE ano >= 1998 AND ano <= 2026
  ")

  pop_raw <- bd_query_cached(pop_query, "ibge_population")

  message("  Population observations: ", nrow(pop_raw))
  message("  Year range: ",
          min(pop_raw$ano, na.rm = TRUE), " - ",
          max(pop_raw$ano, na.rm = TRUE))

}, error = function(e) {
  message("  ERROR querying population: ", conditionMessage(e))
  message("  Table name may differ. Run bdplyr('", BQ_POPULACAO, "') |> glimpse()")
  message("  Fallback: use sidrar package:")
  message("    install.packages('sidrar')")
  message("    pop <- sidrar::get_sidra(x = 6579, period = '1998-2022', geo = 'City')")
})

# =============================================================================
# Summary
# =============================================================================

message("\n=== Data download summary ===\n")

cached_files <- list.files(data_raw, pattern = "\\.rds$")
for (f in cached_files) {
  size_mb <- round(file.size(file.path(data_raw, f)) / 1e6, 1)
  message(sprintf("  %-40s %6.1f MB", f, size_mb))
}

message("\nAll data cached in: ", data_raw)
message("To refresh, delete the .rds files and re-run this script.")
