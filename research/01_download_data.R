# =============================================================================
# 01_download_data.R
# Download raw data: TSE electoral results, budget amendments, population
# =============================================================================

source(here::here("research", "_common.R"))

# =============================================================================
# PART 1: Electoral Data (TSE)
# =============================================================================
# We need Deputado Federal vote counts at the municipality level
# for each election year. The electionsBR package downloads directly
# from TSE's open data repository.

message("\n=== Downloading TSE electoral data ===\n")

for (yr in election_years_all) {
  outfile <- file.path(data_raw, paste0("tse_votes_depfed_", yr, ".rds"))

  if (file.exists(outfile)) {
    message("  ", yr, ": already downloaded, skipping.")
    next
  }

  message("  Downloading ", yr, " Deputado Federal vote data...")

  tryCatch({
    # vote_mun_zone gives candidate-level vote counts by municipality and zone
    df <- electionsBR::elections_tse(
      year = yr,
      type = "vote_mun_zone"
    )

    # Filter to Deputado Federal only
    # The position code column varies by year; common names:
    # CODIGO_CARGO or CD_CARGO
    cargo_col <- intersect(
      c("CODIGO_CARGO", "CD_CARGO", "codigo_cargo", "cd_cargo"),
      names(df)
    )

    if (length(cargo_col) > 0) {
      df <- df |> filter(.data[[cargo_col[1]]] == POSITION_DEPUTADO_FEDERAL)
    } else {
      # Try filtering by description
      desc_col <- intersect(
        c("DESCRICAO_CARGO", "DS_CARGO", "descricao_cargo", "ds_cargo"),
        names(df)
      )
      if (length(desc_col) > 0) {
        df <- df |>
          filter(str_detect(
            toupper(.data[[desc_col[1]]]),
            "DEPUTADO FEDERAL"
          ))
      }
    }

    saveRDS(df, outfile)
    message("    -> Saved: ", nrow(df), " rows")
  }, error = function(e) {
    message("    -> ERROR: ", conditionMessage(e))
  })
}

# Also download candidate-level data (for elected status, party, etc.)
message("\n=== Downloading TSE candidate data ===\n")

for (yr in election_years_all) {
  outfile <- file.path(data_raw, paste0("tse_candidates_depfed_", yr, ".rds"))

  if (file.exists(outfile)) {
    message("  ", yr, ": already downloaded, skipping.")
    next
  }

  message("  Downloading ", yr, " candidate data...")

  tryCatch({
    df <- electionsBR::elections_tse(
      year = yr,
      type = "candidate"
    )

    # Filter to Deputado Federal
    cargo_col <- intersect(
      c("CODIGO_CARGO", "CD_CARGO", "codigo_cargo", "cd_cargo"),
      names(df)
    )
    if (length(cargo_col) > 0) {
      df <- df |> filter(.data[[cargo_col[1]]] == POSITION_DEPUTADO_FEDERAL)
    } else {
      desc_col <- intersect(
        c("DESCRICAO_CARGO", "DS_CARGO", "descricao_cargo", "ds_cargo"),
        names(df)
      )
      if (length(desc_col) > 0) {
        df <- df |>
          filter(str_detect(
            toupper(.data[[desc_col[1]]]),
            "DEPUTADO FEDERAL"
          ))
      }
    }

    saveRDS(df, outfile)
    message("    -> Saved: ", nrow(df), " rows")
  }, error = function(e) {
    message("    -> ERROR: ", conditionMessage(e))
  })
}

# =============================================================================
# PART 2: Budget Amendment Data (Emendas Orçamentárias Individuais)
# =============================================================================
# This is the most challenging data to obtain programmatically.
# We try multiple strategies in order of preference.

message("\n=== Downloading budget amendment data ===\n")

# --- Strategy A: orcamentoBR package (SIOP SPARQL endpoint) ------------------
# The orcamentoBR package provides access to federal budget data.
# It may not have the legislator-municipality level detail we need,
# but it's worth trying first.

budget_file <- file.path(data_raw, "budget_amendments_raw.rds")

if (file.exists(budget_file)) {
  message("  Budget data already downloaded.")
} else {
  message("  Attempting download via orcamentoBR...")

  tryCatch({
    # Try to get emendas individuais data
    # The orcamentoBR package provides budget data via SPARQL
    # Check available functions
    if ("get_emendas" %in% ls("package:orcamentoBR")) {
      budget_raw <- orcamentoBR::get_emendas()
      saveRDS(budget_raw, budget_file)
      message("    -> Saved budget amendment data")
    } else {
      message("    -> orcamentoBR does not have get_emendas()")
      message("    -> See README.md for manual download instructions")
    }
  }, error = function(e) {
    message("    -> orcamentoBR failed: ", conditionMessage(e))
    message("    -> See README.md for manual download instructions")
  })
}

# --- Strategy B: Manual download instructions --------------------------------
# If programmatic access fails, the user needs to download data manually.
# The README provides detailed instructions for this.

if (!file.exists(budget_file)) {
  message("\n")
  message("  ============================================================")
  message("  MANUAL DOWNLOAD REQUIRED FOR BUDGET AMENDMENT DATA")
  message("  ============================================================")
  message("  ")
  message("  The budget amendment data could not be downloaded automatically.")
  message("  Please follow these steps:")
  message("  ")
  message("  Option 1: SIGA Brasil (most comprehensive)")
  message("    1. Go to https://www12.senado.leg.br/orcamento/sigabrasil")
  message("    2. Navigate to 'Emendas Parlamentares'")
  message("    3. Filter by: Tipo = Individual, Autor = Deputado Federal")
  message("    4. Select years: 1999-2010 (original period)")
  message("    5. Download as CSV")
  message("    6. Place files in: ", data_raw)
  message("  ")
  message("  Option 2: Portal da Transparencia")
  message("    1. Go to https://portaldatransparencia.gov.br/emendas")
  message("    2. Filter and download emendas individuais by year")
  message("    3. Place files in: ", data_raw)
  message("  ")
  message("  Option 3: Tesouro Transparente")
  message("    1. Go to https://www.tesourotransparente.gov.br")
  message("    2. Navigate to 'Emendas Parlamentares'")
  message("    3. Download the data panel")
  message("    4. Place files in: ", data_raw)
  message("  ")
  message("  After downloading, the file should contain at minimum:")
  message("    - Author/legislator identifier")
  message("    - Municipality code (IBGE or name)")
  message("    - Amendment value (R$)")
  message("    - Year of execution")
  message("  ")
  message("  Name the file: budget_amendments_manual.csv")
  message("  ============================================================")
}

# =============================================================================
# PART 3: Population Data (IBGE)
# =============================================================================
# Municipal population estimates for computing per capita measures.

message("\n=== Downloading IBGE population data ===\n")

pop_file <- file.path(data_raw, "ibge_population.rds")

if (file.exists(pop_file)) {
  message("  Population data already downloaded.")
} else {
  message("  Downloading municipal population estimates via sidrar...")

  tryCatch({
    # IBGE Table 6579: Municipal population estimates
    # This table has annual population estimates by municipality
    pop_data <- sidrar::get_sidra(
      x = 6579,            # Population estimates table
      period = "1998-2022", # Full period
      geo = "City"          # Municipality level
    )

    saveRDS(pop_data, pop_file)
    message("    -> Saved: ", nrow(pop_data), " rows")
  }, error = function(e) {
    message("    -> sidrar failed: ", conditionMessage(e))
    message("    -> Trying alternative approach...")

    # Alternative: Census population counts for key years
    tryCatch({
      # Try IBGE population estimates table 793
      pop_data <- sidrar::get_sidra(
        x = 793,
        period = "all",
        geo = "City"
      )
      saveRDS(pop_data, pop_file)
      message("    -> Saved alternative population data: ", nrow(pop_data), " rows")
    }, error = function(e2) {
      message("    -> Alternative also failed: ", conditionMessage(e2))
      message("    -> You may need to download population data manually from IBGE")
      message("    -> https://www.ibge.gov.br/estatisticas/sociais/populacao.html")
    })
  })
}

# =============================================================================
# PART 4: Municipality crosswalk (TSE <-> IBGE codes)
# =============================================================================

message("\n=== Building municipality crosswalk ===\n")

crosswalk <- load_municipality_crosswalk()
message("  Crosswalk has ", nrow(crosswalk), " municipalities")

message("\n=== Data download complete ===\n")
message("Check ", data_raw, " for downloaded files.")
message("If budget amendment data is missing, follow the manual instructions above.")
