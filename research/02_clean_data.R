# =============================================================================
# 02_clean_data.R
# Clean and standardize raw TSE, budget, and population data
# =============================================================================

source(here::here("research", "_common.R"))

# =============================================================================
# PART 1: Clean Electoral Data
# =============================================================================

message("\n=== Cleaning TSE electoral data ===\n")

# TSE column names vary across years. We standardize to a common schema:
#   election_year, state, mun_code_tse, candidate_id, candidate_name,
#   party, coalition, votes, elected

# Column name mappings for different TSE data vintages
vote_col_map <- list(
  # Pre-2014 format
  old = list(
    year        = c("ANO_ELEICAO", "ano_eleicao"),
    state       = c("SIGLA_UF", "sigla_uf", "SG_UF"),
    mun_code    = c("CODIGO_MUNICIPIO", "codigo_municipio", "CD_MUNICIPIO"),
    mun_name    = c("NOME_MUNICIPIO", "nome_municipio", "NM_MUNICIPIO"),
    cand_id     = c("NUMERO_CANDIDATO", "numero_candidato", "NR_CANDIDATO"),
    cand_name   = c("NOME_CANDIDATO", "nome_candidato", "NM_CANDIDATO"),
    cand_seq    = c("SEQUENCIAL_CANDIDATO", "sequencial_candidato",
                    "SQ_CANDIDATO"),
    party_abbr  = c("SIGLA_PARTIDO", "sigla_partido", "SG_PARTIDO"),
    votes       = c("TOTAL_VOTOS", "total_votos", "QT_VOTOS_NOMINAIS",
                    "qt_votos_nominais")
  )
)

#' Find the first matching column name from a list of candidates
find_col <- function(df, candidates) {
  match <- intersect(candidates, names(df))
  if (length(match) > 0) return(match[1])
  return(NA_character_)
}

# Process each election year
votes_all <- list()

for (yr in election_years_all) {
  infile <- file.path(data_raw, paste0("tse_votes_depfed_", yr, ".rds"))

  if (!file.exists(infile)) {
    message("  ", yr, ": raw data not found, skipping.")
    next
  }

  message("  Cleaning ", yr, " vote data...")
  df <- readRDS(infile) |> clean_names()

  # Identify columns dynamically
  cols <- sapply(vote_col_map$old, function(candidates) {
    find_col(df, tolower(candidates))
  })

  # Build standardized dataframe
  # Not all columns may exist; handle gracefully
  result <- df |>
    transmute(
      election_year = yr,
      state = if (!is.na(cols["state"])) .data[[cols["state"]]] else NA_character_,
      mun_code_tse = if (!is.na(cols["mun_code"])) {
        as.character(.data[[cols["mun_code"]]])
      } else NA_character_,
      mun_name = if (!is.na(cols["mun_name"])) .data[[cols["mun_name"]]] else NA_character_,
      candidate_seq = if (!is.na(cols["cand_seq"])) {
        as.character(.data[[cols["cand_seq"]]])
      } else NA_character_,
      candidate_number = if (!is.na(cols["cand_id"])) {
        as.character(.data[[cols["cand_id"]]])
      } else NA_character_,
      candidate_name = if (!is.na(cols["cand_name"])) .data[[cols["cand_name"]]] else NA_character_,
      party = if (!is.na(cols["party_abbr"])) .data[[cols["party_abbr"]]] else NA_character_,
      votes = if (!is.na(cols["votes"])) {
        as.numeric(.data[[cols["votes"]]])
      } else NA_real_
    )

  # Aggregate to municipality level (in case data is at zone level)
  result <- result |>
    group_by(election_year, state, mun_code_tse, mun_name,
             candidate_seq, candidate_number, candidate_name, party) |>
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")

  votes_all[[as.character(yr)]] <- result
  message("    -> ", nrow(result), " candidate-municipality observations")
}

# Combine all years
votes_clean <- bind_rows(votes_all)
message("\n  Total: ", nrow(votes_clean), " observations across all years")

# =============================================================================
# PART 2: Add elected status from candidate data
# =============================================================================

message("\n=== Merging elected status from candidate data ===\n")

for (yr in election_years_all) {
  cand_file <- file.path(data_raw, paste0("tse_candidates_depfed_", yr, ".rds"))

  if (!file.exists(cand_file)) {
    message("  ", yr, ": candidate data not found, skipping.")
    next
  }

  cand_df <- readRDS(cand_file) |> clean_names()

  # Find the elected status column
  elected_col <- find_col(cand_df, c(
    "desc_sit_tot_turno", "ds_sit_tot_turno",
    "cod_sit_tot_turno", "cd_sit_tot_turno",
    "situacao_candidato", "ds_situacao"
  ))

  seq_col <- find_col(cand_df, c(
    "sequencial_candidato", "sq_candidato"
  ))

  if (!is.na(elected_col) && !is.na(seq_col)) {
    # Create elected indicator
    elected_lookup <- cand_df |>
      transmute(
        candidate_seq = as.character(.data[[seq_col]]),
        elected_status = toupper(.data[[elected_col]])
      ) |>
      mutate(
        elected = str_detect(elected_status, "ELEITO|MEDIA|SUPLENTE") &
          !str_detect(elected_status, "NAO ELEITO|NÃO ELEITO")
      ) |>
      distinct(candidate_seq, .keep_all = TRUE)

    # Merge with votes
    votes_clean <- votes_clean |>
      left_join(
        elected_lookup |> select(candidate_seq, elected),
        by = "candidate_seq",
        suffix = c("", ".new")
      ) |>
      mutate(
        elected = coalesce(elected.new, elected)
      ) |>
      select(-any_of("elected.new"))

    message("  ", yr, ": merged elected status")
  }
}

# For candidates without elected status, mark as FALSE
votes_clean <- votes_clean |>
  mutate(elected = replace_na(elected, FALSE))

# =============================================================================
# PART 3: Compute vote shares at municipality level
# =============================================================================

message("\n=== Computing vote shares ===\n")

# Total valid votes for Deputado Federal in each municipality-year
mun_totals <- votes_clean |>
  group_by(election_year, state, mun_code_tse) |>
  summarise(total_votes_mun = sum(votes, na.rm = TRUE), .groups = "drop")

votes_clean <- votes_clean |>
  left_join(mun_totals, by = c("election_year", "state", "mun_code_tse")) |>
  mutate(
    vote_share = ifelse(total_votes_mun > 0, votes / total_votes_mun, 0)
  )

message("  Vote shares computed.")
message("  Mean vote share: ", round(mean(votes_clean$vote_share, na.rm = TRUE), 4))
message("  Max vote share: ", round(max(votes_clean$vote_share, na.rm = TRUE), 4))

# =============================================================================
# PART 4: Clean Budget Amendment Data
# =============================================================================

message("\n=== Cleaning budget amendment data ===\n")

budget_file_auto <- file.path(data_raw, "budget_amendments_raw.rds")
budget_file_manual <- file.path(data_raw, "budget_amendments_manual.csv")

if (file.exists(budget_file_auto)) {
  message("  Loading automatically downloaded budget data...")
  budget_raw <- readRDS(budget_file_auto) |> clean_names()

  # Standardize column names (will depend on actual data structure)
  # This is a template; adjust column names based on actual data
  budget_clean <- budget_raw |>
    transmute(
      year = as.numeric(year),
      legislator_id = as.character(legislator_id),
      legislator_name = legislator_name,
      mun_code_ibge = as.character(mun_code_ibge),
      amendment_value = as.numeric(amendment_value)
    )
} else if (file.exists(budget_file_manual)) {
  message("  Loading manually downloaded budget data...")
  budget_raw <- read_csv(budget_file_manual, show_col_types = FALSE) |>
    clean_names()

  # User will need to verify column names match
  message("  Column names found: ", paste(names(budget_raw), collapse = ", "))
  message("  Please verify the column mapping in 02_clean_data.R if needed.")

  # Template mapping — user should adjust column names as needed
  budget_clean <- budget_raw |>
    rename_with(tolower) |>
    transmute(
      year = as.numeric(
        .data[[find_col(budget_raw, c("ano", "year", "ano_execucao"))]]
      ),
      legislator_name = .data[[find_col(budget_raw, c(
        "autor", "nome_autor", "legislator", "parlamentar", "nome_parlamentar"
      ))]],
      mun_code_ibge = as.character(
        .data[[find_col(budget_raw, c(
          "cod_municipio", "codigo_ibge", "ibge", "municipio_codigo",
          "cod_ibge", "mun_code"
        ))]]
      ),
      amendment_value = as.numeric(
        .data[[find_col(budget_raw, c(
          "valor", "value", "valor_pago", "valor_empenhado",
          "vl_pago", "vl_empenhado"
        ))]]
      )
    )
} else {
  message("  WARNING: No budget amendment data found!")
  message("  Run 01_download_data.R and follow manual download instructions.")
  budget_clean <- tibble(
    year = integer(),
    legislator_name = character(),
    mun_code_ibge = character(),
    amendment_value = numeric()
  )
}

if (nrow(budget_clean) > 0) {
  # Aggregate by legislator-municipality-year
  budget_clean <- budget_clean |>
    group_by(year, legislator_name, mun_code_ibge) |>
    summarise(
      total_amendment_value = sum(amendment_value, na.rm = TRUE),
      n_amendments = n(),
      .groups = "drop"
    )
  message("  Budget data: ", nrow(budget_clean), " legislator-municipality-year obs")
}

# =============================================================================
# PART 5: Clean Population Data
# =============================================================================

message("\n=== Cleaning population data ===\n")

pop_file <- file.path(data_raw, "ibge_population.rds")

if (file.exists(pop_file)) {
  pop_raw <- readRDS(pop_file)

  # sidrar output typically has columns: Municipio (Codigo), Ano, Valor
  pop_clean <- pop_raw |>
    clean_names() |>
    transmute(
      mun_code_ibge = as.character(
        .data[[find_col(clean_names(pop_raw), c(
          "municipio_codigo", "cod_municipio", "codigo"
        ))]]
      ),
      year = as.numeric(
        .data[[find_col(clean_names(pop_raw), c(
          "ano", "year", "periodo"
        ))]]
      ),
      population = as.numeric(
        .data[[find_col(clean_names(pop_raw), c(
          "valor", "value", "populacao"
        ))]]
      )
    ) |>
    filter(!is.na(population), population > 0)

  message("  Population data: ", nrow(pop_clean), " municipality-year obs")
} else {
  message("  WARNING: No population data found! Run 01_download_data.R first.")
  pop_clean <- tibble(
    mun_code_ibge = character(),
    year = numeric(),
    population = numeric()
  )
}

# =============================================================================
# PART 6: Save cleaned data
# =============================================================================

message("\n=== Saving cleaned data ===\n")

saveRDS(votes_clean, file.path(data_clean, "votes_clean.rds"))
message("  Saved votes_clean.rds: ", nrow(votes_clean), " rows")

saveRDS(budget_clean, file.path(data_clean, "budget_clean.rds"))
message("  Saved budget_clean.rds: ", nrow(budget_clean), " rows")

saveRDS(pop_clean, file.path(data_clean, "population_clean.rds"))
message("  Saved population_clean.rds: ", nrow(pop_clean), " rows")

message("\n=== Data cleaning complete ===\n")
