# =============================================================================
# 02_clean_data.R
# Clean and standardize data downloaded from Base dos Dados.
# All datasets already share standardized id_municipio — no crosswalk needed.
# =============================================================================

source(here::here("research", "_common.R"))

# =============================================================================
# PART 1: Clean Electoral Data (votes by candidate × municipality)
# =============================================================================

message("\n=== Cleaning TSE electoral data ===\n")

votes_raw <- readRDS(file.path(data_raw, "tse_votes_depfed.rds"))
candidates_raw <- readRDS(file.path(data_raw, "tse_candidates_depfed.rds"))

# Standardize column names (Base dos Dados uses consistent naming)
votes_clean <- votes_raw |>
  transmute(
    election_year = as.integer(ano),
    state = sigla_uf,
    id_municipio = as.character(id_municipio),
    id_candidato = as.character(id_candidato_bd),
    candidate_number = as.character(numero_candidato),
    candidate_name = nome_candidato,
    party = sigla_partido,
    votes = as.numeric(votos),
    result = resultado
  ) |>
  # Mark elected candidates
  mutate(
    elected = str_detect(toupper(result), "ELEITO|MEDIA") &
      !str_detect(toupper(result), "NAO ELEITO|NÃO ELEITO")
  )

# If result column doesn't have elected status, merge from candidates table
if (all(is.na(votes_clean$result))) {
  message("  Result column empty in votes table; merging from candidates table...")

  elected_lookup <- candidates_raw |>
    transmute(
      election_year = as.integer(ano),
      id_candidato = as.character(id_candidato_bd),
      elected = str_detect(toupper(situacao), "ELEITO|MEDIA") &
        !str_detect(toupper(situacao), "NAO ELEITO|NÃO ELEITO")
    ) |>
    distinct(election_year, id_candidato, .keep_all = TRUE)

  votes_clean <- votes_clean |>
    select(-elected) |>
    left_join(elected_lookup, by = c("election_year", "id_candidato")) |>
    mutate(elected = replace_na(elected, FALSE))
}

# Compute vote shares at municipality level
mun_totals <- votes_clean |>
  group_by(election_year, id_municipio) |>
  summarise(total_votes_mun = sum(votes, na.rm = TRUE), .groups = "drop")

votes_clean <- votes_clean |>
  left_join(mun_totals, by = c("election_year", "id_municipio")) |>
  mutate(
    vote_share = ifelse(total_votes_mun > 0, votes / total_votes_mun, 0)
  )

message("  Observations: ", nrow(votes_clean))
message("  Years: ", paste(sort(unique(votes_clean$election_year)), collapse = ", "))
message("  Elected candidates: ", sum(votes_clean$elected, na.rm = TRUE))
message("  Mean vote share: ", round(mean(votes_clean$vote_share, na.rm = TRUE), 4))

# =============================================================================
# PART 2: Clean Budget Amendment Data
# =============================================================================

message("\n=== Cleaning budget amendment data ===\n")

emendas_file <- file.path(data_raw, "emendas_parlamentares.rds")
manual_file <- file.path(data_raw, "budget_amendments_manual.csv")

if (file.exists(emendas_file)) {
  emendas_raw <- readRDS(emendas_file)

  message("  Columns available: ", paste(names(emendas_raw), collapse = ", "))

  # Identify key columns dynamically
  # Base dos Dados standardizes: ano, id_municipio
  # Author/legislator and value columns vary by dataset version
  find_col <- function(df, candidates) {
    match <- intersect(tolower(candidates), tolower(names(df)))
    if (length(match) > 0) return(match[1])
    return(NA_character_)
  }

  author_col <- find_col(emendas_raw, c(
    "nome_autor", "autor", "nome_parlamentar", "parlamentar"
  ))
  value_col <- find_col(emendas_raw, c(
    "valor_pago", "valor_empenhado", "valor_liquidado", "valor"
  ))
  type_col <- find_col(emendas_raw, c(
    "tipo_emenda", "tipo"
  ))
  mun_col <- find_col(emendas_raw, c(
    "id_municipio", "codigo_ibge", "cod_municipio"
  ))

  message("  Detected columns -> author: ", author_col,
          ", value: ", value_col,
          ", type: ", type_col,
          ", municipality: ", mun_col)

  # Build standardized budget dataframe
  budget_clean <- emendas_raw |>
    transmute(
      year = as.integer(ano),
      legislator_name = if (!is.na(author_col)) .data[[author_col]] else NA_character_,
      id_municipio = as.character(
        if (!is.na(mun_col)) .data[[mun_col]] else NA_character_
      ),
      amendment_value = as.numeric(
        if (!is.na(value_col)) .data[[value_col]] else NA_real_
      ),
      type = if (!is.na(type_col)) .data[[type_col]] else NA_character_
    )

  # Filter to individual amendments by federal deputies if type column exists
  if (!all(is.na(budget_clean$type))) {
    n_before <- nrow(budget_clean)
    budget_clean <- budget_clean |>
      filter(str_detect(toupper(type), "INDIVIDUAL"))
    message("  Filtered to individual amendments: ", nrow(budget_clean),
            " (from ", n_before, ")")
  }

  # Remove rows without municipality or value
  budget_clean <- budget_clean |>
    filter(!is.na(id_municipio), !is.na(amendment_value), amendment_value > 0)

  # Aggregate by legislator-municipality-year
  budget_clean <- budget_clean |>
    group_by(year, legislator_name, id_municipio) |>
    summarise(
      total_amendment_value = sum(amendment_value, na.rm = TRUE),
      n_amendments = n(),
      .groups = "drop"
    )

  message("  Budget data: ", nrow(budget_clean), " legislator-municipality-year obs")
  message("  Year range: ", min(budget_clean$year), " - ", max(budget_clean$year))

} else if (file.exists(manual_file)) {
  message("  Loading manually downloaded budget data from CSV...")
  budget_clean <- read_csv(manual_file, show_col_types = FALSE) |>
    clean_names()
  message("  Columns: ", paste(names(budget_clean), collapse = ", "))
  message("  Please verify column mapping in 02_clean_data.R")
} else {
  message("  WARNING: No budget amendment data found!")
  message("  Run 01_download_data.R first, or place manual CSV in data/raw/")
  budget_clean <- tibble(
    year = integer(),
    legislator_name = character(),
    id_municipio = character(),
    total_amendment_value = numeric(),
    n_amendments = integer()
  )
}

# =============================================================================
# PART 3: Clean Population Data
# =============================================================================

message("\n=== Cleaning population data ===\n")

pop_file <- file.path(data_raw, "ibge_population.rds")

if (file.exists(pop_file)) {
  pop_raw <- readRDS(pop_file)

  pop_clean <- pop_raw |>
    transmute(
      year = as.integer(ano),
      id_municipio = as.character(id_municipio),
      population = as.numeric(populacao)
    ) |>
    filter(!is.na(population), population > 0)

  message("  Population data: ", nrow(pop_clean), " municipality-year obs")
  message("  Year range: ", min(pop_clean$year), " - ", max(pop_clean$year))
} else {
  message("  WARNING: No population data found! Run 01_download_data.R first.")
  pop_clean <- tibble(
    year = integer(),
    id_municipio = character(),
    population = numeric()
  )
}

# =============================================================================
# PART 4: Save cleaned data
# =============================================================================

message("\n=== Saving cleaned data ===\n")

saveRDS(votes_clean, file.path(data_clean, "votes_clean.rds"))
message("  Saved votes_clean.rds: ", nrow(votes_clean), " rows")

saveRDS(budget_clean, file.path(data_clean, "budget_clean.rds"))
message("  Saved budget_clean.rds: ", nrow(budget_clean), " rows")

saveRDS(pop_clean, file.path(data_clean, "population_clean.rds"))
message("  Saved population_clean.rds: ", nrow(pop_clean), " rows")

message("\n=== Data cleaning complete ===\n")
