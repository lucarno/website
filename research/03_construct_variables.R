# =============================================================================
# 03_construct_variables.R
# Construct key variables: HHI, effective candidates, associated candidates,
# budget amendments per capita, and assemble the analysis panel.
# =============================================================================

source(here::here("research", "_common.R"))

# Load cleaned data
votes <- readRDS(file.path(data_clean, "votes_clean.rds"))
budget <- readRDS(file.path(data_clean, "budget_clean.rds"))
pop <- readRDS(file.path(data_clean, "population_clean.rds"))

# =============================================================================
# STEP 1: Compute HHI and effective number of candidates per municipality
# =============================================================================
# Following Firpo et al. (2015), the number of effective candidates in a
# municipality is derived from the Herfindahl-Hirschman Index of vote shares.
# HHI_m = sum(s_im^2), where s_im is the vote share of candidate i in mun m.
# Effective number of candidates: N_eff = 1 / HHI

message("\n=== Computing HHI and effective candidates ===\n")

mun_hhi <- votes |>
  # Only consider candidates with positive votes
  filter(votes > 0, vote_share > 0) |>
  group_by(election_year, state, mun_code_tse) |>
  summarise(
    hhi = compute_hhi(vote_share),
    n_candidates = n(),
    total_votes = sum(votes),
    .groups = "drop"
  ) |>
  mutate(
    n_effective = effective_n_candidates(hhi)
  )

message("  Municipality-year HHI observations: ", nrow(mun_hhi))
message("  Mean HHI: ", round(mean(mun_hhi$hhi, na.rm = TRUE), 4))
message("  Mean effective candidates: ", round(mean(mun_hhi$n_effective, na.rm = TRUE), 2))
message("  Median effective candidates: ", round(median(mun_hhi$n_effective, na.rm = TRUE), 2))

# =============================================================================
# STEP 2: Rank elected legislators within each municipality
# =============================================================================
# For each municipality, rank all ELECTED legislators by their vote share
# in that municipality (from highest to lowest).

message("\n=== Ranking elected legislators within municipalities ===\n")

elected_votes <- votes |>
  filter(elected == TRUE, votes > 0) |>
  # Rank within each municipality-year by vote share (descending)
  group_by(election_year, state, mun_code_tse) |>
  mutate(
    rank_in_mun = rank(-vote_share, ties.method = "min")
  ) |>
  ungroup()

message("  Elected legislator-municipality pairs: ", nrow(elected_votes))

# =============================================================================
# STEP 3: Define "associated candidates"
# =============================================================================
# A legislator i is "associated" with municipality m if:
#   rank(i, m) < N_effective(m)
# i.e., the legislator's rank in the municipality is less than the effective
# number of candidates in that municipality.

message("\n=== Defining associated candidates ===\n")

elected_votes <- elected_votes |>
  left_join(
    mun_hhi |> select(election_year, state, mun_code_tse, n_effective),
    by = c("election_year", "state", "mun_code_tse")
  ) |>
  mutate(
    associated = as.integer(rank_in_mun < n_effective)
  )

# Summary statistics
assoc_summary <- elected_votes |>
  group_by(election_year) |>
  summarise(
    n_pairs = n(),
    n_associated = sum(associated, na.rm = TRUE),
    pct_associated = mean(associated, na.rm = TRUE) * 100,
    .groups = "drop"
  )

message("  Associated candidates by election year:")
for (i in seq_len(nrow(assoc_summary))) {
  message(sprintf("    %d: %d associated out of %d pairs (%.1f%%)",
                  assoc_summary$election_year[i],
                  assoc_summary$n_associated[i],
                  assoc_summary$n_pairs[i],
                  assoc_summary$pct_associated[i]))
}

# =============================================================================
# STEP 4: Aggregate budget amendments by legislator-municipality-term
# =============================================================================
# Sum total amendment value over the legislative term for each
# legislator-municipality pair, then compute per capita.

message("\n=== Aggregating budget amendments by legislative term ===\n")

if (nrow(budget) > 0) {
  # Assign each budget year to a legislative term
  budget <- budget |>
    left_join(
      legislative_terms |>
        rowwise() |>
        mutate(budget_year = list(term_start:term_end)) |>
        unnest(budget_year) |>
        select(election_year, budget_year),
      by = c("year" = "budget_year")
    )

  # Aggregate by legislator-municipality-term
  amendments_term <- budget |>
    filter(!is.na(election_year)) |>
    group_by(election_year, legislator_name, mun_code_ibge) |>
    summarise(
      total_amendment = sum(total_amendment_value, na.rm = TRUE),
      n_amendments_term = sum(n_amendments, na.rm = TRUE),
      .groups = "drop"
    )

  message("  Legislator-municipality-term observations: ", nrow(amendments_term))
} else {
  message("  WARNING: No budget data available. Skipping amendment aggregation.")
  amendments_term <- tibble(
    election_year = integer(),
    legislator_name = character(),
    mun_code_ibge = character(),
    total_amendment = numeric(),
    n_amendments_term = integer()
  )
}

# =============================================================================
# STEP 5: Compute per capita amendments
# =============================================================================

message("\n=== Computing per capita amendments ===\n")

if (nrow(amendments_term) > 0 && nrow(pop) > 0) {
  # Use population from the election year (or closest available)
  # For a term, use population at the start of the term

  amendments_term <- amendments_term |>
    left_join(
      pop |>
        # Keep population for the relevant years
        select(mun_code_ibge, year, population),
      by = c("mun_code_ibge", "election_year" = "year")
    ) |>
    mutate(
      amendments_pc = ifelse(
        !is.na(population) & population > 0,
        total_amendment / population,
        NA_real_
      )
    )

  message("  Mean amendments per capita: R$",
          round(mean(amendments_term$amendments_pc, na.rm = TRUE), 2))
  message("  Median amendments per capita: R$",
          round(median(amendments_term$amendments_pc, na.rm = TRUE), 2))
} else {
  message("  Skipping per capita computation (missing data)")
}

# =============================================================================
# STEP 6: Build analysis panel
# =============================================================================
# The panel has one row per: elected legislator × municipality × election year
# Includes: current vote share, next-election vote share, associated indicator,
# amendments per capita during the term, and controls.

message("\n=== Building analysis panel ===\n")

# Start with elected legislator-municipality pairs
panel <- elected_votes |>
  select(
    election_year, state, mun_code_tse, mun_name,
    candidate_seq, candidate_number, candidate_name, party,
    votes, vote_share, total_votes_mun,
    rank_in_mun, n_effective, associated
  )

# Add next-election vote share (for demand-side analysis)
# A legislator may or may not run in the next election
next_election <- votes |>
  select(
    election_year, mun_code_tse,
    candidate_name, party,
    vote_share_next = vote_share,
    votes_next = votes
  )

# Create election year mapping for "next" election
next_year_map <- tibble(
  election_year = c(1998, 2002, 2006, 2010, 2014, 2018),
  next_election_year = c(2002, 2006, 2010, 2014, 2018, 2022)
)

panel <- panel |>
  left_join(next_year_map, by = "election_year") |>
  left_join(
    next_election,
    by = c(
      "next_election_year" = "election_year",
      "mun_code_tse",
      "candidate_name"
    )
  )

# Indicator for whether candidate ran in next election
panel <- panel |>
  mutate(
    ran_next = !is.na(vote_share_next)
  )

message("  Panel observations: ", nrow(panel))
message("  Candidates running in next election: ",
        sum(panel$ran_next, na.rm = TRUE), " (",
        round(mean(panel$ran_next, na.rm = TRUE) * 100, 1), "%)")

# Merge budget amendments (requires linking legislator names across datasets)
# NOTE: Matching legislators across TSE and budget data is non-trivial.
# We match by name (after normalization) since there's no common ID.
if (nrow(amendments_term) > 0) {
  # Normalize names for matching
  normalize_name <- function(x) {
    x |>
      toupper() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_squish()
  }

  panel <- panel |>
    mutate(name_normalized = normalize_name(candidate_name))

  amendments_term <- amendments_term |>
    mutate(name_normalized = normalize_name(legislator_name))

  # TODO: Need to convert mun_code_tse to mun_code_ibge for matching
  # This requires the municipality crosswalk
  # For now, we leave the merge as a placeholder
  message("  NOTE: Budget amendment merge requires TSE-IBGE code crosswalk.")
  message("  Complete this merge after verifying the crosswalk is correct.")
}

# Add municipality-level controls
panel <- panel |>
  left_join(
    mun_hhi |> select(election_year, mun_code_tse, hhi, n_candidates),
    by = c("election_year", "mun_code_tse")
  )

# =============================================================================
# STEP 7: Save constructed variables and panel
# =============================================================================

message("\n=== Saving constructed data ===\n")

saveRDS(mun_hhi, file.path(data_clean, "municipality_hhi.rds"))
message("  Saved municipality_hhi.rds")

saveRDS(elected_votes, file.path(data_clean, "elected_votes_ranked.rds"))
message("  Saved elected_votes_ranked.rds")

saveRDS(amendments_term, file.path(data_clean, "amendments_by_term.rds"))
message("  Saved amendments_by_term.rds")

saveRDS(panel, file.path(data_clean, "analysis_panel.rds"))
message("  Saved analysis_panel.rds: ", nrow(panel), " rows")

message("\n=== Variable construction complete ===\n")
