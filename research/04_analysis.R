# =============================================================================
# 04_analysis.R
# Main regressions: Supply side and Demand side
# Replicating core tables from Firpo, Ponczek & Sanfelice (2015)
# =============================================================================

source(here::here("research", "_common.R"))

# Load analysis panel
panel <- readRDS(file.path(data_clean, "analysis_panel.rds"))

# Filter to original period
panel_orig <- panel |>
  filter(election_year %in% election_years_original)

message("\n=== Analysis Panel Summary ===\n")
message("  Observations: ", nrow(panel_orig))
message("  Election years: ", paste(unique(panel_orig$election_year), collapse = ", "))
message("  Unique legislators: ",
        n_distinct(panel_orig$candidate_name))
message("  Unique municipalities: ",
        n_distinct(panel_orig$mun_code_tse))

# =============================================================================
# TABLE 1: Descriptive Statistics
# =============================================================================

message("\n=== Table 1: Descriptive Statistics ===\n")

desc_vars <- panel_orig |>
  summarise(
    `Vote share (current)` = sprintf("%.4f (%.4f)",
                                     mean(vote_share, na.rm = TRUE),
                                     sd(vote_share, na.rm = TRUE)),
    `Vote share (next election)` = sprintf("%.4f (%.4f)",
                                           mean(vote_share_next, na.rm = TRUE),
                                           sd(vote_share_next, na.rm = TRUE)),
    `Associated candidate` = sprintf("%.3f (%.3f)",
                                     mean(associated, na.rm = TRUE),
                                     sd(associated, na.rm = TRUE)),
    `HHI` = sprintf("%.4f (%.4f)",
                    mean(hhi, na.rm = TRUE),
                    sd(hhi, na.rm = TRUE)),
    `Effective candidates` = sprintf("%.2f (%.2f)",
                                     mean(n_effective, na.rm = TRUE),
                                     sd(n_effective, na.rm = TRUE)),
    `Ran in next election` = sprintf("%.3f (%.3f)",
                                     mean(ran_next, na.rm = TRUE),
                                     sd(ran_next, na.rm = TRUE)),
    N = as.character(n())
  )

# Print descriptives
desc_long <- desc_vars |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean (SD)")
print(desc_long, n = Inf)

# =============================================================================
# SUPPLY SIDE: Do legislators direct amendments to associated municipalities?
# =============================================================================
# DV: Budget amendments per capita from legislator i to municipality m
# Key IV: Associated candidate indicator
# FE: Legislator, election year
# Clustering: Municipality

message("\n=== Supply Side Regressions ===\n")
message("  DV: Budget amendments per capita")
message("  Key IV: Associated candidate indicator")
message("  Following Firpo et al. (2015), Tables 2-3\n")

# Check if budget amendment data is available
has_budget <- "amendments_pc" %in% names(panel_orig) &&
  sum(!is.na(panel_orig$amendments_pc)) > 0

if (has_budget) {

  # Model S1: Simple OLS
  supply_s1 <- feols(
    amendments_pc ~ associated,
    data = panel_orig,
    vcov = ~mun_code_tse
  )

  # Model S2: With legislator fixed effects
  supply_s2 <- feols(
    amendments_pc ~ associated | candidate_name,
    data = panel_orig,
    vcov = ~mun_code_tse
  )

  # Model S3: With legislator + year fixed effects
  supply_s3 <- feols(
    amendments_pc ~ associated | candidate_name + election_year,
    data = panel_orig,
    vcov = ~mun_code_tse
  )

  # Model S4: With vote share as continuous measure instead of binary
  supply_s4 <- feols(
    amendments_pc ~ vote_share | candidate_name + election_year,
    data = panel_orig,
    vcov = ~mun_code_tse
  )

  # Model S5: Both associated and vote share
  supply_s5 <- feols(
    amendments_pc ~ associated + vote_share | candidate_name + election_year,
    data = panel_orig,
    vcov = ~mun_code_tse
  )

  # Display results
  message("\n--- Supply Side Results ---\n")

  supply_table <- modelsummary(
    list(
      "S1: OLS" = supply_s1,
      "S2: Leg FE" = supply_s2,
      "S3: Leg+Year FE" = supply_s3,
      "S4: Vote Share" = supply_s4,
      "S5: Both" = supply_s5
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "default"
  )

  print(supply_table)

  # Save table
  modelsummary(
    list(
      "S1: OLS" = supply_s1,
      "S2: Leg FE" = supply_s2,
      "S3: Leg+Year FE" = supply_s3,
      "S4: Vote Share" = supply_s4,
      "S5: Both" = supply_s5
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = file.path(output_dir, "table_supply_side.tex"),
    title = "Supply Side: Effect of Electoral Association on Budget Amendment Allocation"
  )

} else {
  message("  WARNING: Budget amendment data not available.")
  message("  Supply-side regressions cannot be estimated.")
  message("  Please ensure budget data is downloaded and merged in scripts 01-03.")
}

# =============================================================================
# DEMAND SIDE: Do voters reward legislators who brought amendments?
# =============================================================================
# DV: Vote share of legislator i in municipality m in NEXT election
# Key IV: Per capita amendments received from legislator i
# FE: Legislator, municipality (or state)
# Clustering: Municipality
# Sample: Legislators who run for reelection

message("\n=== Demand Side Regressions ===\n")
message("  DV: Vote share in next election")
message("  Key IV: Budget amendments per capita")
message("  Sample: Legislators running for reelection")
message("  Following Firpo et al. (2015), Tables 4-5\n")

# Restrict to legislators who ran in next election
panel_demand <- panel_orig |>
  filter(ran_next == TRUE)

message("  Demand-side sample: ", nrow(panel_demand), " observations")

if (has_budget) {

  # Model D1: Simple OLS
  demand_d1 <- feols(
    vote_share_next ~ amendments_pc,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Model D2: Controlling for current vote share
  demand_d2 <- feols(
    vote_share_next ~ amendments_pc + vote_share,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Model D3: With legislator fixed effects
  demand_d3 <- feols(
    vote_share_next ~ amendments_pc + vote_share | candidate_name,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Model D4: With legislator + year fixed effects
  demand_d4 <- feols(
    vote_share_next ~ amendments_pc + vote_share |
      candidate_name + election_year,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Model D5: Close elections subsample
  # Restrict to municipalities where the legislator was "marginally" associated
  # i.e., close to the cutoff of rank < N_effective
  panel_close <- panel_demand |>
    mutate(
      margin = n_effective - rank_in_mun
    ) |>
    filter(abs(margin) <= 2) # Within 2 ranks of the cutoff

  message("  Close elections subsample: ", nrow(panel_close), " observations")

  if (nrow(panel_close) > 50) {
    demand_d5 <- feols(
      vote_share_next ~ amendments_pc + vote_share |
        candidate_name + election_year,
      data = panel_close,
      vcov = ~mun_code_tse
    )
  } else {
    message("  WARNING: Close elections subsample too small for estimation.")
    demand_d5 <- NULL
  }

  # Display results
  message("\n--- Demand Side Results ---\n")

  demand_models <- list(
    "D1: OLS" = demand_d1,
    "D2: Controls" = demand_d2,
    "D3: Leg FE" = demand_d3,
    "D4: Leg+Year FE" = demand_d4
  )
  if (!is.null(demand_d5)) {
    demand_models[["D5: Close Elections"]] <- demand_d5
  }

  demand_table <- modelsummary(
    demand_models,
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "default"
  )

  print(demand_table)

  # Save table
  modelsummary(
    demand_models,
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = file.path(output_dir, "table_demand_side.tex"),
    title = "Demand Side: Effect of Budget Amendments on Future Vote Share"
  )

} else {
  # Without budget data, we can still examine the reduced-form relationship
  # between being an associated candidate and future vote shares
  message("  Budget data not available. Running reduced-form regressions.")
  message("  (Associated candidate -> future vote share, without amendments)\n")

  # Reduced form D1: Does being associated predict higher future vote share?
  rf_d1 <- feols(
    vote_share_next ~ associated,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Reduced form D2: With controls
  rf_d2 <- feols(
    vote_share_next ~ associated + vote_share,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  # Reduced form D3: With fixed effects
  rf_d3 <- feols(
    vote_share_next ~ associated + vote_share |
      candidate_name + election_year,
    data = panel_demand,
    vcov = ~mun_code_tse
  )

  message("\n--- Reduced Form Results (without budget data) ---\n")

  rf_table <- modelsummary(
    list(
      "RF1: OLS" = rf_d1,
      "RF2: Controls" = rf_d2,
      "RF3: Leg+Year FE" = rf_d3
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "default"
  )

  print(rf_table)

  modelsummary(
    list(
      "RF1: OLS" = rf_d1,
      "RF2: Controls" = rf_d2,
      "RF3: Leg+Year FE" = rf_d3
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = file.path(output_dir, "table_reduced_form.tex"),
    title = "Reduced Form: Association and Future Vote Share"
  )
}

# =============================================================================
# ADDITIONAL: RD-style analysis around the association cutoff
# =============================================================================
# The "running variable" is rank_in_mun - n_effective
# The treatment is: associated (rank < n_effective)
# This provides a quasi-experimental identification strategy

message("\n=== RD-Style Analysis Around Association Cutoff ===\n")

panel_rd <- panel_orig |>
  mutate(
    running_var = rank_in_mun - n_effective,
    # Treatment: associated (below cutoff)
    below_cutoff = as.integer(running_var < 0)
  ) |>
  filter(!is.na(running_var))

if (nrow(panel_rd) > 100) {
  # Simple RD: local linear regression around the cutoff
  # Using a bandwidth of +/- 3 ranks
  rd_bw <- 3

  panel_rd_local <- panel_rd |>
    filter(abs(running_var) <= rd_bw)

  message("  RD bandwidth: +/- ", rd_bw, " ranks")
  message("  Observations within bandwidth: ", nrow(panel_rd_local))

  if (has_budget) {
    # RD for supply side
    rd_supply <- feols(
      amendments_pc ~ below_cutoff * running_var | election_year,
      data = panel_rd_local,
      vcov = ~mun_code_tse
    )
    message("\n  RD Supply Side (effect of association on amendments):")
    print(summary(rd_supply))
  }

  # RD for demand side (reduced form)
  if (sum(panel_rd_local$ran_next, na.rm = TRUE) > 50) {
    rd_demand <- feols(
      vote_share_next ~ below_cutoff * running_var | election_year,
      data = panel_rd_local |> filter(ran_next == TRUE),
      vcov = ~mun_code_tse
    )
    message("\n  RD Demand Side (effect of association on future vote share):")
    print(summary(rd_demand))
  }
} else {
  message("  Insufficient observations for RD analysis.")
}

message("\n=== Analysis complete ===\n")
message("Output saved to: ", output_dir)
