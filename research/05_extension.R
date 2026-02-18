# =============================================================================
# 05_extension.R
# Extend analysis to recent elections (2014, 2018, 2022)
# Compare results with original period (1998-2010)
# =============================================================================

source(here::here("research", "_common.R"))

# Load analysis panel (constructed in 03_construct_variables.R)
panel <- readRDS(file.path(data_clean, "analysis_panel.rds"))

# Split into original and extension periods
panel_orig <- panel |>
  filter(election_year %in% election_years_original)

panel_ext <- panel |>
  filter(election_year %in% election_years_extension)

panel_all <- panel

message("\n=== Extension Analysis ===\n")
message("  Original period: ", paste(election_years_original, collapse = ", "))
message("  Extension period: ", paste(election_years_extension, collapse = ", "))
message("  Original obs: ", nrow(panel_orig))
message("  Extension obs: ", nrow(panel_ext))

# =============================================================================
# NOTE ON INSTITUTIONAL CHANGES
# =============================================================================
# Key institutional changes after the original period:
#
# 1. EC 86/2015 (Constitutional Amendment): Made individual budget amendments
#    "impositivas" (mandatory execution), meaning the executive must execute
#    at least a minimum share of individual amendments. This changed the
#    strategic calculus of amendments significantly.
#
# 2. EC 100/2019: Extended mandatory execution to bench amendments.
#
# 3. EC 126/2022 and subsequent changes further modified amendment rules.
#
# These changes may affect both the supply and demand sides:
# - Supply: With mandatory execution, the bargaining value of amendments
#   may have changed, but legislators still choose WHERE to direct them.
# - Demand: Voters may be more aware of amendments if execution is guaranteed.
# =============================================================================

# =============================================================================
# TABLE E1: Descriptive statistics by period
# =============================================================================

message("\n=== Comparing periods: Descriptive statistics ===\n")

desc_by_period <- panel |>
  mutate(period = ifelse(
    election_year %in% election_years_original,
    "Original (1998-2010)",
    "Extension (2014-2022)"
  )) |>
  group_by(period) |>
  summarise(
    mean_vote_share = mean(vote_share, na.rm = TRUE),
    mean_associated = mean(associated, na.rm = TRUE),
    mean_hhi = mean(hhi, na.rm = TRUE),
    mean_n_effective = mean(n_effective, na.rm = TRUE),
    pct_ran_next = mean(ran_next, na.rm = TRUE),
    n_obs = n(),
    n_legislators = n_distinct(candidate_name),
    n_municipalities = n_distinct(mun_code_tse),
    .groups = "drop"
  )

print(desc_by_period)

# =============================================================================
# SUPPLY SIDE: Extension period
# =============================================================================

message("\n=== Supply Side: Extension Period ===\n")

has_budget <- "amendments_pc" %in% names(panel) &&
  sum(!is.na(panel$amendments_pc)) > 0

if (has_budget) {

  # Extension period only
  ext_s1 <- feols(
    amendments_pc ~ associated | candidate_name + election_year,
    data = panel_ext,
    vcov = ~mun_code_tse
  )

  # Full period (pooled)
  full_s1 <- feols(
    amendments_pc ~ associated | candidate_name + election_year,
    data = panel_all,
    vcov = ~mun_code_tse
  )

  # Interaction with period indicator
  panel_all <- panel_all |>
    mutate(post_2010 = as.integer(election_year > 2010))

  interact_s1 <- feols(
    amendments_pc ~ associated * post_2010 | candidate_name + election_year,
    data = panel_all,
    vcov = ~mun_code_tse
  )

  message("\n--- Supply Side: Comparing Periods ---\n")

  modelsummary(
    list(
      "Original (1998-2010)" = feols(
        amendments_pc ~ associated | candidate_name + election_year,
        data = panel_orig, vcov = ~mun_code_tse
      ),
      "Extension (2014-2022)" = ext_s1,
      "Full Period" = full_s1,
      "Interaction" = interact_s1
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = "default"
  ) |> print()

  # Save
  modelsummary(
    list(
      "Original" = feols(
        amendments_pc ~ associated | candidate_name + election_year,
        data = panel_orig, vcov = ~mun_code_tse
      ),
      "Extension" = ext_s1,
      "Full" = full_s1,
      "Interaction" = interact_s1
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = file.path(output_dir, "table_supply_extension.tex"),
    title = "Supply Side: Original vs. Extension Period"
  )

} else {
  message("  Budget data not available. Skipping supply-side extension.")
}

# =============================================================================
# DEMAND SIDE: Extension period
# =============================================================================

message("\n=== Demand Side: Extension Period ===\n")

panel_demand_ext <- panel_ext |> filter(ran_next == TRUE)
panel_demand_all <- panel_all |> filter(ran_next == TRUE)
panel_demand_orig <- panel_orig |> filter(ran_next == TRUE)

if (has_budget) {

  # Extension period
  ext_d1 <- feols(
    vote_share_next ~ amendments_pc + vote_share |
      candidate_name + election_year,
    data = panel_demand_ext,
    vcov = ~mun_code_tse
  )

  # Full period
  full_d1 <- feols(
    vote_share_next ~ amendments_pc + vote_share |
      candidate_name + election_year,
    data = panel_demand_all,
    vcov = ~mun_code_tse
  )

  # Interaction
  panel_demand_all <- panel_demand_all |>
    mutate(post_2010 = as.integer(election_year > 2010))

  interact_d1 <- feols(
    vote_share_next ~ amendments_pc * post_2010 + vote_share |
      candidate_name + election_year,
    data = panel_demand_all,
    vcov = ~mun_code_tse
  )

  message("\n--- Demand Side: Comparing Periods ---\n")

  modelsummary(
    list(
      "Original" = feols(
        vote_share_next ~ amendments_pc + vote_share |
          candidate_name + election_year,
        data = panel_demand_orig, vcov = ~mun_code_tse
      ),
      "Extension" = ext_d1,
      "Full" = full_d1,
      "Interaction" = interact_d1
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = "default"
  ) |> print()

  modelsummary(
    list(
      "Original" = feols(
        vote_share_next ~ amendments_pc + vote_share |
          candidate_name + election_year,
        data = panel_demand_orig, vcov = ~mun_code_tse
      ),
      "Extension" = ext_d1,
      "Full" = full_d1,
      "Interaction" = interact_d1
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = file.path(output_dir, "table_demand_extension.tex"),
    title = "Demand Side: Original vs. Extension Period"
  )

} else {
  # Reduced form with association indicator
  message("  Running reduced-form extension (without budget data).")

  rf_orig <- feols(
    vote_share_next ~ associated + vote_share |
      candidate_name + election_year,
    data = panel_demand_orig,
    vcov = ~mun_code_tse
  )

  rf_ext <- feols(
    vote_share_next ~ associated + vote_share |
      candidate_name + election_year,
    data = panel_demand_ext,
    vcov = ~mun_code_tse
  )

  rf_all <- feols(
    vote_share_next ~ associated + vote_share |
      candidate_name + election_year,
    data = panel_demand_all,
    vcov = ~mun_code_tse
  )

  panel_demand_all <- panel_demand_all |>
    mutate(post_2010 = as.integer(election_year > 2010))

  rf_interact <- feols(
    vote_share_next ~ associated * post_2010 + vote_share |
      candidate_name + election_year,
    data = panel_demand_all,
    vcov = ~mun_code_tse
  )

  message("\n--- Reduced Form: Comparing Periods ---\n")

  modelsummary(
    list(
      "Original" = rf_orig,
      "Extension" = rf_ext,
      "Full" = rf_all,
      "Interaction" = rf_interact
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = "default"
  ) |> print()

  modelsummary(
    list(
      "Original" = rf_orig,
      "Extension" = rf_ext,
      "Full" = rf_all,
      "Interaction" = rf_interact
    ),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    output = file.path(output_dir, "table_rf_extension.tex"),
    title = "Reduced Form: Association and Future Vote Share by Period"
  )
}

# =============================================================================
# FIGURE: Effect over time (coefficient plot)
# =============================================================================

message("\n=== Coefficient Plot: Effect by Election Year ===\n")

# Estimate the key coefficient separately for each election year
year_coefs <- list()

for (yr in sort(unique(panel$election_year))) {
  panel_yr <- panel |>
    filter(election_year == yr, ran_next == TRUE)

  if (nrow(panel_yr) < 50) next

  tryCatch({
    if (has_budget) {
      m <- feols(
        vote_share_next ~ amendments_pc + vote_share,
        data = panel_yr,
        vcov = ~mun_code_tse
      )
      coef_name <- "amendments_pc"
    } else {
      m <- feols(
        vote_share_next ~ associated + vote_share,
        data = panel_yr,
        vcov = ~mun_code_tse
      )
      coef_name <- "associated"
    }

    year_coefs[[as.character(yr)]] <- tibble(
      election_year = yr,
      estimate = coef(m)[coef_name],
      se = se(m)[coef_name],
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )
  }, error = function(e) {
    message("  Could not estimate for ", yr, ": ", conditionMessage(e))
  })
}

if (length(year_coefs) > 0) {
  coef_plot_data <- bind_rows(year_coefs) |>
    mutate(
      period = ifelse(
        election_year <= 2010,
        "Original",
        "Extension"
      )
    )

  # Save coefficient plot
  p <- ggplot(coef_plot_data, aes(x = election_year, y = estimate, color = period)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2012, linetype = "dotted", color = "gray70") +
    scale_color_manual(values = c("Original" = "steelblue", "Extension" = "firebrick")) +
    labs(
      x = "Election Year",
      y = ifelse(has_budget, "Coefficient on Amendments PC", "Coefficient on Associated"),
      title = "Effect on Future Vote Share by Election Year",
      subtitle = "Firpo et al. (2015) replication and extension",
      color = "Period"
    ) +
    theme_minimal(base_size = 14)

  ggsave(
    file.path(output_dir, "fig_coef_by_year.pdf"),
    p, width = 8, height = 5
  )
  ggsave(
    file.path(output_dir, "fig_coef_by_year.png"),
    p, width = 8, height = 5, dpi = 300
  )

  message("  Saved coefficient plot to output/")
}

message("\n=== Extension analysis complete ===\n")
