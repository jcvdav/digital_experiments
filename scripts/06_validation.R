################################################################################
# Perform statistical validation of game effects
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
#
# This script performs statistical validation of game effects using fixed-effects
# regression. It estimates effects of game round and environmental uncertainty
# on catch rate, compares results to original study coefficients, and tests
# robustness using information-only analysis. Outputs fig5_effects and tab1_effects.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
	here,
	fixest,
	modelsummary,
	tidyverse
)

theme_set(
	theme_minimal(base_size = 8)
)

# Load data --------------------------------------------------------------------
raw_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds")) %>%
	mutate(both_games = ifelse(both_games == 1, "Both treatments", "Only baseline"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
model <- fixest::feols(h / 5 ~ t + game | region,
							data = raw_data,
							panel.id = ~session_id + t,
							vcov = "DK",
							fsplit = ~both_games)

fink_coefs <- tibble(coefficient = c("Round", "Env. Uncertainty"),
										 values = c(-0.012, -0.016)) %>%
	mutate(coefficient = fct_relevel(coefficient, "Round", "Env. Uncertainty"))

plot <- coeftable(model) %>%
	janitor::clean_names() %>%
	mutate(sample = fct_relevel(sample, c("Full sample",
																				"Both treatments",
																				"Only baseline")),
				 coefficient = case_when(coefficient == "t" ~ "Round",
				 												T ~ "Env. Uncertainty"),
				 coefficient = fct_relevel(coefficient, "Round", "Env. Uncertainty")) %>%
	ggplot(aes(x = sample, y = estimate)) +
	geom_linerange(aes(ymin = estimate - (1.96 * std_error),
										 ymax = estimate + (1.95 * std_error))) +
	geom_pointrange(aes(ymin = estimate - (std_error),
											ymax = estimate + (std_error)),
									color = "steelblue",
									linewidth = 1.5) +
	geom_hline(data = fink_coefs,
						 aes(yintercept = values),
						 linetype = "dashed") +
	geom_hline(yintercept = 0) +
	facet_wrap(~coefficient, scales = "free", ncol = 2) +
	labs(x = "",
			 y = "Estimate ± Std.Err")

startR::lazy_ggsave(plot = plot,
										filename = "fig5_effects",
										width = 12,
										height = 6)

## Source of reductions
# X ----------------------------------------------------------------------------
shock_times <- raw_data %>%
	filter(game == "Uncertainty",
				 s < 1) %>%
	select(session_id, g, shock_t = t)


##############
alt_data <- raw_data %>%
	left_join(shock_times %>% group_by(session_id, g) %>%
							summarize(shock_t = min(shock_t), .groups = "drop"),
						by = c("session_id", "g"), relationship = "many-to-many") %>%
	mutate(post = 1 * (t >= shock_t)) %>%
	replace_na(replace = list(post = 0)) %>%
	filter(post == 0)


robust_reduced <- fixest::feols(h / 5 ~ t + game | region,
																data = alt_data %>% filter(post == 0),
																panel.id = ~session_id + t,
																vcov = "DK",
																fsplit = ~both_games)

names(model) <-  c("Full", "Both treatments", "Baseline only")
names(robust_reduced) <-  c("Full", "Both treatments", "Baseline only")

msummary(models = list("Panel A) Validation analysis" = model,
											 "Panel B) Information only" = robust_reduced),
				 output = here("results", "tab", "tab1_effects.tex"),
				 shape = "rbind",
				 stars = panelsummary:::econ_stars(),
				 gof_omit = "R|IC|Std.|FE",
				 coef_map = c("t" = "Round",
				 						 "gameUncertainty" = "Env. Uncertainty"),
				 title = "\\label{tab:effects}\\textbf{Coefficient estimates for the effect of game round and environmental uncertainty on catch rate.} Panel A shows summary statistics associated with the validation results plotted in Fig 5. Panel B shows results for testing for the effect of information alone.",
				 notes = c("Each column represents results for a different sample. Each panel represents a different test. Numbers in parentheses are Driscol-Kraay Standard errors. All specifications include fixed-effects by region."),
				 escape = F)
