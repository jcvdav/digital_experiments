################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
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

coeftable(model) %>%
	janitor::clean_names() %>%
	mutate(sample = fct_relevel(sample, c("Full sample",
																				"Both treatments",
																				"Only baseline")),
				 coefficient = case_when(coefficient == "t" ~ "Round",
				 												T ~ "Env. Uncertainty"),
				 coefficient = fct_relevel(coefficient, "Round", "Env. Uncertainty")) %>%
	ggplot(aes(x = sample, y = estimate)) +
	geom_pointrange(aes(ymin = estimate - std_error,
											ymax = estimate + std_error),
									fill = "steelblue",
									shape = 21) +
	geom_hline(data = fink_coefs,
						 aes(yintercept = values),
						 linetype = "dashed") +
	geom_hline(yintercept = 0) +
	facet_wrap(~coefficient, scales = "free_y", ncol = 2) +
	theme_minimal(base_size = 7) +
	labs(x = "",
			 y = "Estimate ± Std.Err")

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

msummary(models = list("Panel A) Validation analysis" = model,
											 "Panel B) Information only" = robust_reduced),
				 shape = "rbind",
				 stars = panelsummary:::econ_stars(),
				 gof_omit = "R|IC|Std.|FE",
				 coef_map = c("t" = "Round",
				 						 "gameUncertainty" = "Env. Uncertainty"),
				 notes = c("Each column represents results for a different sample.
				          Each panel represents a different test.
				 					The third column (Only baseline) can not estimate a treatment effect because there is no treatment with envronmental uncertainty (10 sessions).
				 					Numbers in parentheses are Driscol-Kraay Standard errors.
				 					All specifications include fixed-effects by region."))
