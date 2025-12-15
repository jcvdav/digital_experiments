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

theme_set(
	theme_minimal(base_size = 8)
)

# Load data --------------------------------------------------------------------
raw_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
shock_times <- raw_data %>%
	filter(game == "Uncertainty",
				 s < 1) %>%
	select(session_id, g, shock_t = t)

event_study_panel <- shock_times %>%
	left_join(raw_data, by = join_by(session_id, g), relationship = "many-to-many") %>%
	mutate(ttt = t - shock_t,
				 pre = 1 * (ttt < -5),
				 post = 1 * (ttt > 5),
				 full_ttt = ttt,
				 ttt = case_when(pre == 1 ~ -Inf,
				 								post == 1 ~ Inf,
				 								T ~ ttt))

event_study <- feols(h / 5 ~ pre + post + i(ttt, "0") | session_id + t,
			panel.id = ~session_id + t,
			data = event_study_panel,
			vcov = "DK")

plot <- event_study %>%
	broom::tidy(conf.int = T) %>%
	filter(str_detect(term, "ttt")) %>%
	mutate(term = as.numeric(str_remove(term, "ttt::"))) %>%
	bind_rows(tibble(term = 0,
									 estimate = 0,
									 std.error = 0,
									 conf.low = 0,
									 conf.high = 0)) %>%
	ggplot(aes(x = term, y = estimate)) +
	geom_vline(xintercept = 0, linetype = "dashed") +
	geom_hline(yintercept = 0) +
	geom_line(linetype = "dashed") +
	geom_linerange(aes(ymin = conf.low, ymax = conf.high),
								 color = "black") +
	geom_pointrange(aes(ymin = estimate - std.error,
											ymax = estimate + std.error),
									color = "steelblue",
									linewidth = 1.5) +
	labs(x = "Time to shock (rounds)",
			 y = "Estimate ± Std.Err & 95% CI") +
	theme_minimal(base_size = 7) +
	scale_x_continuous(breaks = c(-5:5))


startR::lazy_ggsave(plot = plot,
										filename = "fig6_event_study",
										width = 12,
										height = 6)

# Robustness -------------------------------------------------------------------
# No pre / post dummies - full range estimation
full_event_study <- feols(h / 5 ~ i(full_ttt, "0") | session_id + t,
										 panel.id = ~session_id + t,
										 data = event_study_panel,
										 vcov = "DK")

# Using the Sun and Abraham estimator
sunab_event_study <- feols(h / 5 ~ sunab(shock_t, t) | session_id + t,
													 panel.id = ~session_id + t,
													 data = event_study_panel)

supp_event_study <- ggfixest::ggiplot(object = list("Main text (preferred)" = event_study,
																"Full range" = full_event_study,
																"Sun & Abrahams" = sunab_event_study)) +
	theme_minimal(base_size = 7) +
	theme(legend.position = "bottom") +
	labs(x = "Time to treatment (rounds)",
			 title = "",
			 color = "Estimation",
			 shape = "Estimation",
			 fill = "Estimation")

startR::lazy_ggsave(plot = supp_event_study,
										filename = "figS1_supp_event_study",
										width = 12,
										height = 8)


modelsummary(event_study,
						 stars = panelsummary:::econ_stars(),
						 shape = term ~ model + statistic,
						 output = here("results", "tab", "tabS1_event_study.tex"),
						 gof_omit = "R|IC|Std.|FE",
						 title = "\\label{tab:event_study}\\textbf{Coefficient estimates for event study}",
						 notes = "ttt indicates 'time-to-treatment', with negative values ocurring before shock and positive values after shock.",
						 escape = F)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
