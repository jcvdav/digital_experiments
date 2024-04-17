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
				 								post == 1 ~Inf,
				 								T ~ ttt))

event_study <- feols(h / 5 ~ pre + post + i(ttt, "0") | session_id + t,
			panel.id = ~session_id + t,
			data = event_study_panel,
			vcov = "DK")

event_study %>%
	tidy(conf.int = T) %>%
	filter(str_detect(term, "ttt")) %>%
	mutate(term = as.numeric(str_remove(term, "ttt::"))) %>%
	ggplot(aes(x = term, y = estimate)) +
	geom_vline(xintercept = 0, linetype = "dashed") +
	geom_hline(yintercept = 0) +
	geom_linerange(aes(ymin = conf.low, ymax = conf.high),
								 linewidth = 0.5,
								 color = "black") +
	geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
									shape = 21,
									fill = "steelblue",
									linewidth = 1) +
	geom_point(x = 0, y = 0, shape = 21, fill = "steelblue",
						 size = 2, inherit.aes = F) +
	labs(x = "Time-to-shock (rounds)",
			 y = "Estimate ± Std.Err & 95% CI") +
	theme_minimal(base_size = 7) +
	scale_x_continuous(breaks = c(-5:5))

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

ggiplot::ggiplot(object = list("Main text (preferred)" = event_study,
															 "Full range" = full_event_study,
															 "Sun & Abrahams" = sunab_event_study)) +
	theme_minimal(base_size = 7) +
	theme(legend.position = "bottom") +
	labs(x = "Time-to-treatment (rounds)",
			 title = "",
			 color = "Estimation",
			 shape = "Estimation",
			 fill = "Estimation")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
