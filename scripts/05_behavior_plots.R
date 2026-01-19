################################################################################
# Create validation plots comparing digital and in-person game data
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
#
# This script creates validation plots comparing digital game results with
# original in-person game data. It visualizes harvest rates and population
# sizes over time for both baseline and uncertainty treatments, comparing
# digital versus in-person game formats. Outputs fig4_state_vars.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
	here,
	readxl,
	tidyverse
)

theme_set(
	theme_minimal(base_size = 7)
)

# Load data --------------------------------------------------------------------
# Data from the Finkbeiner paper
original_data <- read_excel(here("data/raw/Baja baseline data games 2015.xls"))

# Our data
tabular_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

## PROCESSING ##################################################################

original_tabular_data <- original_data %>%
	select(date = fecha,
				 site = lugar,
				 monitor,
				 time = hora,
				 game = juego,
				 treatment = tratamiento,
				 t = ronda,
				 H = capturatotal,
				 last_N = nivelrecurso) %>%
	distinct() %>%
	drop_na()

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################
palette <- c(Baseline = "darkorange1",
						 A = "darkorange1",
             Uncertainty = "steelblue",
						 B = "steelblue")


min_orig <- original_tabular_data %>%
	select(t, h = H, N = last_N) %>%
	mutate(src = "in person",
				 game = "Baseline",
				 h = h / 25)

min_dig <- tabular_data %>%
	select(t, h, N = Nt, game) %>%
	mutate(src = "digital",
				 h = h / 5)

combined <- bind_rows(min_orig, min_dig)

H <- ggplot(data = combined,
						mapping = aes(x = t, y = h, color = game, fill = game, linetype = src)) +
	stat_summary(geom = "ribbon",
							 fun.data = mean_se,
							 alpha = 0.25) +
	stat_summary(geom = "line",
							 fun = mean,
							 linewidth = 1) +
	scale_y_continuous(limits = c(0, 1)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
	labs(x = "Round",
			 y = "Average harvest rate",
			 color = "Treatment",
			 fill = "Treatment",
			 linetype = "Source") +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill")) +
	theme(legend.position = "inside",
				legend.justification.inside = c(1, 1),
				legend.direction = "horizontal")

N <- ggplot(data = combined,
						mapping = aes(x = t, y = N, color = game, fill = game, linetype = src)) +
	stat_summary(geom = "ribbon",
							 fun.data = mean_se,
							 alpha = 0.25) +
	stat_summary(geom = "line",
							 fun = mean,
							 linewidth = 1) +
	scale_y_continuous(limits = c(0, 100)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
	labs(x = "Round",
			 y = "Average population size",
			 color = "Treatment",
			 fill = "Treatment",
			 linetype = "Source") +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill")) +
	theme(legend.position = "None")

plot <- cowplot::plot_grid(H, N,
													 ncol = 2,
													 labels = "auto",
													 vjust = 0.9,
													 align = "hv")

startR::lazy_ggsave(plot = plot,
										filename = "fig4_state_vars",
										width = 12,
										height = 6)
