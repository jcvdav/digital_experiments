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
	readxl,
	tidyverse
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

orig_h <- ggplot(data = original_tabular_data,
								 mapping = aes(x = t, y = H / 25, color = game, fill = game)) +
	stat_summary(geom = "ribbon",
							 fun.data = mean_se,
							 alpha = 0.25) +
	stat_summary(geom = "line",
							 fun = mean,
							 linewidth = 1) +
	scale_y_continuous(limits = c(0, 1)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
	labs(x = "Round",
			 y = "Average group catch rate") +
	theme_minimal(base_size = 7) +
	theme(legend.position = "None",
				panel.grid = element_blank()) +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill"))

orig_N <- ggplot(data = original_tabular_data,
								 mapping = aes(x = t, y = last_N, color = game, fill = game)) +
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
			 fill = "Treatment") +
	theme_minimal(base_size = 7) +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill")) +
	theme(legend.position = "None",
				panel.grid = element_blank())


our_h <- tabular_data %>%
  ggplot(aes(x = t, y = h/5, color = game, fill = game)) +
  stat_summary(geom = "ribbon",
               fun.data = mean_se,
               alpha = 0.25) +
  stat_summary(geom = "line",
               fun = mean,
               linewidth = 1) +
	scale_y_continuous(limits = c(0, 1)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
  labs(x = "Round",
       y = "Average player's catch rate") +
	theme_minimal(base_size = 7) +
  theme(legend.position = "None",
        panel.grid = element_blank()) +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill"))

our_N <- tabular_data %>%
  ggplot(aes(x = t, y = last_N, color = game, fill = game)) +
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
       fill = "Treatment") +
	theme_minimal(base_size = 7) +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill")) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank(),
        panel.grid = element_blank())

cowplot::plot_grid(orig_h,
									 our_h,
									 orig_N,
									 our_N,
									 ncol = 2,
									 labels = "AUTO",
									 label_x = 0.9,
									 align = "hv")
