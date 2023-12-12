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
	tidyverse
)

# Load data --------------------------------------------------------------------
# Data from the Finkbeiner paper
original_data <- read_excel(here("data/raw/Baja baseline data games 2015.xls"))

# Our data
tabular_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

## PROCESSING ##################################################################

original_plot_data <- original_data %>%
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
plot_data <- tabular_data %>%
	filter(both_games == 1,
				 !fisher == 2)

## VISUALIZE ###################################################################
palette <- c(Baseline = "darkorange1",
						 A = "darkorange1",
             Uncertainty = "steelblue",
						 B = "steelblue")

orig_h <- ggplot(data = original_plot_data,
								 mapping = aes(x = t, y = H, color = game, fill = game)) +
	stat_summary(geom = "ribbon",
							 fun.data = mean_se,
							 alpha = 0.25) +
	stat_summary(geom = "line",
							 fun = mean,
							 linewidth = 1) +
	scale_y_continuous(limits = c(0, 25)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
	labs(x = "Round",
			 y = "Average group catch") +
	theme_bw() +
	theme(legend.position = "None",
				panel.grid = element_blank()) +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill"))

orig_N <- ggplot(data = original_plot_data,
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
	theme_bw() +
	scale_fill_manual(values = palette,
										aesthetics = c("color", "fill")) +
	theme(legend.position = "None",
				panel.grid = element_blank())


our_h <- plot_data %>%
  ggplot(aes(x = t, y = h, color = game, fill = game)) +
  stat_summary(geom = "ribbon",
               fun.data = mean_se,
               alpha = 0.25) +
  stat_summary(geom = "line",
               fun = mean,
               linewidth = 1) +
	scale_y_continuous(limits = c(0, 5)) +
	scale_x_continuous(breaks = c(0, 5, 10, 15), labels = c(0, 5, 10, 15)) +
  labs(x = "Round",
       y = "Average player's catch") +
  theme_bw() +
  theme(legend.position = "None",
        panel.grid = element_blank()) +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill"))

our_N <- plot_data %>%
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
  theme_bw() +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill")) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank(),
        panel.grid = element_blank())

cowplot::plot_grid(orig_h,
									 orig_N,
									 our_h,
									 our_N,
									 ncol = 2,
									 labels = "auto",
									 align = "hv")



# X ----------------------------------------------------------------------------


plot_data %>%
	filter(g == 2) %>%
	group_by(session_id) %>%
	filter(any(s == 0.5)) %>%
	mutate(event = (t - min(t[s == 0.5], na.rm = T))) %>%
	ggplot(aes(x = event, y = h)) +
	stat_summary(geom = "ribbon",
							 fun.data = mean_se)



plot_data %>%
	filter(both_games == 1) %>%
	group_by(session_id) %>%
	filter(any(s == 0.5)) %>%
	mutate(event = (t - min(t[s == 0.5], na.rm = T))) %>%
	feols(h ~ event * game | session_id)
