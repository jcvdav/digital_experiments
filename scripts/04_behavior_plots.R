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
tabular_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
plot_data <- tabular_data %>%
	filter(both_games == 1)

## VISUALIZE ###################################################################
palette <- c(Base = "darkorange1",
             Incertidumbre = "steelblue")



p_h <- plot_data %>%
  ggplot(aes(x = t, y = h, color = had_shock, fill = had_shock)) +
  stat_summary(geom = "ribbon",
               fun.data = mean_se,
               alpha = 0.25) +
  stat_summary(geom = "line",
               fun = mean,
               linewidth = 1) +
  labs(x = "Viaje",
       y = "Captura promedio de cada juego") +
  theme_bw() +
  theme(legend.position = "None",
        panel.grid = element_blank()) +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill"))

p_N <- plot_data %>%
  ggplot(aes(x = t, y = last_N, color = had_shock, fill = had_shock)) +
  stat_summary(geom = "ribbon",
               fun.data = mean_se,
               alpha = 0.25) +
  stat_summary(geom = "line",
               fun = mean,
               linewidth = 1) +
  labs(x = "Viaje",
       y = "Población promedio",
       color = "Juego",
       fill = "Juego") +
  theme_bw() +
  scale_fill_manual(values = palette,
                    aesthetics = c("color", "fill")) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank(),
        panel.grid = element_blank())

cowplot::plot_grid(p_h, p_N)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
