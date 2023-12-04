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



lm(h/5 ~ t + game + factor(region),
			data = plot_data) %>%
	summary()

plot_data %>%
	group_by(had_shock) %>%
	summarize(h = mean(h))


