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
raw_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
tabular_data <- raw_data %>%
	filter(both_games == 1)

fixest::feols(h / 5 ~ 1 +  t + game | region, data = tabular_data) %>%
	etable()

lm(h/5 ~ t + game + factor(region),
			data = tabular_data) %>%
	summary()

plot_data %>%
	group_by(had_shock) %>%
	summarize(h = mean(h))


