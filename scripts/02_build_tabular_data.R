################################################################################
# Build tabular data from raw game data
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
#
# This script processes raw game data into a structured tabular format. It
# extracts dates, metadata (bug, fisher, age, sex, region, phone), processes
# game rounds, and creates variables for game type (Baseline vs Uncertainty)
# and shock type (MHW vs No). Outputs processed data for analysis.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
	here,
	tidyverse
)

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "raw", "raw_game_data.rds"))

## PROCESSING ##################################################################

tabular_data <- data %>%
	select(session_id, data) %>%
	# Extract the date
	mutate(date = str_extract(session_id, "[:alnum:]{3}_[:alnum:]{3}_[:digit:]+_"),
				 date = paste0(date, "2023"),
				 date = str_replace_all(date, "_", " "),
				 date = str_remove(date, "[:alpha:]{3}"),
				 date = str_trim(date),
				 date = lubridate::mdy(date)) %>%
	# Extract the metadata
	mutate(session_id = str_replace_all(string = session_id,
																			pattern = "NA", replacement = "0"),
				 metadata = str_extract(string = session_id,
				 											 pattern = "_[:digit:]_[:digit:]_[:digit:]_[:digit:]_[:digit:]_"),
				 bug = str_sub(metadata, 2, 2),
				 fisher = str_sub(metadata, 4, 4),
				 age = str_sub(metadata, 6, 6),
				 sex = str_sub(metadata, 8, 8),
				 region = str_sub(metadata, 10, 10),
				 phone = str_extract(string = session_id, pattern = "[:digit:]{10}")) %>%
	# Extract the data
	mutate(data = map(data, ~.x$result)) %>%
	unnest(data) %>%
	filter(!g == 0) %>%
	# Build a dummy variable for sessions with baseline and shock
	group_by(session_id) %>%
	mutate(both_games = 1 * (max(g) >= 2)) %>%
	ungroup() %>%
	mutate(game = ifelse(g >= 2, "Uncertainty", "Baseline"),
				 shock = ifelse(s == 0.5, "MHW", "No")) %>%
	select(session_id, both_games, date, bug, fisher, age, sex, region, phone, g, game, t, s, shock, last_N, H, E, Nt, h)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = tabular_data,
				file = here("data", "processed", "tabular_game_data.rds"))
