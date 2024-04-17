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
	rnaturalearth,
	sf,
	tidyverse
)

# Load data --------------------------------------------------------------------
state_counts_raw <- read_csv(here("data/raw/lugares_alcance_post.csv"))
demographics_raw <- read_csv(here("data/raw/edades_alcance_post.csv"))
tabular_data <- readRDS(file = here("data", "processed", "tabular_game_data.rds"))

# Spatial layers ---------------------------------------------------------------
mex_zones <- st_read(here("../data_mex_fisheries/data/spatial_features/clean/mexico_fishing_regions.gpkg")) %>%
	st_simplify()
mex <- ne_states(country = "Mexico", returnclass = "sf") %>%
	select(state = name)

## PROCESSING ##################################################################

# Interactions by state --------------------------------------------------------
state_counts <- state_counts_raw %>%
	rename(post = Pauta,
				 state = Estado,
				 n = N) %>%
	mutate(state = case_when(state == "San Luis" ~ "San Luis Potosí",
													 state == "Estado de México" ~ "México",
													 T ~ state)) %>%
	group_by(state) %>%
	summarize(n = sum(n), .groups = "drop")

data <- mex %>%
	left_join(state_counts, by = "state")

# Interactions by age / gender -------------------------------------------------
demographics <- demographics_raw %>%
	janitor::clean_names() %>%
	select(age_bracket = rango_edad, female = number_mujeres, male = number_hombres) %>%
	group_by(age_bracket) %>%
	summarize_all(sum) %>%
	pivot_longer(cols = c(female, male),
							 values_to = "count",
							 names_to = "gender") %>%
	mutate(gender = str_to_sentence(gender))

# Responses by fishing region --------------------------------------------------
sample_data <- tabular_data %>%
	select(region, session_id, g) %>%
	distinct() %>%
	count(region) %>%
	mutate(region = case_when(region == 1 ~ "BC Pacifico",
														region == 2 ~ "Golfo de California",
														region == 3 ~ "Pacífico Sur",
														region == 4 ~ "Golfo de México",
														region == 5 ~ "Caribe",
														T ~ "Not specified"))

zones <- mex_zones %>%
	mutate(region = case_when(region == 1 ~ "BC Pacifico",
														region == 2 ~ "Golfo de California",
														region == 3 ~ "Pacífico Sur",
														region == 4 ~ "Pacífico Sur",
														region == 5 ~ "Golfo de México",
														region == 6 ~ "Caribe",
														T ~ "Not specified")) %>%
	group_by(region) %>%
	summarize()

player_stats <- zones %>%
	left_join(sample_data, by = "region")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
map <- ggplot(data = data, aes(fill = n)) +
	geom_sf(color = "black") +
	theme_minimal(base_size = 7) +
	scale_fill_gradient(low = "white", high = "steelblue") +
	guides(fill = guide_legend(title = "Interactions",
														 ticks.colour = "black",
														 frame.colour = "black")) +
	# scale_fill_viridis_c(option = "mako") +
	new_scale_fill() +
	geom_sf(data = player_stats, aes(fill = n), color = "black") +
	scale_fill_gradient(low = "white", high = "cadetblue") +
	# scale_fill_viridis_c() +
	guides(fill = guide_legend(title = "Players",
	ticks.colour = "black",
	frame.colour = "black")) +
	theme(legend.position = c(1, 1),
				legend.justification = c(1, 1), legend.box = "horizontal")

map
# X ----------------------------------------------------------------------------
demo <- ggplot(demographics,
			 aes(x = age_bracket, y = count, fill = gender)) +
	geom_col(position = "dodge",
					 color = "black") +
	scale_fill_manual(values = c("white", "black")) +
	labs(x = "Age bracket",
			 y = "Count",
			 fill = "FaceBook\nGender") +
	theme_minimal(base_size = 7) +
	theme(legend.position = c(1, 1),
				legend.justification = c(1, 1))


cowplot::plot_grid(map, demo,
									 ncol = 1,
									 labels = c("AUTO"),
									 rel_heights = c(2, 1))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------






















