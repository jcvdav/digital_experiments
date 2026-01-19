################################################################################
# Analyze participant survival and retention rates
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
#
# This script analyzes participant survival and retention rates through the
# game. It calculates survival matrices showing progression from social media
# access to game entry to completing rounds, and creates cumulative response
# plots over time. Outputs fig3_survival.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
	here,
	tidyverse
)

theme_set(
	theme_minimal(base_size = 8)
)

# Load data --------------------------------------------------------------------
raw_game_data <- readRDS(file = here("data", "raw", "raw_game_data.rds"))

## PROCESSING ##################################################################

# Get some stats ---------------------------------------------------------------
# Number of accesses
raw_game_data %>%
  pull(session_id) %>%
  unique() %>%
  length()

# Number of games with at least one move
raw_game_data %>%
  filter(map_dbl(data, ~dim(.x$result)[1]) > 1) %>%
  pull(session_id) %>%
  unique() %>%
  length()

raw_game_data %>%
  filter(map_dbl(data, ~max(.x$result$g)) >= 2) %>%
  pull(session_id) %>%
  unique() %>%
  length()

get_n <- function(data, n = 1) {
  map_lgl(data, ~(max(.x$result$g) >= n)) %>%
    sum()
}

## VISUALIZE ###################################################################

suv_mat <- expand_grid(from = c(3369, 55, 21, 11),
						to = c(3369, 55, 21, 11)) %>%
	filter(to <= from) %>%
	mutate(rate = to / from,
				 from = case_when(
				 	from == 3369 ~ "Social media",
				 	from == 55 ~ "Enter game",
				 	from == 21 ~ "Play one round",
				 	from == 11 ~ "Play > 1 rounds"),
				 to = case_when(
				 	to == 3369 ~ "Social media",
				 	to == 55 ~ "Enter game",
				 	to == 21 ~ "Play one round",
				 	to == 11 ~ "Play > 1 rounds"),
				 to = as.factor(to),
				 from = as.factor(from),
				 to = fct_reorder(to, -rate),
				 from = fct_reorder(from, -rate)) %>%
	ggplot(aes(x = to,
						 y = from,
						 fill = rate)) +
	geom_raster() +
	geom_text(aes(label = ifelse(rate == 1, "", round(rate * 100, 2))), color = "white") +
	coord_equal() +
	scale_x_discrete(position = "top") +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	scale_fill_viridis_c(labels = scales::percent, option = "mako") +
	labs(x = "To",
			 y = "Survival from",
			 fill = "Survival\nrate")


# X ----------------------------------------------------------------------------
cum_t <- raw_game_data %>%
  select(session_id, data) %>%
  mutate(date = str_extract(session_id, "[:alnum:]{3}_[:alnum:]{3}_[:digit:]+_"),
  			 yr = str_extract(session_id, "[:digit:]{4}")) %>%
  mutate(date = paste0(date, yr)) %>%
  mutate(date = str_replace_all(date, "_", " ")) %>%
  mutate(date = str_remove(date, "[:alpha:]{3}")) %>%
  mutate(date = str_trim(date)) %>%
  mutate(date = lubridate::mdy(date)) %>%
  group_by(date) %>%
  summarize(n = n(),
            n1 = get_n(data = data, n = 1),
            n2 = get_n(data = data, n = 2)) %>%
  ungroup() %>%
  bind_rows(tibble(date = ymd("2023-11-15"),
                   n = 0,
                   n1 = 0,
                   n2 = 0)) %>%
  arrange(date) %>%
  mutate(n = cumsum(n),
         n1 = cumsum(n1),
         n2 = cumsum(n2)) %>%
	pivot_longer(cols = contains("n"), names_to = "val", values_to = "n") %>%
	mutate(val = case_when(val == "n" ~ "Entering",
												 val == "n1" ~ "Playing baseline",
												 val == "n2" ~ "Playing baseline and treatment")) %>%
	ggplot(aes(x = date, y = n, color = val)) +
	geom_vline(xintercept = ymd("2023-11-15"), linetype = "dashed", linewidth = 0.1) +
	geom_vline(xintercept = ymd("2023-11-29"), linetype = "dashed", linewidth = 0.1) +
	geom_vline(xintercept = ymd("2023-12-04"), linetype = "dashed", linewidth = 0.1) +
	geom_vline(xintercept = ymd("2023-12-20"), linetype = "dashed", linewidth = 0.1) +
	geom_step(linewidth = 1.5) +
	scale_color_brewer(palette = "Set1") +
	theme(legend.position = "inside",
				legend.position.inside = c(0, 1),
				legend.justification.inside = c(0, 1)) +
	labs(x = "Date",
       y = "Cumulative number of responses",
       color = "Measure")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
plot <- cowplot::plot_grid(suv_mat,
													 cum_t,
													 ncol = 1,
													 # rel_heights = c(3, 1),
													 # align = "hv",
													 labels = "auto")


startR::lazy_ggsave(plot = plot,
										filename = "fig3_survival",
										width = 12,
										height = 18)

