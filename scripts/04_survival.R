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

# Load data --------------------------------------------------------------------
raw_game_data <- readRDS(file = here("data", "raw", "raw_game_data.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
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

expand_grid(from = c(3633, 95, 55, 21, 11),
						to = c(3633, 95, 55, 21, 11)) %>%
	filter(to < from) %>%
	mutate(rate = to / from,
				 from = as.factor(from),
				 to = as.factor(to),
				 to = fct_reorder(to, -as.numeric(to))) %>%
	ggplot(aes(x = factor(to),
						 y = factor(from),
						 fill = rate)) +
	geom_raster() +
	theme_minimal() +
	coord_equal() +
	scale_x_discrete(position = "top") +
	scale_fill_viridis_c(labels = scales::percent) +
	labs(x = "To",
			 y = "From",
			 fill = "Survival\nrate")


# X ----------------------------------------------------------------------------
raw_game_data %>%
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
  ggplot(aes(x = date, y = n, color = val)) +
	geom_vline(xintercept = ymd("2023-11-15")) +
	geom_vline(xintercept = ymd("2023-11-24")) +
	geom_vline(xintercept = ymd("2023-12-05")) +
	geom_vline(xintercept = ymd("2023-12-21")) +
	# geom_vline(xintercept = ymd("2024-01-04")) +
  geom_step() +
  labs(x = "Date",
       y = "Number of responses",
       color = "Measure") +
  theme_minimal()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
