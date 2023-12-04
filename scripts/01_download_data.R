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
	googledrive,
	googlesheets4,
	here,
	tidyverse
)

# Set up google drive authrneitcation ------------------------------------------
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# Download data ----------------------------------------------------------------
# Identify all google sheets that match the naming convention
files <- googledrive::drive_find(corpus = "user",
                                 type = "spreadsheet") %>%
  filter(str_detect(name, pattern = "[:alnum:]{3}_[:alnum:]{3}"),
         map_lgl(drive_resource, ~.x$createdTime > "2023-11-15T00:00:00.000Z")) %>%
  rename(session_id = name)

# Build a safely function, in case it fails
safely_read <- safely(read_sheet)

# Map through them and actually perform the download
local <- files %>%
  mutate(data = map(id, safely_read, range = "A:H"))

## EXPORT ######################################################################
saveRDS(object = local,
        file = here("data", "raw", "raw_game_data.rds"))

## PROCESSING ##################################################################





