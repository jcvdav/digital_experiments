################################################################################
# Download raw game data from Google Drive
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
#
# This script downloads game data from Google Drive spreadsheets. It identifies
# all sheets matching the naming convention (three letters, underscore, three
# letters, underscore, digits), filters by creation date, and downloads the
# data to a local RDS file for further processing.
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
  filter(str_detect(name, pattern = "[:alpha:]{3}_[:alpha:]{3}_[:digit:]{1,2}"),
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





