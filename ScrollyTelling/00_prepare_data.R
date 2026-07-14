# ------------------------------------------------------------------------
# 00_prepare_data.R
#
# Cleans the UN Women SDG indicator export (SG_GEN_LOCGELS: proportion of
# elected seats held by women in deliberative bodies of local government)
# and joins it to country polygons so it can be mapped.
#
# ------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(countrycode)
library(sf)
library(rnaturalearth)

# ---- 1. Read the raw export -------------------------------------------

raw <- read_csv(
  "../Data/UN_Women_Data_Hub_map-export.csv",
  col_types = cols(.default = "c")
)

# ---- 2. Clean the value column -----------------------------------------
# OBS_VALUE is "N" for countries with no Value

clean <- raw %>%
  mutate(
    obs_value_num = suppressWarnings(as.numeric(OBS_VALUE)),
    status = case_when(
      OBS_VALUE == "N" ~ "not_applicable",
      !is.na(obs_value_num) ~ "reported",
      TRUE ~ "no_data"
    ),
    time_period = as.integer(TIME_PERIOD)
  ) %>%
  select(
    ref_area_code = `REF_AREA Code`,
    country_name  = `REF_AREA Description`,
    year          = time_period,
    value         = obs_value_num,
    status
  )

# ---- 3. Match to ISO3 codes ---------------------------------------------
# REF_AREA Code holds UN M49 numeric codes. countrycode's "un" origin
# understands these directly, which is more reliable than name matching.

clean <- clean %>%
  mutate(
    ref_area_code = as.numeric(ref_area_code),
    iso3 = countrycode(
      ref_area_code,
      origin      = "un",
      destination = "iso3c",
      warn        = FALSE
    )
  )



# ---- 4. Join to country polygons -----------------------------------------
# Natural Earth's "medium" scale 
# NB: Natural Earth's plain iso_a3
# field is "-99" (unresolved) for France and Norway 
# USE iso_a3_eh ("expanded harmonized") is Natural Earth's  fix for this


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso3 = iso_a3_eh, admin, continent, region_un, subregion, geometry)

map_data <- world %>%
  left_join(clean, by = "iso3")


# ---- 5. Define the Pacific subset ----------------------------------------
# Pacific Island Countries and Territories, using the SPC/PIC convention.
# Listed explicitly (rather than derived from a region field) so it's easy
# to check and adjust which entities count as "Pacific" for the story.

pacific_iso3 <- c(
  "FJI", # Fiji
  "KIR", # Kiribati
  "MHL", # Marshall Islands
  "FSM", # Micronesia (Federated States of)
  "NRU", # Nauru
  "PLW", # Palau
  "PNG", # Papua New Guinea
  "WSM", # Samoa
  "SLB", # Solomon Islands
  "TON", # Tonga
  "TUV", # Tuvalu
  "VUT", # Vanuatu
  "COK", # Cook Islands
  "NIU", # Niue
  "TKL", # Tokelau
  "ASM", # American Samoa
  "GUM", # Guam
  "MNP", # Northern Mariana Islands
  "NCL", # New Caledonia
  "PYF", # French Polynesia
  "WLF"  # Wallis and Futuna
)

map_data <- map_data %>%
  mutate(is_pacific = iso3 %in% pacific_iso3)

# ---- 6. Save cleaned data for index.qmd -----------------------------------
#
# NOTE ON THE ANTIMERIDIAN: an earlier version of this script tried to
# recenter the whole world map on the Pacific (shifting longitudes to a
# 0-360 range) so the Pacific wouldn't be split at the map's edges. That
# introduced more problems than it solved (stray polygon artifacts from
# countries like Russia and Fiji that already cross the antimeridian in
# the source data). It's also unnecessary for this story: every country
# we zoom into and label sits between about 140 degrees E and 180 degrees
# E — comfortably on one side of the antimeridian — so a plain, standard
# -180/180 world map works fine. index.qmd zooms into that region with a
# CSS pan/scale effect on the ordinary map image, not by re-projecting the
# data. The only casualty is Kiribati, whose Line Islands sit far to the
# east (around -150) separate from its main Gilbert Islands group (around
# 173 E, where its capital and presumably its local government data are)
# — index.qmd places Kiribati's label manually near the Gilbert Islands
# rather than at its computed centroid, for exactly this reason.

dir.create("data", showWarnings = FALSE)
saveRDS(
  list(
    map_data     = map_data,
    pacific_iso3 = pacific_iso3
  ),
  "data/local_gov_women_clean.rds"
)

message("Done. Saved data/local_gov_women_clean.rds")
