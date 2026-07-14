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

dir.create("data", showWarnings = FALSE)
saveRDS(
  list(
    map_data     = map_data,
    pacific_iso3 = pacific_iso3
  ),
  "data/local_gov_women_clean.rds"
)

message("Done. Saved data/local_gov_women_clean.rds")
