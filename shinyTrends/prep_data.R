# prep_data.R
# One-time script: reads raw project data, outputs RDS files for the Shiny app.
# Run from a different project root (trendsAirPollutionAQM/):
#   Rscript shiny_app/prep_data.R

library(sf)
library(dplyr)
library(rnaturalearth)
library(rmapshaper)

# ---- Paths (relative to project root) ----
classif_country_path  <- "output/classifByCountry.csv"
classif_city_path     <- "output/classificationPollution_PosNeg.csv"
signif_trends_path    <- "output/signifTrends.csv"
gpkg_path             <- "data/UC_fixed_geom.gpkg"
summary_city_path     <- "data/summaryCityUC.csv"
yearly_ts_path        <- "data/yearlyCityTS.csv"
out_dir               <- "shinyTrends/data"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ==========================================================================
# 1a. Countries layer
# ==========================================================================
cat("Building countries layer...\n")

world <- ne_countries(scale = "large", returnclass = "sf")

country_classif <- read.csv(classif_country_path, stringsAsFactors = FALSE)

# Fix ISO codes: Natural Earth sets iso_a3 = "-99" for France, Norway, etc.
# Use iso_a3_eh (which has correct codes) as fallback.
world$iso_a3 <- ifelse(world$iso_a3 == "-99", world$iso_a3_eh, world$iso_a3)
# Fix Somaliland ISO code to match
world$iso_a3 <- ifelse(world$sovereignt == "Somaliland", "SOM", world$iso_a3)

countries <- world %>%
  select(name, iso_a3) %>%
  left_join(
    country_classif %>% select(CTR_MN_ISO, mode_value, weighted_mode_value, Income.group, entropy),
    by = c("iso_a3" = "CTR_MN_ISO")
  )

# Simplify geometry for fast Leaflet rendering
countries <- ms_simplify(countries, keep = 0.02, keep_shapes = TRUE)

# Country-level mean pollution is computed after building cities (see below)
# and joined back before saving.

# ==========================================================================
# 1b. Cities layer (centroids)
# ==========================================================================
cat("Building cities layer...\n")

uc <- st_read(gpkg_path, quiet = TRUE)

# Compute centroids (sf geometry column is carried automatically)
uc_centroids <- st_centroid(uc, of_largest_polygon = TRUE) %>%
  select(ID = ID_HDC_G0, UC_NM_MN, CTR_MN_NM, CTR_MN_ISO)

# Join classification
classif <- read.csv(classif_city_path, stringsAsFactors = FALSE) %>%
  select(ID, subtypesLQ, bestfit, bestfitLQ)

# Join significant trends (improvement detection)
signif <- read.csv(signif_trends_path, stringsAsFactors = FALSE) %>%
  select(ID, sig_downward, rounded_peak)

# Join summary stats
summary_city <- read.csv(summary_city_path, stringsAsFactors = FALSE) %>%
  select(ID, mean_pollution, mean_population)

cities <- uc_centroids %>%
  left_join(classif, by = "ID") %>%
  left_join(signif, by = "ID") %>%
  left_join(summary_city, by = "ID")

saveRDS(cities, file.path(out_dir, "cities.rds"))
cat("  -> cities.rds saved:", nrow(cities), "features\n")

# ==========================================================================
# 1a-bis. Add country-level mean pollution (average of city means per country)
# ==========================================================================
cat("Adding country-level mean pollution...\n")

city_df <- st_drop_geometry(cities) %>%
  select(CTR_MN_ISO, mean_pollution, mean_population) %>%
  filter(!is.na(mean_pollution), !is.na(mean_population))

country_pollution <- city_df %>%
  group_by(CTR_MN_ISO) %>%
  summarise(
    mean_pollution      = mean(mean_pollution),
    mean_pollution_popw = sum(mean_pollution * mean_population) / sum(mean_population),
    .groups = "drop"
  )

countries <- countries %>%
  left_join(country_pollution, by = c("iso_a3" = "CTR_MN_ISO"))

saveRDS(countries, file.path(out_dir, "countries.rds"))
cat("  -> countries.rds saved:", nrow(countries), "features\n")

# ==========================================================================
# 1c. Time series
# ==========================================================================
cat("Building time series...\n")

ts <- read.csv(yearly_ts_path, stringsAsFactors = FALSE) %>%
  select(ID, year, avg_pollution, z_score_pollution)

saveRDS(ts, file.path(out_dir, "timeseries.rds"))
cat("  -> timeseries.rds saved:", nrow(ts), "rows\n")

cat("Done! All RDS files written to", out_dir, "\n")
