# Air Pollution Trajectories Explorer

Interactive Shiny app for exploring long-term PM2.5 pollution trajectories across 13,000+ urban centers worldwide. Companion to the paper *[paper title/citation]*.

## Features

- **Interactive map** with city-level markers and country-level choropleth
- **Color by trend category or mean pollution level** for both city markers and country polygons
- **Filter cities** by trend category, World Bank income group, and mean pollution level
- **Simple vs. population-weighted country averages** — toggle how country-level summaries are computed (applies to both trend mode and pollution mode)
- **Click any city** to view its PM2.5 time series with a loess smooth

## Trend categories

| Label | Description |
|---|---|
| Steady Increase | Pollution rising throughout the period |
| Increase and Plateau | Pollution rising then leveling off |
| Increase and Decline | Classic inverted U-shape |
| Delayed Decline | Pollution declining, late onset |
| Steady Decline | Pollution falling throughout the period |
| No Trend | No significant trend detected |

## Running locally

### Prerequisites

R (>= 4.1) with the following packages:

```r
install.packages(c("shiny", "bslib", "leaflet", "sf", "dplyr", "ggplot2", "plotly"))
```

### Data preparation

The app reads pre-processed `.rds` files from `data/`. To regenerate them from the project's raw data, run need to run the prep_data file, with data from a different repository: 
*[https://github.com/camillefournierdl/trendsAirPollutionAQM]*.

from the main root folder (`trendsAirPollutionAQM/`)

```r
source("shinyTrends/prep_data.R")
```

This requires additional packages (`rnaturalearth`, `rmapshaper`), and data from *[https://github.com/camillefournierdl/trendsAirPollutionAQM]*.

The app is hosted at *https://cfournierdel.shinyapps.io/shinyTrends/*. 

## File structure

```
shinyTrends/
├── app.R            # Shiny application (UI + server)
├── prep_data.R      # One-time data preparation script (relying on data from a different repo! See Data preparation section)
├── data/
│   ├── cities.rds       # City centroids with trend classifications
│   ├── countries.rds    # Country polygons with aggregated statistics
│   └── timeseries.rds   # Yearly PM2.5 time series per city
└── www/                 # Static assets (CSS, images)
```
