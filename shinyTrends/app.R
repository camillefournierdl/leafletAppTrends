# app.R — Air Pollution Trajectories Explorer
# Run with: shiny::runApp("shiny_app")

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)

# ---- Load pre-processed data ----
countries  <- readRDS("data/countries.rds")
cities     <- readRDS("data/cities.rds")
timeseries <- readRDS("data/timeseries.rds")

# ---- Constants ----

# Mapping from internal subtypesLQ values to display labels
trend_display <- c(
  "Positive Linear"                    = "Steady Increase",
  "Inverted U-Shaped Positive Quad"    = "Increase and Plateau",
  "Inverted U-Shaped Shifting"         = "Increase and Decline",
  "Inverted U-Shaped Negative Quad"    = "Delayed Decline",
  "Negative Linear"                    = "Steady Decline",
  "notrend"                            = "No Trend"
)

# Internal values (for filtering)
trend_levels <- names(trend_display)
trend_labels <- unname(trend_display)

# Country-level categories (numbered prefix) → display labels
country_display <- c(
  "1. Positive Linear"                    = "Steady Increase",
  "2. Inverted U-Shaped Positive Quad"    = "Increase and Plateau",
  "3. Inverted U-Shaped Shifting"         = "Increase and Decline",
  "4. Inverted U-Shaped Negative Quad"    = "Delayed Decline",
  "5. Negative Linear"                    = "Steady Decline",
  "No Trend"                              = "No Trend"
)

# Color palette keyed by internal subtypesLQ values
trend_colors <- c(
  "Positive Linear"                    = "#593572",
  "Inverted U-Shaped Positive Quad"    = "#baa1ce",
  "Inverted U-Shaped Shifting"         = "#f1b904",
  "Inverted U-Shaped Negative Quad"    = "#7EAD31",
  "Negative Linear"                    = "#3E5519",
  "notrend"                            = "#808080"
)

# Same colors keyed for country-level labels
country_trend_colors <- c(
  "1. Positive Linear"                    = "#593572",
  "2. Inverted U-Shaped Positive Quad"    = "#baa1ce",
  "3. Inverted U-Shaped Shifting"         = "#f1b904",
  "4. Inverted U-Shaped Negative Quad"    = "#7EAD31",
  "5. Negative Linear"                    = "#3E5519",
  "No Trend"                              = "#808080"
)

income_levels <- c(
  "1. High Income",
  "2. Upper middle income",
  "3. Lower middle income",
  "4. Low income"
)

# Pollution level ranges (µg/m³)
pollution_ranges <- c(
  "< 10"    = "0-10",
  "10 - 25" = "10-25",
  "25 - 50" = "25-50",
  "50 - 75" = "50-75",
  "> 75"    = "75-Inf"
)

# Helper: check if a pollution value falls in a selected range
in_pollution_range <- function(val, selected_ranges) {
  bounds <- do.call(rbind, lapply(selected_ranges, function(r) {
    parts <- as.numeric(strsplit(r, "-")[[1]])
    data.frame(lo = parts[1], hi = parts[2])
  }))
  sapply(val, function(v) {
    if (is.na(v)) return(FALSE)
    any(v >= bounds$lo & v < bounds$hi)
  })
}

# ---- Helpers ----

# Map city subtypesLQ to a color
city_color <- function(subtype) {
  ifelse(is.na(subtype), "#808080", unname(trend_colors[subtype]))
}

# Marker radius scaled by log population
marker_radius <- function(pop) {
  r <- log10(pmax(pop, 1, na.rm = TRUE))
  rmin <- min(r, na.rm = TRUE)
  rmax <- max(r, na.rm = TRUE)
  3 + 7 * (r - rmin) / max(rmax - rmin, 1)
}

# Continuous pollution color palette with log scale for more variation at low levels.
# We build an internal palette on log-transformed domain and wrap it so callers
# can pass raw µg/m³ values.
.pollution_pal_internal <- colorNumeric(
  palette = c("#FFFFEE", "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"),
  domain = c(log1p(0), log1p(120)),
  na.color = "#808080"
)
pollution_pal <- function(x) .pollution_pal_internal(log1p(x))

# ---- UI ----
ui <- page_sidebar(
  title = "Air Pollution Trajectories Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,
    radioButtons(
      "marker_color_mode", "Outcome of Interest",
      choices = c(
        "Trajectory category"          = "trend",
        "Mean pollution level"    = "pollution"
      ),
      selected = "trend"
    ),
    hr(),
    h5("Filters"),
    checkboxGroupInput(
      "trend_filter", "Trajectory Category",
      choices  = setNames(trend_levels, trend_labels),
      selected = trend_levels
    ),
    hr(),
    checkboxGroupInput(
      "income_filter", "Income Group",
      choices  = income_levels,
      selected = income_levels
    ),
    hr(),
    checkboxGroupInput(
      "pollution_filter", "Mean Pollution Level (\u00b5g/m\u00b3)",
      choices  = pollution_ranges,
      selected = unname(pollution_ranges)
    ),
    hr(),
    radioButtons(
      "country_avg_mode", "Country Average",
      choices = c(
        "Simple average"              = "simple",
        "Population-weighted average" = "popweighted"
      ),
      selected = "simple"
    ),
  ),

  tags$p(
    "Explore PM2.5 pollution levels and trajectories across 13,000+ urban centers. Original data from Van Donkelaar et al.", tags$a("https://www.satpm.org/", href = "https://www.satpm.org/", target = "_blank"),".", 
    tags$br(),
    "Trajectory classification based on work from Camille Fournier de Lauriere.",
    tags$a("Paper", href = "https://doi.org/XXXX", target = "_blank"), "|",
    tags$a("Code", href = "https://github.com/camillefournierdl/leafletAppTrends", target = "_blank"), "|",
    tags$a("cfournier@ethz.ch", href = "mailto::cfournier@ethz.ch", target = "_blank"),
    tags$br(),
    "Use the left side panel to select between raw pollution levels and trajectories. Filter cities by trajectory type, income group, and pollution level. Click on city markers to see their pollution trajectory over time in the panel below.",
    class = "text-muted", style = "margin: 0.2rem 0 0.5rem 0; font-size: 0.9rem;"
  ),
  
  tags$style(HTML("
    .vlabel-wrap { display: flex; align-items: stretch; }
    .vlabel {
      writing-mode: vertical-rl;
      transform: rotate(180deg);
      text-align: center;
      font-weight: 600;
      font-size: 0.85rem;
      color: #6c757d;
      padding: 0.3rem;
      white-space: nowrap;
    }
    .vlabel-content { flex: 1; min-width: 0; }
  ")),
  
  div(class = "vlabel-wrap",
      div(class = "vlabel", "Map"),
      div(class = "vlabel-content",
          card(
            full_screen = TRUE,
            leafletOutput("map", height = "500px")
          )
      )
  ),
  div(class = "vlabel-wrap",
      div(class = "vlabel", textOutput("plot_title", inline = TRUE)),
      div(class = "vlabel-content",
          card(
            conditionalPanel(
              condition = "output.city_selected",
              plotlyOutput("trend_plot", height = "350px")
            ),
            conditionalPanel(
              condition = "!output.city_selected",
              p("Click a city marker on the map to see its pollution trajectory.",
                class = "text-muted", style = "padding: 2rem; text-align: center;")
            )
          )
      )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # -- Pre-compute income lookup once --
  income_lookup <- st_drop_geometry(countries) %>%
    select(iso_a3, Income.group) %>%
    filter(!is.na(Income.group))

  # -- Reactive: filtered cities --
  filtered_cities <- reactive({
    fc <- cities %>%
      filter(subtypesLQ %in% input$trend_filter) %>%
      filter(in_pollution_range(mean_pollution, input$pollution_filter))

    fc <- fc %>%
      left_join(income_lookup, by = c("CTR_MN_ISO" = "iso_a3")) %>%
      filter(Income.group %in% input$income_filter)

    fc
  })

  # -- Track selected city --
  selected_city <- reactiveVal(NULL)

  # Flag for conditional panel
  output$city_selected <- reactive({ !is.null(selected_city()) })
  outputOptions(output, "city_selected", suspendWhenHidden = FALSE)

  # -- Base map (rendered once) --
  output$map <- renderLeaflet({
    # Country choropleth color function
    country_pal <- colorFactor(
      palette = unname(country_trend_colors),
      levels  = names(country_trend_colors),
      na.color = "transparent"
    )

    # Country tooltip: use display labels
    country_labels <- sapply(countries$weighted_mode_value, function(v) {
      if (is.na(v)) "No data" else unname(country_display[v])
    })

    leaflet(options = leafletOptions(
      minZoom = 2,
      maxBounds = list(c(-90, -200), c(90, 200)),
      maxBoundsViscosity = 1.0
    )) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 20, lat = 20, zoom = 2) %>%
      addPolygons(
        data = countries,
        fillColor   = ~country_pal(weighted_mode_value),
        fillOpacity = 0.4,
        color       = "white",
        weight      = 0.5,
        label       = ~paste0(name, ": ", country_labels),
        group       = "Countries"
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = unname(trend_colors),
        labels   = trend_labels,
        title    = "Trajectory Category",
        opacity  = 0.8,
        layerId  = "legend_trend"
      )
  })

  # -- Update city markers, country polygons, and legend reactively --
  observe({
    fc <- filtered_cities()
    color_mode <- input$marker_color_mode

    proxy <- leafletProxy("map") %>%
      clearGroup("CityMarkers") %>%
      clearGroup("Countries") %>%
      removeControl("legend_trend") %>%
      removeControl("legend_pollution")

    # --- Redraw country polygons based on color mode ---
    if (color_mode == "trend") {
      trend_col <- if (input$country_avg_mode == "popweighted") "weighted_mode_value" else "mode_value"
      trend_vals <- countries[[trend_col]]
      country_pal <- colorFactor(
        palette  = unname(country_trend_colors),
        levels   = names(country_trend_colors),
        na.color = "transparent"
      )
      country_fill <- country_pal(trend_vals)
      country_tip <- paste0(
        countries$name, ": ",
        sapply(trend_vals, function(v) {
          if (is.na(v)) "No data" else unname(country_display[v])
        })
      )
    } else {
      poll_col <- if (input$country_avg_mode == "popweighted") "mean_pollution_popw" else "mean_pollution"
      poll_vals <- countries[[poll_col]]
      country_fill <- pollution_pal(poll_vals)
      country_tip <- paste0(
        countries$name, ": ",
        ifelse(is.na(poll_vals), "No data",
               paste0(round(poll_vals, 1), " \u00b5g/m\u00b3"))
      )
    }

    proxy <- proxy %>%
      addPolygons(
        data        = countries,
        fillColor   = country_fill,
        fillOpacity = 0.4,
        color       = "white",
        weight      = 0.5,
        label       = country_tip,
        group       = "Countries"
      )

    # --- City markers ---
    if (nrow(fc) > 0) {
      coords <- st_coordinates(fc)

      display_trend <- trend_display[fc$subtypesLQ]

      popup_text <- paste0(
        "<strong>", fc$UC_NM_MN, "</strong> (", fc$CTR_MN_NM, ")<br/>",
        "Trend: ", display_trend, "<br/>",
        "Mean PM2.5: ", round(fc$mean_pollution, 1), " \u00b5g/m\u00b3"
      )

      if (color_mode == "trend") {
        fill_col <- city_color(fc$subtypesLQ)
      } else {
        fill_col <- pollution_pal(fc$mean_pollution)
      }

      proxy <- proxy %>%
        addCircleMarkers(
          lng         = coords[, 1],
          lat         = coords[, 2],
          radius      = marker_radius(fc$mean_population),
          color       = fill_col,
          fillColor   = fill_col,
          fillOpacity = 0.7,
          weight      = 1,
          popup       = popup_text,
          layerId     = fc$ID,
          group       = "CityMarkers"
        )
    }

    # --- Legend ---
    if (color_mode == "trend") {
      proxy %>%
        addLegend(
          position = "bottomright",
          colors   = unname(trend_colors),
          labels   = trend_labels,
          title    = "Trajectory Category",
          opacity  = 0.8,
          layerId  = "legend_trend"
        )
    } else {
      # Log-spaced legend breaks for the custom palette
      leg_breaks <- c(2, 5, 10, 20, 40, 80, 120)
      proxy %>%
        addLegend(
          position = "bottomright",
          colors   = pollution_pal(leg_breaks),
          labels   = paste0(leg_breaks, " \u00b5g/m\u00b3"),
          title    = "Mean PM2.5",
          opacity  = 0.8,
          layerId  = "legend_pollution"
        )
    }
  })

  # -- Capture marker click --
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      selected_city(click$id)
    }
  })

  # -- Plot title --
  output$plot_title <- renderText({
    city_id <- selected_city()
    if (is.null(city_id)) return("City Trajectory")

    info <- cities %>% filter(ID == city_id)
    if (nrow(info) == 0) return("City Trajectory")

    paste0(info$UC_NM_MN, ", ", info$CTR_MN_NM)
  })

  # -- Trend plot --
  output$trend_plot <- renderPlotly({
    city_id <- selected_city()
    req(city_id)

    info <- cities %>% filter(ID == city_id)
    ts_data <- timeseries %>% filter(ID == city_id, !is.na(year))

    ts_data <- ts_data %>% filter(!is.na(avg_pollution))
    y_var <- "avg_pollution"
    y_lab <- "PM2.5 (\u00b5g/m\u00b3)"

    # Use display label for subtitle
    subtitle <- if (nrow(info) > 0) unname(trend_display[info$subtypesLQ[1]]) else ""

    p <- ggplot(ts_data, aes(x = year, y = .data[[y_var]])) +
      geom_point(size = 2, color = "#2c3e50") +
      geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
      labs(
        x        = "Year",
        y        = y_lab,
        subtitle = subtitle
      ) +
      theme_minimal(base_size = 13)

    ggplotly(p) %>%
      layout(
        margin = list(t = 40),
        title = list(
          text = paste0(info$UC_NM_MN, ", ", info$CTR_MN_NM,
                        "<br><sup>", subtitle, "</sup>"),
          font = list(size = 14)
        )
      )
  })
}

shinyApp(ui, server)
