# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)
library(tigris)
library(sf)

# Load data ---------------------------------------------------------------
df <- read.csv(
    "dunkin_donuts_chain.csv"
    )

df <- df |>
  mutate(
    loc_LONG_centroid = gsub(",", ".", loc_LONG_centroid),
    loc_LONG_centroid = as.numeric(loc_LONG_centroid),
    loc_LAT_centroid  = gsub(",", ".", loc_LAT_centroid),
    loc_LAT_centroid  = as.numeric(loc_LAT_centroid)
  ) |>
  select(biz_name, e_city, e_state, loc_county, loc_LAT_centroid, loc_LONG_centroid)

view(df)

# Load fonts --------------------------------------------------------------

font_add_google("Nunito")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#fffcf6ff"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "Nunito"

# Load map ---------------------------------------------------------------
# 1. Dunkin points -------------------------------------------------------

df_sf <- st_as_sf(
  df,
  coords = c("loc_LONG_centroid", "loc_LAT_centroid"),  # x = lon, y = lat
  crs = 4326
)

# 2. US states (shifted) -------------------------------------------------

us_states <- states(cb = TRUE, resolution = "20m") |>
  shift_geometry() |>
  st_transform(2163)

df_sf_proj <- st_transform(df_sf, 2163)


# 3. Grid from the US boundary after shifting

grid <- st_make_grid(us_states, cellsize = 16093, square = TRUE)
grid <- st_sf(grid_id = 1:length(grid), geometry = grid)

# Data wrangling ----------------------------------------------------------

df_grid <- st_join(df_sf_proj, grid, join = st_within)

grid_summary <- df_grid |>
  group_by(grid_id) |>
  summarise(store_count = n(), .groups = "drop") |>
  mutate(category = if_else(store_count <= 5, "Less than 5", "Greater than 5"))

grid_points <- st_centroid(grid_summary)

# Graph data -------------------------------------------------------------

plt <- ggplot() +
        geom_sf(data = us_states, fill = "#fff3ddff", color = "#f08d01ff") +
        geom_sf(data = grid_points, aes(color = category), size = 1.5, stroke = 0) +
        scale_color_manual(values = c(
            "Less than 5"    = alpha("#ffd900ff", 0.5),
            "Greater than 5" = alpha("#ff9500ff", 0.5))
        ) +
        coord_sf(
            xlim = st_bbox(us_states)[c("xmin", "xmax")] * c(1.05, 1.05),
            ylim = st_bbox(us_states)[c("ymin", "ymax")] * c(1.05, 1.05),
            expand = FALSE
        ) +
        labs(
            title    = "Where Can You Find Your Nearest Dunkin'?",
            subtitle = "Each point represents a 10-mile area with at least one Dunkin' store.",
            caption  = "Source: Coffee Chains (TidytuesdayR) · Graphic: Bayu Prahara",
            color    = "Category :"
        ) +
        guides(
            color = guide_legend(override.aes = list(size = 6))
        ) +
        theme_minimal(base_family = body_font) +
        theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 28,
            color = "#f08d01ff",
            width = unit(1, "npc"),
            padding = margin(0, 0, 5, 0),
            margin = margin(10, 0, 20, 0)
        ),
        plot.subtitle = element_textbox_simple(
            family = body_font,
            size = 14,
            lineheight = 1.4,
            padding = margin(0, 0, 0, 0),
            margin = margin(-20, 0, 80, 0)
        ),
        plot.caption = element_text(
            hjust = 1, 
            size = 12, 
            color = "grey40", 
            margin = margin(t = 40, b = 20)
        ),
        # Legend
        legend.position = c(0.5, 1),           
        legend.justification = c(0.5, 1),
        legend.box.just = "left",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.margin = margin(t = -20, b = 20, l = 0, r = 0),
        legend.box.margin = margin(b = 20),
        legend.spacing.x = unit(0.5, "cm"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        # Axis
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        # Plot
        plot.margin = margin(80, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
        )
