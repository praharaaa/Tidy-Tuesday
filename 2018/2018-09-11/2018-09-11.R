# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(sf)
library(tigris)

# Load fonts --------------------------------------------------------------

font_add_google("Rowdies")
font_add_google("Scada")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#fff9f9ff"
text_col    <- "#181D25"
body_font   <- "Scada"
title_font  <- "Rowdies"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-09-11")
df <- tt_data$cats_vs_dogs

us_states_sf <- states(cb = TRUE, resolution = "20m") |>
  filter(!NAME %in% c("Alaska", "Hawaii"))  # Exclude AK & HI
us_states_sf_shifted <- shift_geometry(us_states_sf)


# Data wrangling ----------------------------------------------------------

df <- df |> select(state, percent_dog_owners, percent_cat_owners) |> 
    mutate(NAME = state, max_cause = case_when(
          percent_dog_owners > percent_cat_owners ~ "Dog owners dominated", 
          percent_dog_owners < percent_cat_owners ~ "Cat owners dominated", 
          percent_dog_owners == percent_cat_owners ~ "Tie"),
          max_count = pmax(percent_dog_owners, percent_cat_owners))

ecdf_dog <- ecdf(df$percent_dog_owners)
ecdf_cat <- ecdf(df$percent_cat_owners)

df <- df |>
  mutate(
    percentile = case_when(
      max_cause == "Dog owners dominated" ~ ecdf_dog(percent_dog_owners),
      max_cause == "Cat owners dominated" ~ ecdf_cat(percent_cat_owners),
      max_cause == "Tie" ~ 0,
      TRUE ~ NA_real_
    )
  )

ramp_dog <- colorRamp(c("white", "#E60000"))
ramp_cat <- colorRamp(c("white", "#56B4E9"))
ramp_tie <- colorRamp(c("white"))

df_clean <- df |>
  rowwise() |>
  mutate(
    tile_color = case_when(
      max_cause == "Tie" ~ {
        rgb_vals <- ramp_tie(percentile)
        rgb(rgb_vals[1,1], rgb_vals[1,2], rgb_vals[1,3], maxColorValue = 255)
      },
      max_cause == "Dog owners dominated" ~ {
        rgb_vals <- ramp_dog(percentile)
        rgb(rgb_vals[1,1], rgb_vals[1,2], rgb_vals[1,3], maxColorValue = 255)
      },
      max_cause == "Cat owners dominated" ~ {
        rgb_vals <- ramp_cat(percentile)
        rgb(rgb_vals[1,1], rgb_vals[1,2], rgb_vals[1,3], maxColorValue = 255)
      },
      TRUE ~ NA_character_
    )
  ) |>
  ungroup()

map_data_joined <- left_join(us_states_sf_shifted, df_clean, by = "NAME")

# Graph data --------------------------------------------------------------

p <- ggplot(map_data_joined) +
    geom_sf(aes(fill = tile_color), color = "white", size = 0.5) +
    scale_fill_identity() +
    geom_point(
        data = data.frame(
        x = c(-Inf, -Inf),
        y = c(-Inf, -Inf),
        category = c("Dog owners dominated", "Cat owners dominated")
        ),
        aes(x = x, y = y, color = category),
        alpha = 0  # Invisible, cuma buat legend
    ) +
    scale_color_manual(
        name = NULL,
        values = c("Dog owners dominated" = "#E60000", 
                "Cat owners dominated" = "#56B4E9"),
        guide = guide_legend(override.aes = list(alpha = 1, size = 8, shape = 15))
    ) +
    labs(
        title = "Cat vs Dog Ownership Across US States",
        subtitle = "Based on American Veterinary Medical Association data, states are colored by pet ownership dominance. Darker shades indicate higher percentile within each category.",
        caption = "Dataset: Cats vs Dogs USA (TidyTuesdayR) · Graphic: Bayu Prahara"
    ) +
    theme_void(base_family = body_font) +
    theme(
    # Title, subtitle, and caption
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
        family = title_font, 
        face = "bold", 
        size = 28,
        color = "#d32f2f",
        width = unit(1, "npc"),
        padding = margin(0, 0, 5, 0),
        margin = margin(2, 0, 2, 0)
    ),
    plot.subtitle = element_textbox_simple(
        family = body_font,
        size = 14,
        lineheight = 1.4,
        padding = margin(0, 0, 0, 0),
        margin = margin(5, 0, 20, 0)
    ),
    plot.caption = element_text(
        hjust = 1, 
        size = 12, 
        color = "grey40", 
        margin = margin(t = 20)
    ),
    legend.position = "top", 
    legend.margin = margin(20, 10, 0, 10),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.title = element_text(family = body_font, face = "bold", color = "grey20"),
    legend.text = element_text(family = body_font, size = 14),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) 
print(p)