# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Bungee")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#fff9f9ff"
text_col    <- "#181D25"
body_font   <- "Ubuntu"
title_font  <- "Bungee"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-08-21")
df <- tt_data$week21_calfire_frap

# Data wrangling ----------------------------------------------------------

df_count <- df |>
      select(alarm_date, fire_cause) |>
      filter(
        !is.na(alarm_date) & trimws(alarm_date) != "" &
        !is.na(fire_cause) & trimws(fire_cause) != ""
      ) |> mutate(year = year(alarm_date), month = month(alarm_date), day = day(alarm_date)) |>
      group_by(fire_cause, month, year) |> count(fire_cause,month, year)

df_wider <- df_count |> 
      pivot_wider(names_from = fire_cause, values_from = n) |>
      replace_na(list(Human = 0, Natural = 0, Unknown = 0)) |>
      mutate( max_cause = case_when(
          Human >= Natural & Human >= Unknown ~ "Human", 
          Natural >= Human & Natural >= Unknown ~ "Natural", 
          Unknown >= Human & Unknown >= Natural ~ "Unknown"),
          max_count = max(Human, Natural, Unknown)) |>
          filter(month >= 1, month <= 12)

ecdf_human <- ecdf(df_wider$Human)
ecdf_natural <- ecdf(df_wider$Natural)
ecdf_unknown <- ecdf(df_wider$Unknown)

df_wider <- df_wider |>
  mutate(
    percentile = case_when(
      max_cause == "Human" ~ ecdf_human(Human),
      max_cause == "Natural" ~ ecdf_natural(Natural),
      max_cause == "Unknown" ~ ecdf_unknown(Unknown),
      TRUE ~ NA_real_
    )
  )

ramp_human <- colorRamp(c("white", "#E60000"))
ramp_natural <- colorRamp(c("white", "#56B4E9"))
ramp_unknown <- colorRamp(c("white", "#E0E0E0"))

df_clean <- df_wider |>
  rowwise() |>
  mutate(
    tile_color = case_when(
      max_cause == "Human" ~ {
        rgb_vals <- ramp_human(percentile)
        rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
      },
      max_cause == "Natural" ~ {
        rgb_vals <- ramp_natural(percentile)
        rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
      },
      max_cause == "Unknown" ~ {
        rgb_vals <- ramp_unknown(percentile)
        rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
      },
      TRUE ~ NA_character_
    )
  ) |>
  ungroup()

# Graph data -------------------------------------------------------------

p <- ggplot(df_clean, aes(x = year, y = month, fill = tile_color)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_hline(yintercept = 4.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 9.5, linetype = "dashed", color = "red") +
    annotate("text", x = 1950, y = 7, label = "Wildfire Season", 
      color = "grey40", 
      size = 5, 
      fontface = "bold", 
      angle = 90,
      vjust = -1.5
    ) +
    scale_y_continuous(
      breaks = seq(1, 12, 1),
      labels = month.abb,
      trans = "reverse"
    ) +
    scale_x_continuous(breaks = seq(1950, 2018, 4)) +
    scale_fill_identity() +
    labs(
      title     = "Calling Out Those Most Responsible for California Wildfires",
      subtitle  = "Since 1950, California's fire records reveal a shift: <span style='color:#E60000;'>**Human**</span> causes rising above <span style='color:#3D7DA2;'>**'Natural'**</span> and <span style='color:#848484;'>**'Unknown'**</span>. Stronger shades mark higher incident counts relative to each cause's history. Tile colors change because each month-year is shaded by whichever cause was dominant at that time.",
      caption   = "Dataset: California Fires (TidyTuesdayR) · Graphic: Bayu Prahara"
    ) +
    theme_minimal(base_family = body_font) +
    theme(
      #Title, subtitle, and caption
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_textbox_simple(
          family = title_font, 
          face = "bold", 
          size = 24,
          color = "#700000a8",
          width = unit(1, "npc"),
          padding = margin(0, 0, 5, 0),
          margin = margin(2, 0, 4, 0)
      ),
      plot.subtitle = element_textbox_simple(
          family = body_font,
          size = 14,
          lineheight = 1.4,
          padding = margin(0, 0, 0, 0),
          margin = margin(2, 0, 15, 0)
      ),
      plot.caption = element_text(
          hjust = 1, 
          size = 10, 
          color = "grey40", 
          margin = margin(t = 25)
      ),
      strip.text = element_textbox_simple(
          size = 8, 
          family = body_font, 
          face = "bold", lineheight = 1.1,
          width = unit(4, "lines"), 
          halign = 0.5, 
          margin = margin(b = 1)),
      axis.text = element_text(size = 8, family = body_font, color = "grey40"),
      axis.title = element_blank(),
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(40, 40, 40, 40),
      plot.background = element_rect(fill = bg_col, color = bg_col)
      )

print(p)