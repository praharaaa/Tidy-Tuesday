# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(countrycode)
library(viridis)

# Load fonts --------------------------------------------------------------

font_add_google("Bebas Neue")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#f9fffeff"
text_col <- "#181D25"
body_font <- "Inter"
title_font <- "Bebas Neue"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-06-12")
df <- tt_data$week11_fifa_audience

# Data wrangling ----------------------------------------------------------

df_clean <- df |> 
  mutate(subregion = countrycode(country, "country.name", "region")) |>
  filter(!is.na(subregion)) |>
  group_by(confederation, subregion) |>
  summarise(
    total_countries = n(),
    total_tv_audience = sum(tv_audience_share, na.rm = TRUE),
    .groups = "drop"
  )

# Graph data -------------------------------------------------------------

plt <- ggplot(df_clean, aes(x = confederation, y = total_tv_audience, fill = subregion)) +
        geom_bar(stat = "identity", position = "fill", color = "white") +
        scale_y_continuous(
            labels = function(x) paste0(x * 100, "%")
        ) +
        labs(
            title    = "Where Football Fans Are Watching: A Regional Breakdown",
            subtitle = "Each confederation draws its audience from distinct subregions, revealing patterns in global football engagement.",
            x        = "Confederation",
            y        = "Percentage",
            caption  = "FIFA Audiences (TidytuesdayR) · Graphic: Bayu Prahara"
        ) +
        scale_fill_viridis_d(option = "D") +
        theme_minimal(base_family = body_font) +
        theme(
            # Title, subtitle, and caption
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.title = element_textbox_simple(
                family = title_font, 
                face = "bold", 
                size = 32,
                color = "steelblue",
                width = unit(1, "npc"),
                padding = margin(0, 0, 5, 0),
                margin = margin(2, 0, 2, 0)
            ),
            plot.subtitle = element_textbox_simple(
                family = body_font,
                size = 14,
                lineheight = 1.4,
                padding = margin(0, 0, 0, 0),
                margin = margin(10, 0, 20, 0)
            ),
            plot.caption = element_text(
                hjust = 1, 
                size = 12, 
                color = "grey40", 
                margin = margin(t = 40)
            ),
            # Legend
            legend.position = "top",
            legend.justification = "left",
            legend.direction = "horizontal",
            legend.margin = margin(t = 0, b = 20, l = -48, r = 0),
            legend.key.spacing.x = unit(1, "cm"),
            legend.box.spacing = unit(0.2, "cm"),
            legend.key.width = unit(0.7, "cm"), 
            legend.key.height = unit(0.8, "cm"),
            legend.title = element_text(size = 10, face = "bold", color = "grey40", margin = margin(r = 10)),
            legend.text  = element_text(size = 10),
            # Axis
            axis.text = element_text(color = "grey40"),
            axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 10, margin = margin(t = 20), face = "bold", color = "grey40"),
            axis.text.y = element_text(size = 10, margin = margin(r = 8)),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text  = element_text(face = "bold", size = 12),
            # Plot
            plot.margin = margin(40, 40, 40, 40),
            plot.background = element_rect(fill = bg_col, color = bg_col),
            panel.background = element_rect(fill = bg_col, color = bg_col),
            panel.spacing.y = unit(1, "lines")
        )