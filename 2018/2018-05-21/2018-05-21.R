# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)

# Load data ---------------------------------------------------------------

df <- read.csv("honeyproduction.csv")
view(df)

# Load fonts --------------------------------------------------------------

font_add_google("DM Serif Display")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#fffcf6ff"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "DM Serif Display"

# Data wrangling ----------------------------------------------------------

price_per_year <- df |>
    group_by(year) |>
    mutate(
        is_min = priceperlb == min(priceperlb),
        is_max = priceperlb == max(priceperlb)
    ) |>
    summarise(
        mean       = mean(priceperlb),
        min        = priceperlb[is_min][1],
        max        = priceperlb[is_max][1],
        state_min  = state[is_min][1],
        state_max  = state[is_max][1],
        .groups = "drop"
    )   

# Graph data -------------------------------------------------------------

plt <- ggplot(price_per_year, aes(x = year, y = mean)) +
        geom_point(size = 3, color = "brown") +
        geom_line(aes(y = mean), color = "brown") +
        geom_point(aes(y = mean), color = "brown") +
        geom_errorbar(aes(ymin = min, ymax = max), color = "brown", alpha = 0.4, linetype = "dashed", width = 0.3) +
        geom_text(data = subset(price_per_year, year %in% c(1998, 2012)), aes(
            label = scales::dollar(mean, accuracy = 0.01)), 
            vjust = -1, 
            fontface = "bold", 
            color = "brown", 
            size = 5) +
        scale_y_continuous(
            name = "Price",
            labels = label_currency(accuracy = 0.01, prefix = "$")
        ) +
        scale_x_continuous(
            name = "Year"
        ) +
        labs(
            title    = "Average Honey Prices in the US",
            subtitle = "From 1998 to 2012, the average retail price of honey in the US rise from about $0.83 to $2.37 per pound, and the gap between the cheapest and most expensive states also widened.",
            caption  = "Source: Honey Production (TidytuesdayR) · Graphic: Bayu Prahara"
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
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t = 20), face = "bold", color = "grey40"),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        axis.line.x = element_line(color = "grey40", linewidth = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
        )