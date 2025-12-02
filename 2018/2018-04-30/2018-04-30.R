# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)
library(tigris)

# Load data ---------------------------------------------------------------

tt_data <- tt_load("2018-04-30")
df <- tt_data$week5_acs2015_county_data

# Load fonts --------------------------------------------------------------

font_add_google("Playfair Display")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "Playfair Display"

# Data wrangling ----------------------------------------------------------

plt <- ggplot(df, aes(x = Income, y = Poverty, size = TotalPop)) +
    geom_point(
        color = "#0085d8ff",
        alpha = 0.1
    ) +
    geom_smooth(
        linewidth = 1,
        se = TRUE,
        color = "#3366ff",
        show.legend = FALSE
    ) +
    scale_size_area(
        name = "Population",
        max_size = 10,
        labels = label_number(scale_cut = cut_short_scale(), digits = 2),
        guide = guide_legend(direction = "horizontal", nrow = 1)
    ) +
    scale_y_continuous(
        name   = "Poverty rate",
        labels = label_percent(accuracy = 1, scale = 1)
    ) +
    scale_x_log10(
        name   = "Median household income (USD)",
        breaks = c(10000, 30000, 50000, 100000),
        labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    labs(
        title    = "Income and Poverty Across US Counties",
        subtitle = "Counties with higher median household income generally report lower poverty rates.",
        caption  = "Source: ACS Census Data 2015 (TidytuesdayR) · Graphic: Bayu Prahara"
    ) +
    theme_minimal() +
    theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 28,
            padding = margin(0, 0, 0, 0),
            margin = margin(0, 0, 5, 0)
        ),
        plot.subtitle = element_textbox_simple(
            family = body_font,
            size = 14,
            lineheight = 1.4,
            padding = margin(0, 0, 0, 0),
            margin = margin(0, 0, 30, 0)
        ),
        plot.caption = element_text(
            hjust = 1, 
            size = 12, 
            color = "grey40", 
            margin = margin(t = 20)
        ),
        # Legend
        legend.position = c(0, 1),           
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.margin = margin(t = -20, b = 0, l = -42, r = 0),
        legend.box.margin = margin(b = 20),
        legend.spacing.x = unit(0.5, "cm"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
        axis.title.x = element_text(margin = margin(t = 8), color = "grey40"),
        axis.text.x = element_text(size = 10, margin = margin(t = 20), face = "bold", color = "grey40"),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        axis.line.x = element_line(color = "grey40", linewidth = 0.5),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )