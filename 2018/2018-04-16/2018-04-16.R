# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)

# Load data ---------------------------------------------------------------

tt_data <- tt_load("2018-04-16")
#view(tt_data$global_mortality)


# Load fonts --------------------------------------------------------------

font_add_google("Oswald", "oswald")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "oswald"

# Data wrangling ----------------------------------------------------------

peak_HIV <- tt_data$global_mortality |>
    rename(percentage = 'HIV/AIDS (%)') |>
    select(country, year, percentage) |>
    group_by(country) |>
    mutate(
        highlight_group = if_else(any(percentage >= 45), "above 45", "below 45"),
        label_text = if_else(year == 2016 & country %in% c("Botswana", "Zimbabwe", "Swaziland","South Africa", "Namibia" ), country, NA_character_)
    ) |>
    ungroup()
view(peak_HIV)

# Graph data -------------------------------------------------------------

plt <- ggplot(peak_HIV, aes(x = year, y = percentage, group = country)) +
    # Grid lines
    geom_hline(
        yintercept = seq(0, 70, by = 10),
        color = "grey91",
        linewidth = 1
    ) +
    # Main lines
    geom_line(aes(
        color = highlight_group, 
        linewidth = highlight_group 

    )) +
    # Labels
    geom_text_repel(
        aes(label = label_text, color = highlight_group),
        family = body_font,
        direction = "y",
        xlim = c(2016.5, NA),
        hjust = 0,
        segment.linetype = "dotted",
        fontface = "bold",
        size = 4.5,
        na.rm = TRUE
    ) +
    # Scales
    scale_color_manual(
        values = c("above 45" = "#da3131ff", "below 45" = "grey60"),
        guide = "none"
    ) +
    scale_linewidth_manual(
        values = c("above 45" = 1.0, "below 45" = 0.4),
        guide = "none" 
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(1990, 2021),
        breaks = seq(1990, 2016, 1),
        labels = function(x) sprintf("'%02d", x %% 100)
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(-2, 70),
        breaks = seq(0, 70, 10),
        labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(clip = "off") +
    # Labels
    labs(
        title = "HIV/AIDS Mortality Rate Trends in Southern Africa",
        subtitle = "Mortality rates peaked in select countries but declined steadily from early 2000s to 2016.",
        caption = "Source: Global Mortality (Our World in Data/TidytuesdayR) · Graphic: Bayu Prahara"
    ) + 
    # Theme
    theme_minimal(base_family = body_font) +
    theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 28,
            lineheight = 1.1,
            padding = margin(0, 0, 5, 0),
            margin = margin(0, 0, 10, 0)
        ),
        plot.subtitle = element_textbox_simple(
            family = body_font,
            size = 18,
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
        axis.title = element_blank(),
        axis.text = element_text(color = "grey40"),
        axis.text.x = element_text(size = 10, margin = margin(t = 5)),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )

