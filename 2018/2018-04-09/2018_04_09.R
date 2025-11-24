# Load packages -----------------------------------------------------------
library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)

# Load data ---------------------------------------------------------------
tt_data <- tt_load("2018-04-09")
nfl_salary <- tt_data$nfl_salary |>
    rename_with(~ str_replace(., "^nfl_salary\\.", ""))

# Load fonts --------------------------------------------------------------
font_add_google("Archivo", "archivo")
font_add_google("Libre Franklin", "libre")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
body_font <- "libre"
title_font <- "archivo"

# Data wrangling ----------------------------------------------------------

nfl_summarise <- nfl_salary |>
    pivot_longer(-year, names_to = "position", values_to = "salary") |>
    group_by(position, year) |>
    summarise(
        mean_salary = mean(salary, na.rm = TRUE),
        median_salary = median(salary, na.rm = TRUE),
        inequality = round(((mean_salary - median_salary) / median_salary * 100), 2),
        .groups = "drop"
    ) |>
    mutate(
        highlight_group = if_else(position == "Quarterback", "Quarterback", "Other"),
        label_text = if_else(year == 2018, position, NA_character_)
    )

# Graph data -------------------------------------------------------------

plt <- ggplot(nfl_summarise, aes(x = year, y = inequality)) +
    # Grid lines
    geom_hline(
        yintercept = seq(0, 500, by = 100),
        color = "grey91",
        linewidth = 1
    ) +
    # Main lines
    geom_line(aes(
        color = highlight_group, 
        linewidth = highlight_group, 
        group = position
    )) +
    # Labels
    geom_text_repel(
        aes(label = label_text, color = highlight_group),
        family = body_font,
        direction = "y",
        xlim = c(2018.5, NA),
        hjust = 0,
        segment.linetype = "dotted",
        fontface = "bold",
        size = 4.5,
        na.rm = TRUE
    ) +
    # Scales
    scale_color_manual(
        values = c("Quarterback" = "#da3131ff", "Other" = "grey60"),
        guide = "none"
    ) +
    scale_linewidth_manual(
        values = c("Quarterback" = 1.0, "Other" = 0.4),
        guide = "none" 
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(2011, 2020),
        breaks = seq(2011, 2018, 1)
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(-20, 500),
        breaks = seq(0, 500, 100)
    ) +
    coord_cartesian(clip = "off") +
    # Labels
    labs(
        title = "Quarterback Position Shows Highest Pay Disparity in NFL",
        subtitle = "Quarterback consistently has the highest salary inequality among all positions, with gap reaching over 400% by 2018.",
        caption = "Source: NFL Positional Salaries (TidytuesdayR) · Graphic: Bayu Prahara"
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