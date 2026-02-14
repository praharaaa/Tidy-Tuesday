# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(ggrepel)

# Load fonts --------------------------------------------------------------

font_add_google("Squada One")
font_add_google("Libre Franklin")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9f9f9"
text_col    <- "#181D25"
body_font   <- "Libre Franklin"
title_font  <- "Squada One"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-10-09")
df <- tt_data$voter_turnout

# Data wrangling ----------------------------------------------------------

df_group <- df |>
    filter(state != "United States") |>
    mutate(
        votes = as.numeric(votes),
        eligible_voters = as.numeric(eligible_voters)
    ) |>
    group_by(year) |>
    summarise(
        sum_votes = sum(votes, na.rm = TRUE),
        sum_eligible = sum(eligible_voters, na.rm = TRUE),
        turnout_rate = (sum_votes / sum_eligible)*100,
        .groups = "drop"
    ) |>
    mutate(
        election_type = case_when(
        year %% 4 == 0 ~ "Presidential",
        TRUE ~ "Midterm"
        ))

president_terms <- tibble(
    xmin = c(1980, 1988, 1992, 2000, 2008),
    xmax = c(1988, 1992, 2000, 2008, 2014),
    party = c("Republican", "Republican", "Democrat", 
                "Republican", "Democrat"),
    president = c("Ronald\nReagan", "G.H.W\nBush", "Bill\nClinton",
                    "G.W\nBush", "Barack\nObama"))

# Graph data --------------------------------------------------------------

p <- ggplot(df_group, aes(x = year, y = turnout_rate, color = election_type, linewidth = election_type)) +
    geom_rect(
        data = president_terms,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 80, fill = party),
        alpha = 0.05,
        inherit.aes = FALSE
    ) +
    geom_text(
        data = president_terms,
        aes(x = (xmin + xmax) / 2, y = 75, label = president),
        vjust = 1,
        size = 4,
        fontface = "bold",
        inherit.aes = FALSE
    ) +
    geom_vline(
        xintercept = c(1988, 1992, 2000, 2008),
        linetype = "dashed",
        color = "grey60",
        alpha = 0.5
    ) +
    geom_line(
    ) +
    geom_text_repel(
        data = df_group |> filter(year %in% c(2012,2014)),
        aes(label = election_type, color = election_type),
        family = body_font,
        direction = "y",
        xlim = c(2015, NA),
        hjust = 0.5,
        segment.linetype = "dotted",
        fontface = "bold",
        size = 3.5,
        na.rm = TRUE
    ) +
    scale_y_continuous(
        breaks = seq(0, 80, 10),
        labels = scales::percent_format(scale = 1)
    ) +
    scale_x_continuous(
        breaks = seq(1980, 2014, 4), 
        labels = function(x) floor(x)
    ) +
    scale_color_manual(
        values = c("Presidential" = "#4a3e27", "Midterm" = "#a0a0a0ff"),
        guide = "none"
    ) +
    scale_fill_manual(
        values = c("Republican" = "#E60000", "Democrat" = "#0015BC"),
        guide = "none"
    ) +
    scale_linewidth_manual(
        values = c("Presidential" = 1.2, "Midterm" = 0.6),
        guide = "none"
    ) +
    labs(
        title = "The Turnout Gap That Defines United States Election Cycles",
        subtitle = "In the United States, presidential elections (every 4 years) consistently draw 30-60% voter turnout, while midterm elections (congressional races 2 years later) drop to 15-40%. This pattern holds steady across both <span style='color:#d32f2f;'>**Republican**</span> and <span style='color:#2c728e;'>**Democratic**</span> administrations from 1980-2014.",
        caption = "Dataset: Voter Turnout(TidyTuesdayR) · Graphic: Bayu Prahara"
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
                color = "#700000",
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
        axis.text = element_text(size = 12, family = body_font, color = "grey40"),
        axis.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col)
    )

print(p)