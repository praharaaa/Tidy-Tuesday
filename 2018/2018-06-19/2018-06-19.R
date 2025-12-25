# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Roboto Slab")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9fffeff"
text_col    <- "#181D25"
body_font   <- "Inter"
title_font  <- "Roboto Slab"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-06-19")
df <- tt_data$week12_google_trends

# Data wrangling ----------------------------------------------------------

df_clean <- df |>
            pivot_longer(-Day, names_to = "hurricane", values_to = "search") |>
            mutate(hurricane = str_extract(hurricane, "(?<=Hurricane )[A-Za-z]+"))

# Graph data -------------------------------------------------------------

plt <- ggplot(df_clean, aes(x = Day, y = search, group = hurricane, color = hurricane)) +
        geom_line(linewidth = 1) +
        scale_color_manual(
            values = c("Harvey" = "#f08d01ff", "Irma" = "#2c728eff", "Jose" = "#20a486ff", "Maria" = "#67c645ff")
        ) +
        labs(
            title    = "When Hurricanes on the Horizon, Searches on the Rise",
            subtitle = "Daily Google search interest for 
            <span style='color:#f08d01ff;'>**Harvey**</span>, 
            <span style='color:#2c728eff;'>**Irma**</span>, 
            <span style='color:#20a486ff;'>**Jose**</span>, and 
            <span style='color:#75d054ff;'>**Maria**</span> 
            in the United States from August 21 to September 25, 2017.",
            caption  = "Source: Hurricane Google Trends US (TidytuesdayR) · Graphic: Bayu Prahara"
        ) +
        theme_minimal(base_family = body_font)+
        theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 28,
            color = "#f08c01a8",
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
        # Legend
        legend.position = "none",
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