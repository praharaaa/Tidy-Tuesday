# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(ggrepel)

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

tt_data <-tt_load("2018-08-28")
df <- tt_data$`nfl_2010-2017`

# Data wrangling ----------------------------------------------------------

highlighted <- c(
  "Michael Vick", "Cam Newton", "Colin Kaepernick", "Robert Griffin",
  "Randall Cunningham", "Terrelle Pryor", "Tim Tebow", "Daunte Culpepper",
  "Joe Webb", "Cleo Lemon", "C.J. Beathard", "Clint Stoerner", "Dak Prescott",
  "DeShone Kizer", "Daunte Culpepper",  "Kevin Hogan",
  "Scott Tolzien", "Deshaun Watson", "Tyler Thigpen", "Koy Detmer")

df_QB <- df |>
        select(-`...1`, -(rec:rate)
        ) |>
        filter(position == "QB"
        ) |>
        drop_na() |>
        group_by(name
        ) |>
        summarise(
            mean_rush_att = mean(rush_att, na.rm = TRUE),
            mean_rush_yds = mean(rush_yds, na.rm = TRUE),
            mean_rush_avg = mean(rush_avg, na.rm = TRUE),
            mean_rush_tds = mean(rush_tds, na.rm = TRUE),
            .groups = "drop"
        ) |>
        filter(mean_rush_tds != 0
        ) |>
        arrange(name
        ) |> 
        mutate(is_highlighted = name %in% highlighted)

# Graph data --------------------------------------------------------------

p <- ggplot(df_QB, aes(x = mean_rush_tds, y = mean_rush_avg)) +
    coord_cartesian(clip = "off") +
    geom_point(aes(color = is_highlighted), alpha = 0.4) +
    geom_text_repel(
        data = df_QB |> filter(is_highlighted),
        aes(x = mean_rush_tds, y = mean_rush_avg, label = name),
        size = 3, fontface = "bold", color = "black",
        box.padding = 0.3, point.padding = 0.2, max.overlaps = 20,        
        # add connecting lines
        segment.color = "grey50",
        segment.size  = 0.5,
        segment.alpha = 0.7,
        min.segment.length = 0      
    ) +
    scale_color_manual(values = c(
        "TRUE"  = "#0085d8",
        "FALSE" = "#9ebdd0ff")
    ) +
    guides(
        color = "none"
    ) +
    labs(
        x = "Total Rushing Touchdowns",
        y = "Average Yards per Attempt",
        title = "The Rise of Running Quarterbacks in Modern Football",
        subtitle = "Career rushing statistics for NFL quarterbacks from 2000 to 2017. Each point shows a quarterback's complete rushing performance, combining total touchdowns with yards per attempt efficiency across their entire career.",
        caption  = "Dataset: NFL Stats (TidytuesdayR) · Graphic: Bayu Prahara"
    ) +
    theme_classic(base_family = body_font
    ) +
    theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 28,
            lineheight = 1.1,
            padding = margin(0, 0, 5, 0),
            margin = margin(0, 0, 15, 0)
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
        legend.margin = margin(t = -10, b = 0, l = -58, r = 0),
        legend.box.margin = margin(b = 20),
        legend.spacing.x = unit(0.5, "cm"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)),
        axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        axis.line = element_line(color = alpha("grey70", 0.5)),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )
    
print(p)