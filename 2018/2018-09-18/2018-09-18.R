# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(janitor)

# Load fonts --------------------------------------------------------------

font_add_google("Bebas Neue")
font_add_google("Barlow")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#fff9f9ff"
text_col    <- "#181D25"
body_font   <- "Barlow"
title_font  <- "Bebas Neue"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-09-18")
df <- tt_data$hypoxia

# Data wrangling ----------------------------------------------------------

df <- df |> janitor::clean_names() |> rename_with(~ gsub("^_", "", .x))
names(df)
df_clean <- df[-1, ] |> 
        mutate( feet  = c(0, 10000, 20000, 30000, 40000, 50000),
                meter = c(0, 3000, 6100, 9100, 12000, 15200)
                ) |>
        mutate(across(3:10, as.numeric)) |>
        select(-`x1`, -altitude, air_press, pp_o2)

triangle_df <- tibble(x = c(50, 100, 75, 50), y = c(0, 0, 29031, 0), group = 1)

# Graph data --------------------------------------------------------------

p <- ggplot(df_clean, aes(x = alv_p_o2, y = feet)) + 
        geom_line(color = "blue") +
        geom_point(size = 3, color = "blue") +
        geom_text(aes(label = alv_p_o2),size = 4.5, vjust = -1, hjust = 0.5) +
        geom_polygon(
                data = triangle_df, 
                aes(x = x, y = y, group = group), 
                fill = "orange", 
                alpha = 0.3,
                inherit.aes = FALSE
        ) +
        geom_hline(yintercept = 10000, linetype = "dashed", color = "grey40"
        ) +
        annotate("text", x = 75, y = 35000, label = "Everest mountain\n(29,031ft/8,848m)", 
                color = "#700000", 
                size = 4.5, 
                fontface = "bold"
        ) +
        annotate("text", x = 35, y = 5000, label = "Safe Zone", 
                color = "#700000", 
                size = 4.5, 
                fontface = "bold",
                hjust = 0.5
        ) +
        scale_y_continuous(
                breaks = seq(0, 50000, 10000),
                labels = c("Sea level", "10k/3k", "20k/6.1k", "30k/9.1k", "40k/12k", "50k/15.2k")
        ) +
            labs(
                title     = "Pilot Hypoxia Risk: PaO₂ Drops Below Safe Levels at Altitude",
                subtitle  = "Once effective altitude climbs beyond about 16,000 to 26,000 feet (5 to 8 km), the oxygen pressure in our lungs drops below the safe zone and hypoxia becomes a real risk for pilots who are not using supplemental oxygen. Commercial jets typically fly around 35,000 to 40,000 feet (10 to 12 km) while the cabin is kept near 6,000 to 8,000 feet, so breathing still feels normal for most passengers.",
                caption   = "Dataset: Hypoxia (TidyTuesdayR) · Graphic: Bayu Prahara",
                x         = "mmHg",
                y         = "Feet/Meter"
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
        axis.title = element_text(size = 12, family = body_font, color = "grey40", face = "bold"),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col)
        )
print(p)