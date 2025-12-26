# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9fffeff"
text_col    <- "#181D25"
body_font   <- "Inter"
title_font  <- "Oswald"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-06-26")
df <- tt_data$week13_alcohol_global
df_2 <- read.csv("Religious Composition 2010-2020 (percentages).csv")

# Data wrangling ----------------------------------------------------------
df <- df |> mutate(
            country = case_when(
                country == "USA" ~ "United States",
                country == "Cote d'Ivoire" ~ "Ivory Coast",
                country == "Cabo Verde" ~ "Cape Verde",
                country == "Swaziland" ~ "Eswatini",
                country == "Macedonia" ~ "North Macedonia",
                country == "Russia" ~ "Russian Federation",
                TRUE ~ country
                )
            )

df_2 <- df_2 |>
            rename_all(tolower) |>
            filter(year == 2010) |>
            select(region, country, population, muslims) |>
            mutate(
            muslims_percentage = round(muslims, 2),
            muslims_population = population * muslims_percentage,
            country = case_when(
                country == "Democratic Republic of the Congo" ~ "DR Congo",
                country == "Republic of the Congo" ~ "Congo",
                country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
                country == "St. Vincent and the Grenadines" ~ "St. Vincent & the Grenadines",
                country == "United States" ~ "United States",
                country == "Sao Tome and Principe" ~ "Sao Tome & Principe",
                country == "Russia" ~ "Russian Federation",
                grepl("^All ", country) ~ NA_character_,
                TRUE ~ country
                )
            ) |>
            filter(!is.na(country))

df_clean <- df |>
            full_join(df_2, join_by(country)) |>
            filter(
                is.finite(beer_servings) &
                is.finite(spirit_servings) &
                is.finite(wine_servings) &
                is.finite(total_litres_of_pure_alcohol) &
                is.finite(muslims)
            ) |>
            select(region, country, everything())

highlight_countries <- c("Iran", "Indonesia", "India", "Nigeria", "Uganda", "Lebanon", 
                         "Gambia", "Qatar", "Eritrea", "Ethiopia", "Benin", 
                         "Guinea-Bissau", "Burkina Faso", "Sierra Leone", 
                         "Tanzania", "Bosnia-Herzegovina", 
                         "North Macedonia", "Montenegro", "Kazakhstan", 
                         "Cyprus", "Albania", "Malaysia")

df_clean_10 <- df_clean |> 
            filter(muslims_percentage > 10) |> 
            mutate(highlight = country %in% highlight_countries)

# Graph data -------------------------------------------------------------

plt <-  ggplot(df_clean_10, aes(x = total_litres_of_pure_alcohol, y = muslims_percentage, color = region, size = muslims_population)) +
        geom_point(alpha = 0.8) +
        geom_text_repel(
              data = df_clean_10 |> filter(highlight),
              aes(label = country),
              color = text_col,
              fontface = "bold",
              size = 4,
              box.padding = 0.5,
              point.padding = 0.3,
              segment.color = "gray50",
              segment.size = 0.3,
              vjust = 1.2,
              direction = "y",
              max.overlaps = Inf
        ) +
        scale_y_continuous(
          labels = label_percent(accuracy = 1, scale = 1)
        ) +
        scale_size_continuous(guide = "none") +
        scale_color_discrete(
          guide = guide_legend(override.aes = list(size = 3))
        ) +
        labs(
            title    = "Alcohol Consumption and Muslim Population by Country (2010)",
            subtitle = "Pure alcohol consumption (liters per capita) for countries with Muslim populations above 10%.",
            caption  = "Source: Alcohol Consumption (TidytuesdayR) | Religious Composition 2010 (Pew Research) · Graphic: Bayu Prahara",
            x = "Total Liters of Pure Alcohol per Capita",
            y = "Muslim Population (%)",
            color = "Region"
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
            color = "#0165f0a8",
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
            size = 10, 
            color = "grey40", 
            margin = margin(t = 20)
        ),
        # Legend
        legend.position = "top",
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.margin = margin(t = 0, b = 20, l = -48, r = 0),
        legend.key.spacing.x = unit(1, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.width = unit(0.3, "cm"), 
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        legend.text  = element_text(size = 10),
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
        axis.title.x = element_text(margin = margin(t = 10), color = "grey40"),
        axis.text.x = element_text(size = 10, margin = margin(t = 20)),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        axis.line.x = element_line(color = "grey40", linewidth = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        # Panel
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
        )