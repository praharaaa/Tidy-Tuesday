# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(cowplot)
library(tigris)
library(sf)
library(viridis)

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

tt_data <-tt_load("2018-07-17")
df <- tt_data$week16_exercise
df <- rename(df, NAME = state)

us_states_sf <- states(cb = TRUE, resolution = "20m")
us_states_sf_shifted <- shift_geometry(us_states_sf) |> filter(NAME %in% state.name) 

# Data wrangling ----------------------------------------------------------

df$men_nonworking <- as.numeric(df$men_nonworking)
df$women_nonworking <- as.numeric(df$women_nonworking)
# Fill NA values based on the journal
df$men_nonworking[c(21, 36)] <- c(7, 15)
df$women_nonworking[c(28, 36, 43)] <- c(9, 8, 9)

df <- df |> filter(row_number() != 1)
cols <- c("men_working", "women_working", "men_nonworking", "women_nonworking")
col_means <- sapply(df[cols], mean, na.rm = TRUE)
col_sds <- sapply(df[cols], sd, na.rm = TRUE)

df_categorized <- df |>
        mutate(across(
            all_of(cols),
            ~ {
            col_name <- cur_column()
            m <- col_means[col_name]
            s <- col_sds[col_name]
            case_when(
                . < m - 1.5 * s ~ "significantly low",
                . < m ~ "low",
                . > m + 1.5 * s ~ "significantly high",
                . > m ~ "high",
                TRUE ~ "average"
            )
            },
            .names = "{.col}_cat"
        )) |>
        select(NAME, men_working, men_working_cat, women_working, women_working_cat, men_nonworking, men_nonworking_cat, women_nonworking, women_nonworking_cat)

df_mapped <- left_join(us_states_sf_shifted, df_categorized, by = "NAME") |>
        pivot_longer(
            cols = ends_with("_cat"),
            names_to = "category",
            values_to = "level"
        ) |>
        mutate(category_label = str_remove(category, "_cat")) |>
        separate(category_label, into = c("gender", "status"), sep = "_") |>
        mutate(level = factor(level, 
                       levels = c("significantly high", 
                                  "high", 
                                  "average", 
                                  "low", 
                                  "significantly low"),
                       ordered = TRUE))

#view(df_mapped)

p <- ggplot(df_mapped) +
    geom_sf(aes(fill = level), color = "white", size = 0.1, alpha = 0.8) +
    facet_grid(status ~ gender,
                switch = "y", 
                labeller = labeller(
                status = c("working" = "Working", "nonworking" = "Non-working"),
                gender = c("men" = "Men", "women" = "Women")
                )) +
    labs(
        title = "Mapping Leisure-Time Physical Activity Across 50 States",
        subtitle = "Percentage of adults (ages 18–64) meeting both aerobic and muscle-strengthening federal guidelines through leisure-time physical activity, segmented by sex and work status, based on data from the National Health Interview Survey (2010–2015).",
        caption = "Dataset: TidyTuesdayR · Graphic: Bayu Prahara"
    ) +
    scale_fill_manual(
        name = "Level",
        values = c(
            "significantly high" = "#003366",
            "high"               = "#7AC6E7",
            "average"            = "#C7E77A",
            "low"                = "#E07B7B",
            "significantly low"  = "#990000"
        )
    ) +
    coord_sf(clip = "off") +
    theme_void() +
    theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 24,
            color = "#002f70a8",
            width = unit(1, "npc"),
            padding = margin(0, 0, 5, 0),
            margin = margin(2, 0, 4, 0)
        ),
        plot.subtitle = element_textbox_simple(
            family = body_font,
            size = 14,
            lineheight = 1.4,
            padding = margin(0, 0, 0, 0),
            margin = margin(2, 0, 20, 0)
        ),
        plot.caption = element_text(
            hjust = 1, 
            size = 10, 
            color = "grey40", 
            margin = margin(t = 25)
        ),
        # Legend
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.margin = margin(t = 20, b = 20, l = 0, r = 0),
        legend.key.spacing.x = unit(1, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.width = unit(0.7, "cm"), 
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_text(size = 10, face = "bold", color = "grey40", margin = margin(r = 10)),
        legend.text  = element_text(size = 10),
        strip.text.x  = element_text(face = "bold", size = 12, angle = 0),
        strip.text.y  = element_text(face = "bold", size = 12, angle = 90, margin = margin(l = 5, r = 5)),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )

print(p)
