# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Space Grotesk")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9fffeff"
plot_col    <- "#effffcff"
text_col    <- "#181D25"
body_font   <- "Ubuntu"
title_font  <- "Space Grotesk"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-08-07")
df <- tt_data$week19_airline_safety
view(df)

# Data wrangling ----------------------------------------------------------

df_clean <- df |> mutate(year_range = case_when(
                                    year_range == "85_99" ~ "1999",
                                    year_range == "00_14" ~ "2014"),
                  ) |>
                  group_by(airline, year_range, type_of_event
                  ) |>
                  summarise(n_events = sum(as.numeric(n_events), 
                            na.rm = TRUE), 
                            .groups = "drop"
                  ) |>
                  pivot_wider(names_from = c(year_range, type_of_event), 
                              values_from = n_events, 
                              names_glue = "{type_of_event}_{year_range}"
                  ) |>
                  mutate(total_events_1999 = rowSums(across(contains("1999")), na.rm = TRUE),
                         total_events_2014 = rowSums(across(contains("2014")), na.rm = TRUE),
                         diff = total_events_2014 - total_events_1999
                  ) |> 
                  arrange(diff)

df_long <- df_clean |>
                  select(airline, total_events_1999, total_events_2014, diff
                  ) |>
                  pivot_longer(
                        cols = starts_with("total_events_"),
                        names_to = "year",
                        values_to = "events"
                  ) |>
                  mutate(
                        year = case_when(
                        year == "total_events_1999" ~ 1999,
                        year == "total_events_2014" ~ 2014),
                        trend = ifelse(diff <= 0, "decrease", "increase")
                  )

baseline <- df_long |>
                 filter(year == 1999) |>
                 select(airline, events) |>
                 rename(baseline_events = events)

df_plot <- df_long |>
                left_join(baseline, by = "airline") |> mutate(airline = fct_reorder(airline, diff))

panel_bg <- df_plot |>
                distinct(airline, trend) |>
                mutate(
                    xmin = 1998, xmax = 2015,
                    ymin = -Inf, ymax = Inf  
                )

# Graph data -------------------------------------------------------------

p <- ggplot(df_plot, aes(x = year, y = events, group = airline)) +
        geom_line(aes(color = trend), linewidth = 1.2
        ) +
        geom_ribbon(
            aes(ymin = baseline_events, ymax = events, fill = trend),
            alpha = 0.3,
            color = NA
        ) +
        geom_point(aes(color = trend), size = 3
        ) +
        geom_text(
            data = subset(df_plot, year == 1999),
            aes(label = events),
            hjust = 0.5, vjust = -1,
            size = 3,
            family = body_font,
            color = text_col
        ) +
        geom_text(
            data = subset(df_plot, year == 2014),
            aes(label = events),
            hjust = 0.5, vjust = -1,
            size = 3,
            family = body_font,
            color = text_col
        ) +
        geom_rect(
            data = panel_bg,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = trend),
            inherit.aes = FALSE,
            alpha = 0.1,
            color = NA
        ) +
        coord_cartesian(ylim = c(0, 650)) +
        scale_color_manual(
            values = c("increase" = "red", "decrease" = "green"),
            name = "Trend"
        ) +
        scale_fill_manual(
            values = c("increase" = "red", "decrease" = "green"),
            name = "Trend"
        ) +
        scale_x_continuous(
            breaks = c(1999, 2014),
            labels = c("'99", "'14")
        ) +
        facet_wrap(~airline, nrow = 8, ncol = 7) +
        labs(
            title = "Which Airlines Improved Their Safety Records Most Between 1999 and 2014?",
            subtitle = "Tracking combined safety metrics (incidents + fatal accidents + fatalities) relative to 1999 baselines across 56 carriers. Measured per available seat kilometer to account for airline size. *Asterisks indicate regional subsidiaries included.",
            caption = "Dataset: Airline Safety (FiveThirtyEight & TidyTuesdayR) · Graphic: Bayu Prahara"
        ) +
        theme_minimal() +
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
            strip.text = element_textbox_simple(
                size = 8, 
                family = body_font, 
                face = "bold", lineheight = 1.1,
                width = unit(4, "lines"), 
                halign = 0.5, 
                margin = margin(b = 1)),
            axis.text.x = element_text(size = 8, family = body_font),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(40, 40, 40, 40),
            plot.background = element_rect(fill = bg_col, color = bg_col),
            panel.background = element_rect(fill = bg_col, color = bg_col),
            panel.spacing.y = unit(2, "lines"),
        )
print(p)

