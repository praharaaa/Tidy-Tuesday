# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Alfa Slab One")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#fffcf6ff"
text_col <- "#181D25"
body_font <- "Inter"
title_font <- "Alfa Slab One"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-06-05")
df <- tt_data$week10_biketown

# Data wrangling ----------------------------------------------------------

df_clean <- df |>
  mutate(
    StartDate = mdy(StartDate), 
    StartHour = hour(hms(StartTime)),
    DayOfWeek = wday(StartDate),
    IsWeekend = if_else(DayOfWeek %in% c(1, 7), "Weekend", "Weekday"),
    Month = month(StartDate)
  ) |>
  filter(StartHour >= 0, StartHour <= 5) |>
  filter(!if_all(RouteID:RentalAccessPath, ~ is.na(.))) |>
  filter(Distance_Miles > 0, Distance_Miles <= 80) |>
    group_by(StartHour, PaymentPlan, IsWeekend, Month) |>
    summarise(
        n = n(),
        .groups = "drop"
    ) |>
  group_by(StartHour, PaymentPlan, IsWeekend) |>
  summarise(
    n_min  = min(n),
    n_mean = mean(n),
    n_max  = max(n),
    .groups = "drop"
  )

df_plot <- df_clean |>
  filter(StartHour %in% 0:5) |>
  mutate(
    StartHour_f = factor(
      StartHour,
      levels = c(0:5)
    )
  )

# Graph data -------------------------------------------------------------

plt <- ggplot(df_plot, aes(x = StartHour_f, y = n_mean, group = 1)) +
      geom_ribbon(
        aes(ymin = n_min, ymax = n_max), fill  = "skyblue", alpha = 0.3
      ) +
      geom_line(color = "steelblue", linewidth  = 0.8) +
      geom_point(color = "steelblue", size  = 1.3) +
      facet_grid(PaymentPlan ~ IsWeekend) +
      labs(
        title    = "Who Rides Biketown After Midnight?",
        subtitle = "Average hourly rides between midnight and 5 AM, with shaded bands showing minimum and maximum monthly values. Subscriber rides rise again around 4-5 AM on weekdays, likely early shift workers.",
        x        = "Start hour",
        y        = "Number of rides",
        caption  = "NIKE Biketown (TidytuesdayR) · Graphic: Bayu Prahara"
      ) +
      scale_x_discrete(
        labels = c("0" = "00:00",
                  "1" = "01:00",
                  "2" = "02:00",
                  "3" = "03:00",
                  "4" = "04:00",
                  "5" = "05:00")
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
          color = "steelblue ",
          width = unit(1, "npc"),
          padding = margin(0, 0, 5, 0),
          margin = margin(2, 0, 2, 0)
      ),
      plot.subtitle = element_textbox_simple(
          family = body_font,
          size = 14,
          lineheight = 1.4,
          padding = margin(0, 0, 0, 0),
          margin = margin(10, 0, 20, 0)
      ),
      plot.caption = element_text(
          hjust = 1, 
          size = 12, 
          color = "grey40", 
          margin = margin(t = 40)
      ),
      # Legend
      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
      legend.key.spacing.x = unit(1.5, "cm"),
      legend.key.width = unit(1, "cm"), 
      legend.key.height = unit(1, "cm"),
      legend.title = element_text(size = 14, face = "bold",color = "grey40", margin = margin(r = 30)),
      legend.text = element_text(size = 14),
      # Axis
      axis.text = element_text(color = "grey40"),
      axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, margin = margin(t = 20), face = "bold", color = "grey40"),
      axis.text.y = element_text(size = 10, margin = margin(r = 8)),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text  = element_text(face = "bold", size = 12),
      # Plot
      plot.margin = margin(40, 40, 40, 40),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      panel.background = element_rect(fill = bg_col, color = bg_col),
      panel.spacing.y = unit(1, "lines")
      )
print(plt)