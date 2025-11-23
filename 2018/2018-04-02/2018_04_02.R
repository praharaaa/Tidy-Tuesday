# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(tigris)

# Load data ---------------------------------------------------------------

tuesdata <- read.csv(
    "2018_04_02_Tuition Fee in US by state (2004-2025).csv"
    )
US_tuition_fee <-tuesdata |>
    mutate(
        across(X2004:X2025, ~ as.numeric(gsub("[$,]", "", .)))
        )

# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts-------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
highlight_col <- "#181D25"

body_font <- "libre"
title_font <- "domine"

# Data wrangling ----------------------------------------------------------

plot_data <- US_tuition_fee |>
  select(State, X2005, X2025) |>
  mutate(
    percentage = round(((X2025 - X2005) / X2005 * 100),2),
    NAME = State
  )
plot_data <- plot_data |> mutate(NAME = State)

us_states_sf <- states(cb = TRUE, resolution = "20m")
us_states_sf_shifted <- shift_geometry(us_states_sf)

map_data_joined <- left_join(us_states_sf_shifted, plot_data, by = "NAME") |>
  filter(!is.na(percentage))

# Graph data --------------------------------------------------------------

p <- ggplot(map_data_joined) +
  geom_sf(aes(fill = percentage), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#E0ECF4", high = "#ba0000ff", na.value = "grey90", name = "Percentage") +
  theme_void() +
  guides(
    fill = guide_colorbar(
      title.position = "top",       
      title.hjust = 0.5              
    )
  ) +
  labs(
    title = str_wrap("Real Growth in Public College Tuition Across America", 40),
    subtitle = str_wrap("Percentage increase in public four-year in-state tuition and fees from 2005 to 2025, adjusted for inflation (in 2025 dollars).", 70),
    caption = "Source: Collegeboard.com · Graphic: Bayu Prahara"
    ) +
  theme(
    legend.position = "top", 
    legend.margin = margin(20, 10, 0, 10),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.title = element_text(family = body_font, face = "bold", color = "grey20"),
    legend.text = element_text(family = body_font),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.title = element_text(family = title_font, face = "bold", size = 24),
    plot.subtitle = element_text(family = body_font, lineheight = 1),
    plot.margin = margin(15, 15, 15, 15),
    plot.caption = element_text(family = body_font, hjust = 1, size = 8, color = "grey40"),
  ) 
