# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(rotl)
library(treemapify)

# Load fonts --------------------------------------------------------------

font_add_google("Bebas Neue")
font_add_google("Barlow")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9fff9ff"
text_col    <- "#181D25"
body_font   <- "Barlow"
title_font  <- "Bebas Neue"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-09-25")
df <- tt_data$africa_species

# Data wrangling ----------------------------------------------------------

# Pull species list first
df_species <- df |> distinct(species) |> pull(species)
# Check based on OTL
matches <- rotl::tnrs_match_names(df_species) |> filter(!is.na(unique_name))
# Left join with case insensitive matching
df_joined <- df |> 
  mutate(species_lower = str_to_lower(species)
  ) |>
  left_join(
    matches |> mutate(search_string_lower = str_to_lower(search_string)),
    by = join_by(species_lower == search_string_lower)
  ) |> 
  select(unique_name, country, kingdom, origin, environment_system
  ) |>
  mutate(genus = word(unique_name, 1)
  ) |>
  filter(!is.na(unique_name)) 
# Split the genus from unique_name and count
df_genus <- df_joined |> 
  count(genus, kingdom, name = "n_invasive") |> 
  group_by(kingdom) |>
  mutate(rank = row_number(desc(n_invasive))) |>
  ungroup() |>
  filter(
    (kingdom == "Animalia" & rank <= 12) |
    (kingdom == "Plantae" & rank <= 12) |
    kingdom %in% c("Fungi", "Bacteria", "Chromista")
  ) |>
  select(-rank) |>
  arrange(desc(n_invasive))

# Graph data --------------------------------------------------------------

p <- ggplot(df_genus, aes(area = n_invasive, fill = kingdom, label = paste(genus, n_invasive, sep = "\n"))) +
  geom_treemap(color = "white", size = "2") +
  geom_treemap_text(
    color = "white",
    place = "centre",
    fontface = "bold",
    size = 12,
    grow = FALSE
  ) +
  scale_fill_manual(
    values = c("Animalia"  = "#A40c0A",
               "Bacteria"  = "#7215D8",
               "Chromista" = "#e51872",
               "Fungi"     = "#b74707", 
               "Plantae"   = "#26762c"
               )
  ) +
  facet_wrap(~kingdom, nrow = 5, ncol = 1) +
  labs(
    title    = "Africa Has an Invasive Species Problem",
    subtitle = "This visualization highlights the top 12 genera with the most invasive species in several regions or countries in Africa. The reality? Over 1,000 invasive species are threatening the continent's ecosystems with no natural predators to keep them in check.",
    caption  = "Dataset: Invasive Species in Africa (TidyTuesdayR) · Graphic: Bayu Prahara"
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    # Title, subtitle, and caption
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
        family = title_font, 
        face = "bold", 
        size = 28,
        color = "#d32f2f",
        width = unit(1, "npc"),
        padding = margin(0, 0, 5, 0),
        margin = margin(2, 0, 10, 0)
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
    strip.text = element_textbox_simple(
        size = 16, 
        family = body_font, 
        face = "bold", lineheight = 1.1,
        width = unit(4, "lines"),
        hjust = 0,  
        margin = margin(b = 8)),
    legend.position = "none", 
    legend.margin = margin(20, 10, 0, 10),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.title = element_text(family = body_font, face = "bold", color = "grey20"),
    legend.text = element_text(family = body_font, size = 14),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(40, 40, 40, 40),
    panel.spacing.y = unit(2, "lines")
  )

print(p)