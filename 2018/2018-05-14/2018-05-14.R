# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)
library(cowplot)
library(magick)

# Load fonts --------------------------------------------------------------

font_add_google("Russo One")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#fffcf6ff"
text_col <- "#181D25"
body_font <- "Arial"
title_font <- "Russo One"

# Load data ---------------------------------------------------------------

df <- read.csv("week7_starwars.csv", fileEncoding = "latin1")

# Data wrangling ----------------------------------------------------------

# Store the first row as a character vector to extract the true column headers
# (The raw data has headers buried in the first row instead of the column names)
row2 <- as.character(df[1, ])

# Preserve the current column names before we modify them
# Use this to know which positions need updating
new_names <- names(df)

# Replace columns 4-29 with the actual headers from the first data row
new_names[4:29] <- row2[4:29]

# Create a clean dataframe by removing the first row (which was just header data)
clean_df <- df[-1, ]

# Assign the newly constructed column names to the clean dataframe
colnames(clean_df) <- new_names

# Ensure all column names are unique (R will auto-suffix duplicates with .1, .2, etc.)
colnames(clean_df) <- make.unique(colnames(clean_df))

# Convert Yes/No responses in columns 4-9 to a standardized format
clean_df <- clean_df %>%
  mutate(across(4:9, ~ case_when(
    . == cur_column() ~ "Yes",
    is.na(.) | . == "" ~ "No",
    TRUE ~ as.character(.)
  )))

# Replace all empty strings with NA (missing value indicator)
clean_df[clean_df == ""] <- NA

# Define a comprehensive, clean set of column names for all 29 columns
# Using paste0() to generate series like "watch_starwars_ep_1", "watch_starwars_ep_2", etc.
new_names <- c(
        "respondent_id",
        "seen_any_film",
        "is_sw_fan",
        paste0("watch_starwars_ep_", 1:6),
        paste0("starwars_ep_", 1:6),
        "han_solo",
        "luke_skywalker",
        "princess_leia",
        "anakin_skywalker",
        "obi_wan",
        "emperor_palpatine",
        "darth_vader",
        "lando",
        "boba_fett",
        "c3po",
        "r2d2",
        "jar_jar",
        "padme",
        "yoda",
        "shot_first",
        "know_expanded",
        "is_expanded_fan",
        "is_startrek_fan",
        "gender",
        "age",
        "income",
        "education",
        "region"
        )

names(clean_df) <- new_names

# Convert all ranking columns from character/text to numeric values
# lapply() applies as.numeric() to each column in the list
# This allows us to perform mathematical operations on rankings later
rank_cols <- paste0("starwars_ep_", 1:6)
clean_df[rank_cols] <- lapply(clean_df[rank_cols], as.numeric)

rank_long <- clean_df |>
  select(respondent_id, all_of(rank_cols)) |>
  pivot_longer(
    cols = all_of(rank_cols),
    names_to = "film",
    values_to = "rank"
  ) |>
  mutate(
    rank = as.numeric(rank)
  )

# Scoring the films
# Convert rankings (1-6 scale) into points where better rankings = more points
rank_scores <- rank_long |>
  filter(!is.na(rank)) |>            
  mutate(points = 7 - rank) |>       
  group_by(film) |>
  summarise(
    total_points = sum(points),
    mean_rank    = mean(rank)
  ) |>
  arrange(desc(total_points))          

# Rank the characters
# Make a list of column from Han Solo 10th column to Yoda 15th column
chara_cols <- names(clean_df)[16:29]

# Transform character data from wide to long format
chara_favor <- clean_df |>
  select(respondent_id, all_of(chara_cols)) |>
  pivot_longer(
    cols = all_of(chara_cols),
    names_to = "character",
    values_to = "favor"
  ) |>
  filter(!is.na(favor)) |>
  group_by(character, favor) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(character) |>
  mutate(prop = round((n / sum(n)),2)
  )

# Arrange the favorability ratings in a logical order
chara_favor <- chara_favor |>
  mutate(
    favor = factor(
      favor,
      levels = c(
        "Very favorably",
        "Somewhat favorably",
        "Neither favorably nor unfavorably (neutral)",
        "Somewhat unfavorably",
        "Very unfavorably",
        "Unfamiliar (N/A)"
      )
    )
  ) |>
  arrange(character, favor)

# Create a dataframe containing poster information for each film
# We use tibble::tribble() for cleaner, more readable table creation (manual row-by-row entry)
poster_df <- tibble::tribble(
  ~film_id,          ~film_name,                                          ~ranking, ~points, ~path,
  "starwars_ep_5",   "Star Wars Episode V The Empire Strikes Back (1980)",  "1st",    3751,   "Star Wars Episode V The Empire Strikes Back.png",
  "starwars_ep_6",   "Star Wars Episode VI Return of The Jedi (1983)",      "2nd",    3304,   "Star Wars Episode VI Return of The Jedi.png",
  "starwars_ep_4",   "Star Wars Episode IV A New Hope (1977)",              "3rd",    3116,   "Star Wars Episode IV a New Hope.png",
  "starwars_ep_1",   "Star Wars Episode I The Phantom Menace (1999)",       "4th",    2728,   "Star Wars Episode I The Phantom Menace.png",
  "starwars_ep_2",   "Star Wars Episode II Attack of The Clones (2002)",    "5th",    2435,   "Star Wars Episode II Attack of The Clones.png",
  "starwars_ep_3",   "Star Wars Episode III Revenge of The Sith (2005)",    "6th",    2220,   "Star Wars Episode III Revenge of The Sith.png"
)

poster_df <- poster_df |>
  mutate(
    film_name_display = c(
      "Star Wars Episode V\nThe Empire Strikes Back\n(1980)",
      "Star Wars Episode VI\nReturn of The Jedi\n(1983)",
      "Star Wars Episode IV\nA New Hope\n(1977)",
      "Star Wars Episode I\nThe Phantom Menace\n(1999)",
      "Star Wars Episode II\nAttack of The Clones\n(2002)",
      "Star Wars Episode III\nRevenge of The Sith\n(2005)"
    )
  )

# Graph data -------------------------------------------------------------
# Create shared theme
common_theme <- function(include_legend = FALSE) {
  list(
    theme_minimal(base_family = body_font),
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_textbox_simple(
        family = title_font, 
        face = "bold", 
        size = 96,
        color = "#f08d01ff",
        width = unit(1, "npc"),
        padding = margin(0, 0, 5, 0),
        margin = margin(10, 0, 20, 0)
      ),
      plot.subtitle = element_textbox_simple(
        family = body_font,
        size = 36,
        lineheight = 1.4,
        padding = margin(0, 0, 0, 0),
        margin = margin(0, 0, 20, 0)
      ),
      plot.caption = element_text(
        hjust = 1, 
        size = 24, 
        color = "grey40", 
        margin = margin(t = 10, b = 20)
      ),
      plot.margin = margin(10, 40, 10, 40),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      panel.background = element_rect(fill = bg_col, color = bg_col),
      # Axis
      axis.text = element_text(color = "grey40"),
      axis.title.x = element_text(size = 20, margin = margin(t = 20)),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 20, margin = margin(t = 10)),
      axis.text.y = element_text(size = 20, margin = margin(r = 3), face = "bold"),
    ),
    if(include_legend) {
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.margin = margin(t = 0, b = 20, l = 0, r = 0),
        legend.key.spacing.x = unit(5, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_text(size = 18, face = "bold",color = "grey40", margin = margin(r = 30)),
        legend.text = element_text(size = 18)
      )
    }
  )
}

# Plot 1: Films ranked
make_poster_panel <- function(film_name_display, ranking, points, poster_path) {
  poster_img <- magick::image_read(poster_path)
  
  ggdraw(xlim = c(0, 334), ylim = c(0, 800)) +
    draw_image(poster_img, x = 0, y = 300, width = 334, height = 500) +
    draw_label(ranking, x = 167, y = 260, size = 14, fontface = "plain", hjust = 0.5, color = "grey40") +
    draw_label(film_name_display, x = 167, y = 180, size = 16, fontface = "bold", hjust = 0.5, color = "grey40") +
    draw_label(paste0(points, " points"), x = 167, y = 100, size = 12, fontface = "plain", hjust = 0.5, color = "grey40")
}

all_panels <- pmap(
  list(
    film_name = poster_df$film_name_display,
    ranking = poster_df$ranking,
    points = poster_df$points,
    poster_path = poster_df$path
  ),
  make_poster_panel
)

rank_starwars_plot <- cowplot::plot_grid(
  plotlist = all_panels,
  nrow = 1,
  rel_widths = c(1, 1, 1, 1, 1, 1)
)

rank_plot <- ggplot() +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 25, b = 0, l = 25),
    plot.background = element_rect(fill = bg_col, color = bg_col),
  ) +
  cowplot::draw_plot(rank_starwars_plot, x = 0, y = 0, width = 1, height = 1)

# Plot 2: Characters rated
chara_favor_display <- chara_favor |>
  mutate(
    character = str_to_title(str_replace_all(character, "_", " ")),
    favor = case_when(
      favor == "Neither favorably nor unfavorably (neutral)" ~ "Neutral",
      TRUE ~ favor
    )
  )

favor_plot <- ggplot(
  chara_favor_display, 
  aes(
    x = fct_reorder(character, prop, .fun = max), 
    y = prop, 
    fill = factor(favor, levels = c(
      "Unfamiliar (N/A)",
      "Very unfavorably",
      "Somewhat unfavorably",
      "Neutral",
      "Somewhat favorably",
      "Very favorably"
    ))
  )
) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(), name = "Percentage") +
  scale_fill_manual(
    name = "Rating",
    values = c(
      "Very favorably" = "#282AD8",
      "Somewhat favorably" = "#3EA0FF",
      "Neutral" = "#faffc5ff",
      "Somewhat unfavorably" = "#FFE099",
      "Very unfavorably" = "#F76D5E",
      "Unfamiliar (N/A)" = "#373737"
    )
  ) +
  labs(y = "Percentage") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  common_theme(include_legend = TRUE)

# Plot 3 : header & footer
header <- ggplot() +
  labs(
    title = "Star Wars: Ranked & Rated",
    subtitle = "Fan-voted episode rankings meet character favorability. Find out which films topped the charts and which characters won hearts."
  ) +
  theme_void() +
  common_theme(include_legend = FALSE)

footer <- ggplot() +
  labs(caption = "Source: Star Wars (TidytuesdayR) · Graphic: Bayu Prahara") +
  theme_void() +
  common_theme(include_legend = FALSE)

# Combine the plots
final_combined <- cowplot::plot_grid(
  header,
  rank_plot,
  favor_plot,
  footer,
  nrow = 4,
  rel_heights = c(0.5, 1, 1.5, 0.25)
)