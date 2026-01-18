# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(showtext)
library(tidytuesdayR)
library(ggtext)

# Load fonts --------------------------------------------------------------

font_add_google("Gravitas One")
font_add_google("Fjalla One")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9fffeff"
text_col    <- "#181D25"
body_font   <- "Fjalla One"
title_font  <- "Gravitas One"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-07-10")
df <- tt_data$week15_beers
beer_styles <- c("ale", "ipa", "lager", "porter", "stout", "kolsch", "bock", "cider")

# Data wrangling ----------------------------------------------------------

sequential_edges <- df |>
            mutate(name_clean = tolower(name)) |>
            mutate(
                name_clean = str_replace_all(name_clean, "india(n)? pale ale|i\\.p\\.a\\.?|india pale", "ipa"),
                name_clean = str_remove_all(name_clean, "[#()']")
            ) |>
            unnest_tokens(word, name_clean) |>
            filter(!word %in% c("and", "the", "a", "an", "of", "in", "to", "amp")) |>
            filter(!str_detect(word, "^[0-9]")) |>
            group_by(name) |>
            mutate(word_next = lead(word)) |>
            ungroup() |>
            filter(!is.na(word_next)) |>
            filter(word_next %in% beer_styles) |>
            select(word1 = word, word2 = word_next) |>
            count(word1, word2, sort = TRUE) |>
            filter(n >= 10)

graph_sequential <- graph_from_data_frame(sequential_edges, directed = TRUE)

# Plot data  -------------------------------------------------------------
set.seed(42)
plt <- ggraph(graph_sequential, layout = "sugiyama") +
    geom_edge_link(aes(width = n, edge_colour = n),
        arrow = arrow(length = unit(5, "mm"), type = "closed"),
        alpha = 0.8) +
    geom_node_point(aes(size = degree(graph_sequential, mode = "all"),
        fill = ifelse(name %in% beer_styles, "Style", "Descriptor")),
        shape = 21, color = "white", stroke = 3, alpha = 0.95) +
    # Blue Label
    geom_node_text(data = . %>% filter(!name %in% beer_styles),
        aes(label = name),
        family = body_font, size = 5, fontface = "bold",
        repel = TRUE, bg.colour = bg_col, bg.r = 0.1,
        vjust = -1)  +
    # Red label
    geom_node_text(data = . %>% filter(name %in% beer_styles),
        aes(label = name),
        family = body_font, size = 5, fontface = "bold",
        color = "white",
        repel = FALSE,
        nudge_y = 0) +  
    scale_edge_width(range = c(2, 8), guide = "none") +
    scale_edge_colour_gradient(low = "#3498db", high = "#e74c3c", guide = "none") +
    scale_size_continuous(range = c(10, 22), guide = "none") +
    scale_fill_manual(values = c("Descriptor" = "#3498db", "Style" = "#e74c3c"), guide = "none") +
    theme_graph(background = bg_col) +
    labs(
        title = "Exploring Ale & India Pale Ale (IPA) Beer Name Patterns",
        subtitle = "From 2411 beer names, these are the most common word combinations ending with 'ale' or 'ipa'. Thicker lines meaning more frequent combinations.",
        caption = "Source: Beers (TidyTuesdayR) · Graphic: Bayu Prahara"
    ) +
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
            margin = margin(2, 0, 10, 0)
        ),
        plot.caption = element_text(
            hjust = 1, 
            size = 10, 
            color = "grey40", 
            margin = margin(t = 25)
        ),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )

print(plt)

