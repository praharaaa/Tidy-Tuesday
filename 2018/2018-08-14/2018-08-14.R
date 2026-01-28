# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(ggrepel)
library(scales)
library(data.table)

# Load fonts --------------------------------------------------------------

font_add_google("Space Grotesk")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#fff9f9ff"
text_col    <- "#181D25"
body_font   <- "Ubuntu"
title_font  <- "Space Grotesk"

# Load data ---------------------------------------------------------------

df <- fread("ht_mapping.csv")

# Data wrangling ----------------------------------------------------------

highlighted <- c(
  # Politics & US Election
  "#trump", "#obama", "#hillary", "#maga", "#trumpforpresident", "#pjnet", "#tcot", "#ccot", "#fakenews",
  "#corruptcongress", "#dirtycongress", "#mustbebanned", "#2016in4words", "#sanjose",

  # Social Issues & Activism
  "#blacklivesmatter", "#blm", "#policebrutality", "#cops", "#police", "#danita", "#dannis",

  # News & Media
  "#breaking", "#politics", "#business", "#health", "#local",

  # Entertainment & Events
  "#nowplaying", "#soundcloud", "#music", "#christmasaftermath", "#sports",

  # International / Religion
  "#erdogan", "#merkel", "#merkelmussbleiben", "#isis", "#islamkills", "#stopislam", "#islam", "#indonesia",

  # Local / Unique Names
  "#foke", "#usa", "#marv", "#vvmar", "#arbt", "#arb", "#usfa", "#barr", "#fellon")

df_ht <-  df |> mutate(is_highlighted = hashtag %in% highlighted)

# Graph data --------------------------------------------------------------

p <- ggplot(df_ht, aes(x = Right, y = Left)) +
    coord_cartesian(clip = "off") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    geom_point(aes(color = is_highlighted, size = total), alpha = 0.4) +
    geom_text_repel(
        data = df_ht |> filter(is_highlighted),
        aes(x = Right, y = Left, label = hashtag),
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
    scale_size_area(
        max_size = 15, 
        breaks = c(100, 1000, 10000),
        name = "Total hashtag usage"
    ) +
    scale_x_log10(    
        limits = c(1, 10000),
        breaks = c(1, 10, 100, 1000, 10000),
    ) +
    scale_y_log10(
        limits = c(1, 10000),
        breaks = c(1, 10, 100, 1000, 10000),
    ) +
    guides(
        size = guide_legend(override.aes = list(color = "#0085d8", fill = "#0085d8", alpha = 0.6)),
        color = "none"
    ) +
    labs(
        x = "Hashtag usage by Right Troll accounts",
        y = "Hashtag usage by Left Troll accounts",
        title    = "Polarized Patterns: Hashtag Usage by Left vs. Right Russian Troll Accounts",
        subtitle = "Comparing how nearly 50,000 hashtags were distributed across the political spectrum in over 3 million tweets from the IRA dataset. Hashtags above the diagonal line were used more by left-leaning accounts, while those below were favored by right-leaning accounts.",
        caption  = "Dataset: Russian Troll Tweets (TidytuesdayR) · Graphic: Bayu Prahara"
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
            margin = margin(0, 0, 10, 0)
        ),
        plot.subtitle = element_textbox_simple(
            family = body_font,
            size = 14,
            lineheight = 1.4,
            padding = margin(0, 0, 0, 0),
            margin = margin(0, 0, 40, 0)
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
