# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(ggalluvial)

# Load fonts --------------------------------------------------------------

font_add_google("Bangers")
font_add_google("Kalam")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#f9fcffff"
text_col <- "#181D25"
body_font <- "Kalam"
title_font <- "Bangers"

# Load data ---------------------------------------------------------------

df <- read.csv("week9_comic_characters.csv")
view(df)

# Data wrangling ----------------------------------------------------------

clean_df <- df |>
    mutate(
        sex_grouped   = case_when(
            sex == "Male Characters"    ~ "Male",
            sex == "Female Characters"  ~ "Female",
            TRUE ~ "Others",),
        align_grouped = case_when(
            align == "Bad Characters"   ~ "Bad",
            align == "Good Characters"  ~ "Good"),
        id_grouped    = case_when(
            id == "No Dual Identity"    ~ "No Dual",
            id == "Public Identity"     ~ "Public",
            id == "Secret Identity"     ~ "Secret",
            TRUE ~ "Unknown"
        )
    ) |>
    filter(!is.na(align_grouped), publisher == "Marvel") |>
    group_by(sex_grouped, align_grouped, id_grouped) |>
    summarise(freq = n(), .groups = 'drop') |>
    mutate(
        highlight = sex_grouped == "Male" & align_grouped == "Bad" & id_grouped == "Secret")

total_df <- clean_df |>
    mutate(
        total_freq = sum(freq),
        pct = freq / total_freq
    )

# Graph data -------------------------------------------------------------

plt <- ggplot(clean_df, aes(axis1 = sex_grouped, axis2 = align_grouped, axis3 = id_grouped, y = freq)) +
        geom_alluvium(aes(fill = sex_grouped, alpha = highlight), curve_type = "sigmoid") +
        geom_stratum() +
        geom_text(stat = "stratum",
                aes(label = after_stat(stratum))) +
        scale_x_discrete(limits = c("Sex", "Alignment", "Identity"),
                expand = c(0.05, 0.05)) +
        scale_y_continuous(
            limits = c(0, 12000),
            breaks = seq(0, 12000, 3000)
        ) +
        scale_fill_manual(
        name   = "Sex",
        values = c(
            "Female" = "#8A4FFF",
            "Male"   = "#40BFC1",
            "Others" = "#F28E2B"  
        )
        ) +
        scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.35), guide = "none") +
        labs(
            title    = "Marvel's Character Composition: Gender, Alignment, and Identity Analysis",
            subtitle = "Of 11,356 Marvel characters analyzed, the 'male + bad alignment + secret identity' archetype alone represents 22%, exceeding the combined total of all 8 female character types (18.4%).",
            caption  = "Source: Comic Characters dataset (TidytuesdayR) · Graphic: Bayu Prahara",
        ) +
        guides(fill = guide_legend(title = "Sex")) +
        theme_minimal(base_family = body_font) +
        theme(
        #Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 36,
            color = "#e23636",
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
            size = 12, 
            color = "grey40", 
            margin = margin(t = 20)
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
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
        )
print(plt)