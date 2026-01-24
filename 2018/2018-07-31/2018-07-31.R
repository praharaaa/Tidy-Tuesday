# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
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

tt_data <-tt_load("2018-07-31")
df <- tt_data$week18_dallas_animals

# Data wrangling ----------------------------------------------------------

top_5_catdog <- df |>
        count(animal_type, animal_breed, name = "n") |>
        filter(animal_type %in% c("CAT", "DOG")) |>
        group_by(animal_type) |>
        slice_max(n, n = 5, with_ties = FALSE) |>
        ungroup()

outcome_5_catdog_plot <- df |> 
        select(animal_type, animal_breed, outcome_type) |>
        count(animal_type, animal_breed, outcome_type) |>
        filter(animal_breed %in% c("DOMESTIC SH", "DOMESTIC MH", "DOMESTIC LH", "SIAMESE", "AMER SH",
                                   "PIT BULL", "CHIHUAHUA SH", "GERM SHEPHERD", "LABRADOR RETR", "CAIRN TERRIER")) |>
        group_by(animal_type, animal_breed) |> 
        mutate(prop_temp = n / sum(n)) |>
        ungroup() |> 
        mutate(
        outcome_group = case_when(
        outcome_type %in% c("ADOPTION", "EUTHANIZED", "TRANSFER", "RETURNED TO OWNER") ~ outcome_type, 
        TRUE ~ "Other"
        ) |> str_to_title(),
        breed_name = animal_breed |> 
        str_to_title() |> 
        str_replace_all("Sh$", "Short Hair") |> 
        str_replace_all("Mh$", "Medium Hair") |> 
        str_replace_all("Lh$", "Long Hair") |> 
        str_replace_all("Retr$", "Retriever")
        ) |>
        group_by(animal_type, breed_name, outcome_group) |>  
        summarise(n = sum(n), prop = round(sum(prop_temp), 2), .groups = "drop") |>
        group_by(animal_type, breed_name) |>
        mutate(total_n = sum(n)) |>
        ungroup() |>
        mutate(
        breed_name = paste0(breed_name, "\n(n = ", total_n, ")"),
        breed_name = factor(breed_name, levels = c(
        "Domestic Short Hair\n(n = 6660)",
        "Domestic Medium Hair\n(n = 549)",
        "Domestic Long Hair\n(n = 171)",
        "Siamese\n(n = 133)",
        "Amer Short Hair\n(n = 96)",
        "Pit Bull\n(n = 5911)",
        "Labrador Retriever\n(n = 3224)",
        "Germ Shepherd\n(n = 3283)",
        "Chihuahua Short Hair\n(n = 3609)",
        "Cairn Terrier\n(n = 736)"
        ))
        )
        
# Graph data -------------------------------------------------------------

p <- ggplot(outcome_5_catdog_plot, aes(
        x = prop, 
        y = breed_name, 
        fill = factor(outcome_group, levels = c(
               "Other", "Transfer", "Returned To Owner", "Adoption","Euthanized")))
        ) +
        geom_col(position = "fill") +
        geom_text(
                aes(label = ifelse(prop >= 0.05, percent(prop, accuracy = 1), "")),
                position = position_fill(vjust = 0.5),
                size = 3.5,
                fontface = "bold",
                color = ifelse(outcome_5_catdog_plot$outcome_group %in% c("Euthanized", "Other"), "white", "black")
        ) +
        facet_wrap(~ animal_type, scales = "free_y", ncol = 1
        ) +
        scale_x_continuous(labels = percent, expand = c(0, 0)
        ) +
        scale_fill_manual(
                name = "Outcome",
                values = c(
                "Euthanized" = "#F76D5E",
                "Adoption" = "#FFE099",
                "Returned To Owner" = "#FAFFC5",
                "Transfer" = "#85c2ffff",
                "Other" = "#282AD8"),
                breaks = c("Euthanized", "Adoption", "Returned To Owner", "Transfer", "Other")
        ) +
        labs(
        x = "Proportion",
        fill = "Outcome",
        title = "What Happens to Shelter Pets After They Arrive?",
        subtitle = "Tracking outcomes for 20,000 animals (cats and dogs combined) at Dallas Animal Shelter during 2016 to 2017. Data shows the top 5 breeds for each species. The category 'Other' includes cases such as died, died on arrival, foster, missing and more.",
        caption = "Dataset: Dallas Animal Shelter 2016-2017 (TidyTuesdayR) · Graphic: Bayu Prahara"
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
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.margin = margin(t = 20, b = 20, l = -100, r = 0),
        legend.key.spacing.x = unit(1, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.width = unit(0.7, "cm"), 
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_text(size = 10, face = "bold", color = "grey40", margin = margin(r = 10)),
        legend.text  = element_text(size = 10),
        strip.text.x  = element_text(face = "bold", size = 16, margin = margin(t = 10, b = 4)),
        strip.text.y  = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t = 10)),
        axis.text.y = element_text(size = 12, margin = margin(r = 3), face = "bold"),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), color = "grey40"),
        axis.title.y = element_blank(),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
        )

print(p)

