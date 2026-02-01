# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggtext)
library(scales)
library(ggrepel)
library(ggbeeswarm)

# Load fonts --------------------------------------------------------------

font_add_google("Kalam")
font_add_google("Shrikhand")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#fff9f9ff"
text_col    <- "#181D25"
body_font   <- "Kalam"
title_font  <- "Shrikhand"

# Load data ---------------------------------------------------------------

tt_data <-tt_load("2018-09-04")
df <- tt_data$fastfood_calories

# Data wrangling ----------------------------------------------------------

df_categorized <- df |> select(restaurant, item, calories) |>
  mutate(
    item_lower = tolower(item),
    category = case_when(
      # Burger
      str_detect(item_lower, "burger") ~ "Burger",
      str_detect(item_lower, "whopper|king") ~ "Burger",  # Burger King signature
      
      # Mexican
      str_detect(item_lower, "taco") ~ "Tacos & Wraps",
      str_detect(item_lower, "burrito") ~ "Tacos & Wraps",
      str_detect(item_lower, "quesadilla|quesarito|quesalupa") ~ "Tacos & Wraps",
      str_detect(item_lower, "chalupa|gordita|nachos") ~ "Tacos & Wraps",
      str_detect(item_lower, "wrap") ~ "Tacos & Wraps",
      
      # Salad
      str_detect(item_lower, "salad") ~ "Salad",
      
      # Fried Chicken - nuggets, tenders, strips
      str_detect(item_lower, "nugget|mcnugget") ~ "Fried Chicken",
      str_detect(item_lower, "tender|strip") ~ "Fried Chicken",
      str_detect(item_lower, "popcorn chicken") ~ "Fried Chicken",
      str_detect(item_lower, "chicken fries") ~ "Fried Chicken",
      
      # Sandwich
      str_detect(item_lower, "sandwich") ~ "Sandwich",
      str_detect(item_lower, "sub") ~ "Sandwich",  # Subway

      # Hot Dog
      str_detect(item_lower, "dog|coney") ~ "Hot Dog",
      
      # Default
      TRUE ~ "Other"
    )
  )

df_categorized |> count(category, sort = TRUE)

df_categorized |> 
  filter(category == "Other") |> 
  distinct(item) |> 
  arrange(item) |> view()

# Final result of re-checking the "Other" category
df_c <- read.csv("fastfood_categories.csv") |> 
        select(restaurant, item, calories, category) |> 
        filter(calories <= 1500) |> 
        mutate(color_group = ifelse(category %in% c("Burger", "Sandwich", "Tacos & Wraps"), category, "Other"))

highlighted <- c(
  # Highest to lowest calories
  # Arbys
  "Triple Decker Sandwich", "Turkey 'n Cheese Slider",

  # Burger King
  "Rodeo King", "Hamburger",

  # Chick Fil-A
  "Spicy Deluxe", "Chick-n-Slider", 

  # Dairy Queen
  "1/2 lb. FlameThrower® GrillBurger", "Barbecue Pork Sandwich", 

  # McDonalds
  "Double Bacon Smokehouse Burger", "Hamburger",

  # Sonic
  "Garlic Parmesan Dunked Ultimate Chicken Sandwich", "Ultimate Chicken Club", 

  # Subway
  "Footlong Big Hot Pastrami", "Kids Mini Sub Veggie Delite",

  # Taco Bell
  "XXL Grilled Stuft Burrito - Beef", "Fresco Crunchy Taco"
)

restaurant_label <- c( 
  "Arbys" = "Arby's", 
  "Burger King" = "Burger King", 
  "Chick Fil-A" = "Chick Fil-A", 
  "Dairy Queen" = "Dairy Queen", 
  "Mcdonalds" = "McDonald's", 
  "Sonic" = "Sonic", 
  "Subway" = "Subway", 
  "Taco Bell" = "Taco Bell" )

df_c <- df_c |> mutate(is_highlighted = item %in% highlighted)

# Graph data --------------------------------------------------------------

p <- ggplot(df_c, aes(x = calories, y = "")) +
        geom_beeswarm(cex = 5, size = 5, aes(color = color_group), alpha = 0.3
        ) + 
        geom_beeswarm(data = df_c |> filter(is_highlighted), cex = 5, size = 5, aes(color = color_group), alpha = 0.9
        ) + 
        facet_wrap(~restaurant, nrow = 8, ncol = 1, labeller = as_labeller(restaurant_label)
        ) +
        geom_text_repel(
        data = df_c |> filter(is_highlighted),
        aes(label = str_wrap(item, width = 15)),
        size = 3, fontface = "bold", color = "black",
        box.padding = 0.3, point.padding = 0.2, max.overlaps = 20, 
        hjust = 0.5,
        vjust = -3,       
        # Add connecting lines
        segment.color = "grey50",
        segment.size  = 0.5,
        segment.alpha = 0.7,
        min.segment.length = 0      
        ) +
        geom_vline(xintercept = 500, linetype = "dashed", color = "grey40") +
        geom_vline(xintercept = 1000, linetype = "dashed", color = "grey40") +
        scale_color_manual(values = c(
            "Burger" = "#d32f2f", 
            "Sandwich" = "#2c728e", 
            "Tacos & Wraps" = "#882c8e", 
            "Other" = "grey80")) +
        labs(
            title = "Fast Food Calories: Burgers vs Sandwiches vs Tacos and Wraps",
            subtitle = "Highlighting <span style='color:#d32f2f;'>**burgers**</span>, <span style='color:#2c728e;'>**sandwiches**</span>, and <span style='color:#882c8e;'>**tacos & wraps**</span> (100-1000+ calories) grouped by restaurant chain",
            x = "Calories",            
            caption  = "Fastfood Calories (TidyTuesdayR) · Graphic: Bayu Prahara"
        ) +
        theme_minimal(base_family = body_font)+
        theme(
        # Title, subtitle, and caption
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(
            family = title_font, 
            face = "bold", 
            size = 24,
            color = "#d32f2f",
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
        strip.text = element_text(
            size = 14, 
            face = "bold", 
            hjust = 0, 
            color = "grey40", 
            margin = margin(b = 3)
        ),
        # Legend
        legend.position = "none",
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(margin = margin(r = 8), color = "grey40"),
        axis.text.x = element_text(size = 10, margin = margin(t = 20), face = "bold", color = "grey40"),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        axis.line.x = element_line(color = "grey40", linewidth = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col),
        panel.spacing.y = unit(0.5, "cm"),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),   
        )
print(p)