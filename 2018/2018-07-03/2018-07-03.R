# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(WDI)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Archivo Black")
font_add_google("Montserrat")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#f9f9f9ff"
text_col    <- "#181D25"
body_font   <- "Montserrat"
title_font  <- "Archivo Black"

# Load data ---------------------------------------------------------------

df <- WDI(country = c("KH","RW","BA","LB","ID","BD", "PS"), 
                   indicator = "SP.DYN.LE00.IN", 
                   start = 1960, 
                   end = 2023, 
                   extra = FALSE)

# Data wrangling ----------------------------------------------------------

df <- df |> rename(life_expectancy = SP.DYN.LE00.IN)

country_labels <- c(
  "Cambodia" = "Cambodia\nKhmer Rouge Genocide (1975-1979)",
  "Rwanda" = "Rwanda\nGenocide Against the Tutsi (1994)",
  "Bosnia and Herzegovina" = "Bosnia and Herzegovina\nBosnian War (1992-1995)",
  "Lebanon" = "Lebanon\nCivil War (1975-1990)",
  "Indonesia" = "Indonesia\nIndonesian Mass Killings (1965-1966)",
  "Bangladesh" = "Bangladesh\nLiberation War (1971-1972)",
  "West Bank and Gaza" = "West Bank and Gaza (Palestine)\nIsrael-Palestine Conflicts (1948-present)"
)

crisis_years <- tibble(
    country = c(
        rep("Bangladesh", 1),
        rep("Bosnia and Herzegovina", 4), 
        rep("Indonesia", 2),              
        rep("Cambodia", 10),
        rep("Lebanon", 16),
        rep("West Bank and Gaza", 34),
        rep("Rwanda", 1)
        ),
    year = c(
        1971,
        1992, 1993, 1994, 1995,
        1965, 1966,
        1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
        1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
        1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
        1994
        ), 
    severity = c(
        "extreme",
        rep("extreme", 4),
        rep("extreme", 2),
        rep("extreme", 10),
        rep("extreme", 16),
        rep("extreme", 4),   # Palestine 1990-1993 (First Intifada)
        rep("moderate", 6),  # Palestine 1994-1999
        rep("extreme", 6),   # Palestine 2000-2005 (Second Intifada)
        rep("moderate", 8),  # Palestine 2006-2013
        "extreme",           # Palestine 2014
        rep("moderate", 5),  # Palestine 2015-2019
        rep("extreme", 2),   # Palestine 2020, 2021
        "moderate",          # Palestine 2022
        "extreme",           # Palestine 2023
        "extreme"            # Rwanda 1994
    )
)

df <- df |> left_join(crisis_years, by = c("country", "year")) |>
            mutate(severity = replace_na(severity, "normal")) |>       
            drop_na(life_expectancy) |>
            arrange(country, year)

# Graph data -------------------------------------------------------------

plt <- ggplot(df, aes(x = year, y = life_expectancy, fill = severity)) +
        geom_col(width = 0.8) +
        facet_wrap(~country, ncol = 1, scales = "fixed", axes = "all", labeller = as_labeller(country_labels)) +
        scale_x_continuous(
            breaks = seq(1960, 2023, by = 10),
            labels = seq(1960, 2023, by = 10),
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            limits = c(0, NA)
        ) +
        scale_fill_manual(
            values = c(
                "extreme" = "#d32f2f",
                "moderate" = "#e29f9fff",
                "normal" = "#bfbfbfff"
            )
        ) +
        labs(
            title = "Life Expectancy During Genocides & Wars",
            subtitle = "How genocides and armed conflicts affected life expectancy in six countries. Red bars mark documented crisis periods. Pink represents ongoing low-intensity conflict.",
            x = "Year",
            y = "Life Expectancy (years)",
            caption  = "Global Life Expectancy (Worldbank) · Graphic: Bayu Prahara"
        ) +
        theme_minimal(base_family = body_font)+
        theme(
        #Title, subtitle, and caption
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
        axis.title.y = element_text(margin = margin(r = 8), color = "grey40"),
        axis.title.x = element_blank(),
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