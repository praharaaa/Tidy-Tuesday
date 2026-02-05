# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(ggrepel)

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

df <- read.csv("C:/Users/acer/Documents/Tidy Tuesday/2018/2018-10-02/us_births_2000-2014.csv")
Sys.setlocale("LC_TIME", "C")

# Data wrangling ----------------------------------------------------------

special_dates_weekdays <- ymd(c(
  "2001-01-01",  # 1 Jan 2001
  "2003-03-03",  # 3 Mar 2003
  "2005-05-05",  # 5 May 2005
  "2006-06-06",  # 6 Jun 2006
  "2008-08-08",  # 8 Agt 2008
  "2009-09-09",  # 9 Sep 2009
  "2011-11-11",  # 11 Nov 2011
  "2012-12-12"   # 12 Dec 2012
))

special_dates_weekends <- ymd(c(
  "2002-02-02",  # Sat
  "2004-04-04",  # Sun
  "2007-07-07",  # Sat
  "2010-10-10"   # Sun
))

df <- df |> 
  mutate(date = make_date(year, month, date_of_month),
         is_sd_weekdays = date %in% special_dates_weekdays,
         is_sd_weekends = date %in% special_dates_weekends,
         category = case_when(
            is_sd_weekdays ~ "Weekday Special",
            is_sd_weekends ~ "Weekend Special",
            TRUE ~ "Normal"),
         label_text = case_when(
            category %in% c("Weekday Special", "Weekend Special") ~ paste(strftime(date, "%d %b %Y"),"\n( n =",scales::comma(births),")"),
            TRUE ~ NA_character_)
        )

# Graph data --------------------------------------------------------------

p <- ggplot(df, aes(x = date, y = births)
    ) +
    geom_point(data = ~ .x |> filter(category == "Normal"),
                aes(color = category, size = category),
                alpha = 0.1
    ) +
    geom_point(data = ~ .x |> filter(category %in% c("Weekday Special", "Weekend Special")),
                aes(color = category, size = category),
                alpha = 1
    ) +
    geom_text_repel(
        data = df |> filter(category %in% c("Weekday Special", "Weekend Special")),
        aes(label = label_text),
        size = 3, fontface = "bold", color = "black",
        box.padding = 0.3, point.padding = 0.2, max.overlaps = 20,
        vjust = 1,       
        # Add connecting lines
        segment.color = "grey50",
        segment.size  = 0.5,
        segment.alpha = 0.7,
        min.segment.length = 0      
    ) +
    scale_size_manual(
        values = c( "Normal" = 1,
                    "Weekday Special" = 3,
                    "Weekend Special" = 3),
        guide = "none"  
    ) +
    scale_color_manual(
        values = c( "Weekday Special" = "blue",
                    "Weekend Special" = "red",
                    "Normal" = "grey50"),
        name = "Category"
    ) +
    guides(color = guide_legend(
        override.aes = list(size = c(3, 3, 3)))
    ) +
    labs(
        x = "Date",
        y = "Births",
        title = "Twin Dates Don't Always Mean More Births",
        subtitle = "US births (2000-2014) on special dates like 01-01-2001 or 12-12-2012 show interesting patterns. Some dates like 04-04-2004 and 06-06-2006 have fewer births because people think they bring bad luck. But 07-07-2007 ('lucky seven') had many births even though it was a weekend. Scheduled C-sections let parents choose good dates and avoid bad ones.",
        caption  = "Dataset: US Birth 2000-2014 (TidytuesdayR) · Graphic: Bayu Prahara"
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
            margin = margin(0, 0, 50, 0)
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
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 8)),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 8)),
        axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        axis.line = element_line(color = alpha("grey70", 0.5)),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )
print(p)