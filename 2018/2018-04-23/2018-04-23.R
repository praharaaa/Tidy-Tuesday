# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(ggrepel)
library(ggtext)
library(scales)

# Load data ---------------------------------------------------------------

tt_data <- tt_load("2018-04-23")
df <- tt_data$week4_australian_salary |>
    select(c(occupation:average_taxable_income))

# Load fonts --------------------------------------------------------------

font_add_google("Oswald", "oswald")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "oswald"

# Data wrangling ----------------------------------------------------------

AU_salary <- df |>
  pivot_wider(
    names_from = gender,
    values_from = c(individuals, average_taxable_income),
    values_fn = list(
      individuals = sum,
      average_taxable_income = mean
    ),
    names_glue = "{.value}_{gender}"
  ) |>
  mutate(
    average_taxable_income_Total = round(((average_taxable_income_Male + average_taxable_income_Female)/2),0),
    total_individual = individuals_Female + individuals_Male,
    ratio_FM = case_when(
      individuals_Female == 0 ~ 2.5,
      individuals_Male == 0 ~ -2.5,
      TRUE ~ log10(individuals_Male / individuals_Female)
    ),
    income_gap = average_taxable_income_Male - average_taxable_income_Female
  )

# Filter and Highligthed data --------------------------------------------

highlight_jobs <- c(
  # Medical / High Income
  "Neurosurgeon",
  "Vascular surgeon",
  "Cardiothoracic surgeon",
  "Anaesthetist",
  "Judge – law",
  
  # Mid-High Income with notable gap
  "Gynaecologist; Obstetrician",
  "Dermatologist",
  
  # Corporate / Finance
  "Company secretary – corporate governance",
  "Securities and finance dealer",
  "Mining production manager",
  
  # Care / Education (female-dominated)
  "Nursing clinical director",
  "Child care centre director",
  "Midwife",
  
  # Low Income / Domestic (female-dominated)
  "Housekeeper – domestic",
  "Legal secretary General clerical workers",

  # Low Income (Equal)
  "Vegetable picker",
  
  # Trades (male-dominated)
  "Fitter",
  "Bricklayer",
  "Cab driver; Taxi driver",
  "Trolley collector"
)

AU_salary <- AU_salary |>
  mutate(
    is_highlighted = occupation %in% highlight_jobs
  )

AU_salary_label <- AU_salary |>
  filter(occupation %in% highlight_jobs)

AU_salary_label <- AU_salary_label |>
  mutate(
    income_gap_scaled = rescale(abs(income_gap), to = c(1, 6)),
    occupation_wrap = case_when(
      nchar(occupation) > 20 ~ str_wrap(occupation, 15),
      TRUE                   ~ occupation
    )
  )

# Graph data -------------------------------------------------------------
plt <- ggplot(AU_salary, aes(x = ratio_FM, y = average_taxable_income_Total, size = abs(income_gap))
    ) +
    geom_point(
        color = "#0085d8ff",
        alpha = 0.1
    ) +
    geom_point(
        data = AU_salary |> filter(is_highlighted),
        color = "#0085d8ff",
        alpha = 0.4, 
        stroke = 1.5,
        fill = "#004a78ff"
    ) +
    geom_vline(
        xintercept = c(-2, -1, 0, 1, 2, 3),
        color = "#0085d8ff",
        alpha = 0.5,
        linewidth = 0.5,
        linetype = "dashed"
    ) +
    scale_size_area(
        name = "Income Gap",
        max_size = 10,
        labels = label_number(scale_cut = cut_short_scale(), digits = 2),
        guide = guide_legend(direction = "horizontal", nrow = 1)
    ) +
    scale_y_continuous(
        name   = "Average Taxable Income (in AUD)",
        limits = c(-10000, 500000),
        breaks = c(0, 100000, 200000, 300000, 400000, 500000),
        labels = label_number(
                 scale_cut = cut_short_scale(),
                 digits = 3),
    ) +
    scale_x_continuous(
        name   = "Ratio F/M",
        breaks = c(-2, -1, 0, 1, 2, 3),
        labels = c(
            "-2" = "100x more\nfemales",
            "-1" = "10x more\nfemales",
            "0"  = "Equal",
            "1"  = "10x more\nmales",
            "2"  = "100x more\nmales",
            "3"  = "1000x more \nmales"
            )
    ) +
    labs(
        title    = "Equal Numbers, Unequal Pay: Gender and Income in Australia",
        subtitle = "Occupations with more men tend to earn more. Larger dots show wider gender pay gaps within occupations, revealing how gender composition correlates with both income level and wage inequality.",
        caption  = "Source: Australian Salary (TidytuesdayR) · Graphic: Bayu Prahara"
    ) +
    geom_text_repel(
        data = AU_salary_label,
        aes(label = occupation_wrap,
            point.size = abs(income_gap_scaled)),
        family = body_font,
        size = 3.2,
        color = text_col,
        point.padding = 0.3,              
        box.padding = 0.5,
        max.overlaps = Inf,
        min.segment.length = 0,
        max.time = 1, max.iter = 1e5,
        segment.color = NA,
        force = 2,
        hjust = 0.5,
        direction = "both"
    ) +
    theme_minimal(
        base_family = body_font
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
            margin = margin(0, 0, 20, 0)
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
        legend.margin = margin(t = -20, b = 0, l = -46, r = 0),
        legend.box.margin = margin(b = 20),
        legend.spacing.x = unit(0.5, "cm"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        # Axis
        axis.text = element_text(color = "grey40"),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        axis.text.y = element_text(size = 10, margin = margin(r = 8)),
        axis.line.x = element_line(color = "grey40", size = 0.5),
        # Plot
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col)
    )
print(plt)

# Misalnya plot kamu bernama plt
ggsave(
  filename = "C:/Users/acer/Documents/Tidy Tuesday/2018/2018-04-23/income_gap_in_AU.png",
  plot = plt,
  width = 3000,   # dalam pixel
  height = 3500,  # dalam pixel
  units = "px",
  dpi = 300       # resolusi (dots per inch)
)
