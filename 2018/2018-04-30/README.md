# TidyTuesday: Income and Poverty Across U.S. Counties

## Overview
This visualization explores how county-level poverty rates relate to median household income across the United States in 2015. It uses a bubble chart with a smooth trend line to show how poverty changes along the income distribution.

## Key Findings
- County poverty rates generally decrease as median household income increases.  
- Most counties cluster in the middle-income range (around USD 40K–70K), with poverty rates typically between about 10% and 30%.  
- At higher income levels, additional income is associated with smaller reductions in poverty, so the poverty curve flattens for richer counties.

## Data Source
- American Community Survey (ACS) 2015 county-level estimates as prepared for the [TidyTuesday 2018-04-30 episode](https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-30).  
- Accessed in R with the `tidytuesdayR` package.

## Tools
- R (tidyverse, tidytuesdayR, ggrepel, ggtext, showtext, scales, tigris)  
- ggplot2 for visualization

## Visualization
![ACS Census Data 2015](income_poverty_US.png)
