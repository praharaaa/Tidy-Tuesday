# TidyTuesday 2018-07-23: Antarctic Sea Ice Concentration (2025)

## Overview
This orthographic animation shows daily sea ice concentration changes around Antarctica from January 1, 2025 to December 31, 2025. White areas represent high ice coverage (up to 100%), fading to transparent over open ocean. The polar projection centers on the South Pole, highlighting seasonal expansion and retreat patterns.

## Key Findings
- **Seasonal cycle**: Ice coverage peaks mid-winter (August-September) as expected, then retreats sharply in summer (February-March).
- **Daily variability**: Smooth transitions reveal weather-driven fluctuations within the broader seasonal trend.

## Data Source
NSIDC-0803 AMSR2 Southern Hemisphere Sea Ice Concentration [v2.0](https://nsidc.org/data/nsidc-0803/versions/2)  
**Note**: Requires free NSIDC login/signup for full dataset download. There is no data in the database for December 13, 2025

## Tools
R (tidyverse, terra, sf, rnaturalearth, gifski, showtext, ggtext)

## Visualization
![Antarctic Sea Ice Animation](sea_ice_animation.gif)
