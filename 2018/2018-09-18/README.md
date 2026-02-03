# TidyTuesday: Pilot Hypoxia Risk: PaO₂ Drops Below Safe Levels at Altitude

## Overview
Line chart showing how alveolar oxygen pressure (PAO2) in the lungs decreases with altitude, using selected points from the *Soaring* magazine hypoxia table. A shaded triangle highlights the region between sea level and the summit of Mount Everest to illustrate how much oxygen pressure is lost as altitude increases.

## Key Findings
- Commercial airliners typically cruise around 35,000 to 40,000 feet, but cabin pressurization keeps passengers at an effective altitude near 6,000 to 8,000 feet where healthy people can usually breathe comfortably.
- Above roughly 16,000 to 26,000 feet of effective altitude, alveolar oxygen pressure drops below the usual safe range for clear thinking, so pilots need supplemental oxygen to avoid subtle hypoxia.
- The line shows PAO2 falling from about 104 mmHg at sea level to much lower values near typical flight levels, even though the air still contains 21 percent oxygen.
- The Everest annotation helps non‑pilots relate extreme mountain altitude to the oxygen environment pilots experience at high flight levels.

## Data Source
Alveolar oxygen and carbon dioxide pressure table adapted from Guyton and Hall’s *Textbook of Medical Physiology* as reproduced in the August 2018 issue of *Soaring* magazine (article on hypoxia, hyperventilation, and supplemental oxygen systems).

## Tools
R (tidyverse, ggplot2, TidyTuesdayR, ggtext, scales, showtext, janitor)

## Visualization
![Hypoxia risk across flight levels](pilot.hypoxia.png)
