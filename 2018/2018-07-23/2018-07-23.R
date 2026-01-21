# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(terra)
library(sf)
library(rnaturalearth)
library(gifski)

# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col      <- "#292929"
text_col    <- "#ffffff"
body_font   <- "Inter"
title_font  <- "Oswald"

# Load data ---------------------------------------------------------------

# 1. Orthographic projection (South Pole)
ortho <- "+proj=ortho +lat_0=-90 +lon_0=0"

# 2. Land
world <- ne_countries(scale = "medium", returnclass = "sf")
antarctica <- world |> 
        filter(continent == "Antarctica" | name == "Antarctica")
other_countries <- world |> 
        filter(continent != "Antarctica" & name != "Antarctica")

# 3. Ocean circle background
south_pole <- st_sfc(st_point(c(0, -90)), crs = 4326)
ocean_circle <- st_transform(south_pole, crs = ortho) |> 
            st_buffer(dist = 6400000)

# 4. Set locale for English date format -----------------------------------
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Plot data ---------------------------------------------------------------

# 5. Create plot function for single .nc file -----------------------------
make_ice_plot <- function(nc_path) {
    date_str <- str_extract(basename(nc_path), "\\d{8}")
    date_lab <- format(as.Date(date_str, "%Y%m%d"), "%d %B %Y")
    
    ice <- rast(nc_path)
    ice_ortho <- project(ice, ortho, method = "bilinear")
    
    ice_df <- as.data.frame(ice_ortho, xy = TRUE)
    names(ice_df)[3] <- "conc_raw"
    ice_df <- ice_df |>
        mutate(conc = conc_raw / 250)

    ggplot() +
    # Coloring Sea
    geom_sf(data = ocean_circle, fill = "#191919", colour = NA) +
    # Sea ice overlay
    geom_raster(data = ice_df, aes(x = x, y = y, fill = conc)) +
    # Land
    geom_sf(
        data = st_transform(antarctica, crs = ortho),
        fill = "#FFFFFF", 
        color = "white",
        size = 0.2
    ) +
    geom_sf(
        data = st_transform(other_countries, crs = ortho),
        fill = "#404040", 
        color = "white",
        size = 0.2
    ) +
    coord_sf(crs = ortho, expand = FALSE, clip = "off"
    ) +
    annotate(
        "text",
        x = 0, y = 0, 
        label = "1 January 2025",
        size = 6,
        hjust = 0.5,
        vjust = 24, 
        color = "white",
        fontface = "bold"
    ) +
    labs(
        title = "Antarctic Sea Ice: A Year in Motion",
        subtitle = "One year of ice concentration changes around the southern continent",
        caption = "Dataset: NASA · Graphic: Bayu Prahara"
    ) +
    scale_fill_gradient(
        low = "transparent", 
        high = "white",
        na.value = "transparent",
        guide = guide_colorbar(title.position = "top",title.hjust = 0.5),
        name = "Sea ice concentration",
    ) +
    theme_void() +
    theme_minimal(base_family = body_font
    ) +
    theme(
    #Title, subtitle, and caption
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
        family = title_font, 
        face = "bold", 
        size = 28,
        color = "#d2e5ffff",
        width = unit(1, "npc"),
        padding = margin(0, 0, 5, 0),
        margin = margin(2, 0, 4, 0)
    ),
    plot.subtitle = element_textbox_simple(
        family = body_font,
        size = 14,
        color = text_col,
        lineheight = 1.4,
        padding = margin(0, 0, 0, 0),
        margin = margin(8, 0, 40, 0)
    ),
    plot.caption = element_text(
        hjust = 1, 
        size = 10, 
        color = text_col, 
        margin = margin(t = 25)
    ),
    # Legend
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.margin = margin(t = 80, b = 20, l = 0, r = 0),
    legend.key.spacing.x = unit(1, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    legend.key.width = unit(2, "cm"), 
    legend.key.height = unit(0.8, "cm"),
    legend.title = element_text(color = "white", size = 11, face = "bold", margin = margin(b = 10)),
    legend.text  = element_text(color = "white", size = 11),
    # Plot
    plot.margin = margin(40, 40, 40, 40),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    )
}

# 6. Generate PNG frames from all .nc files -------------------------------

nc_dir <- "Antarctica Data"
nc_files <- list.files(
    nc_dir,
    pattern = "NSIDC-0803_SEAICE_AMSR2_S_2025.*\\.nc$",
    full.names = TRUE
    ) |> sort()

dir.create("frames_ice", showWarnings = FALSE)

for (i in seq_along(nc_files)) {
    message("Processing: ", basename(nc_files[i]))
    
    p <- make_ice_plot(nc_files[i])
    
    png_filename <- sprintf("frame_%03d.png", i)
    png(png_filename, width = 2400, height = 3600, res = 300, bg = bg_col)
    print(p)
    dev.off()
}

# 7. Create GIF animation from PNG frames ---------------------------------

folder <- "frames_ice"
frames <- list.files(folder, pattern = "frame_\\d+\\.png", full.names = TRUE)
frames <- sort(frames)
gifski(
    frames,
    gif_file = "sea_ice_animation.gif",
    width = 1200,       # resize to 1200px
    height = 1800,      # resize to 2400px
    delay = 1 / 10      # 10 FPS
)

