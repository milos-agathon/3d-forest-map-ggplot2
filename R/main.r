#############################################
# 3D forest map with R
# Milos Popovic 2023/04/25
#############################################
setwd() # please set path to your working directory

install.packages("remotes")
remotes::install_github("tylermorganwall/rayshader")
remotes::install_github("tylermorganwall/rayrender")
remotes::install_github("dmurdoch/rgl")

# 0. LOAD PACKAGES
#-----------------

# libraries we need

libs <- c(
    "tidyverse", "terra", "giscoR",
    "sf", "rayshader"
)

# install missing libraries

installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)){
    install.packages(libs[!installed_libraries])
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_country_borders <- function() {
    country_borders <- giscoR::gisco_get_countries(
        resolution = "10",
        country = "SE"
    )
    return(country_borders)
}

country_borders <- get_country_borders() |>
    sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(country_borders))

# 2. GET FOREST COVER
#--------------------

"https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E000N60/E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"

start_url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
var_url <- c("E000N60/E000N60", "E000N80/E000N80", "E020N80/E020N80")
end <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"

urls <- paste0(start_url, var_url, end)

for (url in urls) {
    download.file(url, destfile = basename(url), mode = "wb")
}

raster_files <- list.files(
    path = getwd(),
    pattern = end,
    full.names = T
)

raster_files

# 3. LOAD FOREST COVER
#---------------------

forest_cover <- lapply(raster_files, terra::rast)
forest_cover_mosaic <- do.call(terra::mosaic, forest_cover)

plot(forest_cover_mosaic)
plot(sf::st_geometry(country_borders), add = T)

# 4. CROP RASTER
#---------------
get_forest_cover_cropped <- function() {
    country_borders_vect <- terra::vect(
        country_borders
    )
    forest_cover_cropped <- terra::crop(
        forest_cover_mosaic, country_borders_vect,
        snap = "in", mask = T
    )

    return(forest_cover_cropped)
}

forest_cover_cropped <- get_forest_cover_cropped() |>
    terra::aggregate(fact = 2) # please increase if R throws  the memory error below

# 5. RASTER TO DATAFRAME
#-----------------------
forest_cover_df <- forest_cover_cropped |>
    as.data.frame(xy = T)

names(forest_cover_df)
names(forest_cover_df)[3] <- "percent_cover"

# 6. BREAKS
#----------
summary(forest_cover_df$percent_cover)
min_val <- min(forest_cover_df$percent_cover)
max_val <- max(forest_cover_df$percent_cover)
limits <- c(min_val, max_val)

breaks <- seq(from = min_val, to = max_val, by = 20)

# 7. COLORS
#----------

cols <- rev(c("#276604", "#ddb746", "#ffd3af", "#ffeadb"))

texture <- colorRampPalette(cols)(256)

# 8. GGPLOT2
#-----------

p <- ggplot(forest_cover_df) +
    geom_raster(
        aes(
            x = x, y = y, fill = percent_cover
        )) +
    # geom_sf(data = country_borders,
    #     fill = "transparent", color = "black", size = .2) +
    scale_fill_gradientn(
        name = "% of area",
        colours = texture,
        breaks = breaks,
        limits = limits
    ) +
    coord_sf(crs = crsLONGLAT) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.25, units = "mm"),
            keywidth = unit(5, units = "mm"),
            title.position = "top",
            label.position = "bottom",
            nrow = 1,
            byrow = T
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 7, color = "grey10"),
        legend.text = element_text(size = 5, color = "grey10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
    ) +
    labs(
        title = "",
        subtitle = "",
        caption = "",
        x = "",
        y = ""
    )

print(p)

# 9. RAYSHADER
#-------------

w <- ncol(forest_cover_cropped)
h <- nrow(forest_cover_cropped)

rayshader::plot_gg(
    ggobj = p,
    multicore = T,
    width = w / 1000,
    height = h / 1000,
    windowsize = c(800, 800)
)

rgl::close3d()

rayshader::plot_gg(
    ggobj = p,
    multicore = T,
    width = w / 1000,
    height = h / 1000,
    windowsize = c(800, 800),
    offset_edges = T,
    shadow_intensity = .99,
    sunangle = 135,
    phi = 30,
    theta = -30
)

rayshader::render_camera(zoom = .3)
rayshader::render_camera(zoom = .5)
rayshader::render_camera(zoom = .5, phi = 85)
rayshader::render_camera(zoom = .5, phi = 85, theta = 0)

# 10. RENDER
#-------------

rayshader::render_highquality(
    filename = "default.png",
    preview = T,
    width = w * .85,
    height = h * .85,
    parallel = T,
    interactive = F
)

rayshader::render_highquality(
    filename = "take_1.png",
    lightintensity = 750,
    preview = T,
    width = w * .85,
    height = h * .85,
    parallel = T,
    interactive = F
)

rayshader::render_highquality(
    filename = "take_2.png",
    lightintensity = 750,
    lightdirection = 135,
    preview = T,
    width = w * .85,
    height = h * .85,
    parallel = T,
    interactive = F
)

rayshader::render_highquality(
    filename = "take_3.png",
    lightintensity = 750,
    lightdirection = 135,
    lightaltitude = 30,
    preview = T,
    width = w * .85,
    height = h * .85,
    parallel = T,
    interactive = F
)

rayshader::render_highquality(
    filename = "take_4.png",
    lightintensity = c(750, 500),
    lightdirection = c(135, 115),
    lightaltitude = c(30, 45),
    preview = T,
    width = w * .85,
    height = h * .85,
    parallel = T,
    interactive = F
)

# BONUS: ANNOTATE MAP
#--------------------
install.packages("magick")
library(magick)
# load map
map1 <- magick::image_read("default.png")

# Set text color
title_color <- cols[4]
text_color <- "grey20"

# Title

map2 <- magick::image_annotate(
    map1, "Forest cover in 2019",
    font = "Georgia",
    color = alpha(title_color, .5),
    size = 250, gravity = "northwest",
    location = "+100+600"
)
#subtitle
map3 <- magick::image_annotate(
    map2, "SWEDEN",
    font = "Georgia",
    color = title_color,
    size = 500, gravity = "northwest",
    location = "+100+800", weight = 600
)

# caption 1 - author
map4 <- magick::image_annotate(
    map3, "©2023 Milos Popovic (https://milospopovic.net)",
    font = "Georgia",
    color = alpha(text_color, .75),
    size = 100, gravity = "southeast",
    location = "+100+200"
)

# caption 2 - data source

map5 <- magick::image_annotate(
    map4, "Data: Copernicus Global Land Service: Land Cover 100m",
    font = "Georgia",
    color = alpha(text_color, .75),
    size = 100, gravity = "southeast",
    location = "+100+100"
)

magick::image_write(
    map5,
    "default-annotated.png"
)
