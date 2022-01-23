library(pacman)
pacman::p_load(here,
               tidyverse,
               tidytuesdayR, # load tidytuesday data
               patchwork,
               Cairo,
               geojsonio,
               RColorBrewer,
               rgdal,
               broom,
               rgeos,
               mapproj,
               scales)

# load data
tuesdata <- tuesdata <- tidytuesdayR::tt_load(2022, week = 2)
colony <- tuesdata$colony %>%
  filter(year == "2021")

# download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

# load this file. (Note: I stored in a folder called 2022_week2)
spdf <- geojson_read("2022_week2/us_states_hexgrid.geojson.json",  what = "sp")

# bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# 'fortify' the data to make it compatible with ggplot2 (we need a data frame format)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , colony, by=c("id"="state")) 

# prepare binning
spdf_fortified$bin <- cut(spdf_fortified$colony_lost_pct, 
                          breaks=c(0,5,10,15,20,Inf), 
                          labels=c("0-5", "6-10", "11-15", "16-20", "20+"), 
                          include.lowest = TRUE)

# colour palette
my_palette = c("#f5ea92", "#f3e260", "#f5dd29", "#f2d600","#d9b51c")

# plot
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="#754a00", size=4, alpha=0.6) +
  theme_void() +
  
# scales
  scale_fill_manual( 
    values=my_palette,
    na.value="#fdfae5",
    name="   Percentage of bee colonies lost in 2021 (%)", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  
# captions and labels
  labs(title = "To bee or not to bee, that is the question",
       caption = "@danni_scales | source: Bee Informed") +

  
  # theme
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#754a00"),
    plot.background = element_rect(fill = "#faf3c0", color = NA), 
    panel.background = element_rect(fill = "#faf3c0", color = NA), 
    legend.background = element_rect(fill = "#faf3c0", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#754a00", face = "bold", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

ggsave("2022_week2/2022_week2.pdf", width=7, height=5, device = cairo_pdf)