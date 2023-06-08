library(tidyverse)
library(dplyr)
library(mapview)
library(tmaptools)
library(ggplot2)
library(sf)
library(ggsci)
library(tictoc)
library(readxl)
library(tmap)
library(classInt)
library(ggforce)


# Read in nbs df and nbs_agg
nbs_df <- readRDS("data/nbs_df.rds")
nbs_agg <- readRDS("data/nbs_agg.rds")


### get WHO 53 countries
euro_countries <- read_xlsx("data/[LCDE 2024] Country names and groupings.xlsx", skip =1)
euro_countries <- euro_countries %>% 
  select(c("Country name", "European sub-region (UN geoscheme)","WB income group (2023)")) %>% 
  rename("country" = `Country name`,
         "subregion" = `European sub-region (UN geoscheme)`)


# city analysis
nbs_city<- nbs_df %>%
  group_by(location) %>% 
  summarize(nbs_count = n()) 

coords <- geocode_OSM(nbs_city$location)
nbs_city_wgeo <- nbs_city
nbs_city_wgeo$lat <- coords$lat
nbs_city_wgeo$lon <- coords$lon

nbs_city_wgeo <- st_as_sf(nbs_city_wgeo, coords = c("lon", "lat"), crs = 4326)
nbs_city_wgeo <- st_set_geometry(nbs_city_wgeo, st_geometry(nbs_city_wgeo))

#st_write(nbs_city_wgeo, "nbs_city_wgeo.gpkg")


#country level analyses
#add country
nbs_df$country <- sapply(nbs_df$location, function(x) strsplit(x, ", ")[[1]][2])

nbs_df <- nbs_df %>% 
  left_join(euro_countries, by = "country")

nbs_country <- nbs_df %>%
  group_by(country) %>% 
  summarise(nbs_count = n()) %>%
  left_join(nbs_df %>% 
              select(country, subregion), 
            by = "country") %>%
  distinct()




# tile map
custom_colors <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")


p <- ggplot(nbs_agg, aes(x = begin_year, y = solution_type, fill = solution_factors)) +
  geom_tile(aes(width = 1, height = 1), color = "white", size = 0.25) +
  labs(x = "Year", y = "", fill = "Number of NbS Projects") +
  scale_y_discrete(expand = c(0, 0), limits = rev(unique(nbs_agg$solution_type))) +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2020, 5), labels = seq(1995, 2020, 5)) +
  scale_fill_manual(
    values = c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32"),
    na.value = "white"
  ) +
  coord_fixed() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 10),
    axis.text.x = element_text(color = "black", size = 8, vjust = 0.5),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "black", hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = grid::unit(0.4, "cm"),
    legend.key.width = grid::unit(0.4, "cm")
  )

p

plot_width <- 10  # in inches
plot_height <- 10
ggsave("nbs_tileplot1.svg", plot = p, width = plot_width, height = plot_height, units = "in", dpi = 300, device = "svg")




# Themes 
theme_set(theme_minimal(base_family = "Times New Roman", base_size = 10))

## subregion ascending order total projects
count_per_subregion <- nbs_country %>%
  group_by(subregion) %>%
  summarise(count = sum(nbs_count, na.rm =TRUE))

p <- ggplot(nbs_country, aes(x = reorder(subregion, nbs_count), y = nbs_count, fill = subregion)) +
  geom_bar(stat = "identity") +
  scale_fill_lancet() +
  labs(x = "Country", y = "Number of Projects", fill = "Subregion") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p + geom_text(data = count_per_subregion, aes(x = subregion, y = count, label = count), vjust = -0.5) +
  coord_cartesian(ylim = c(0, max(count_per_subregion$count) * 1.2))
