# Data Prep File
# UNESCO Data:https://data.unesco.org/explore/dataset/whc001/export/?sort=date_inscribed
# Non-UNESCO Data: https://mapcruzin.com/download-shapefile/

library(tidyverse)
library(sf)

# Reads UNESCO shapfile and then cleans variables
UNESCO_raw <- read_sf("whc001/whc001.shp")

oceania_countries_pattern <- "Australia|New Zealand|Fiji|Papua New Guinea|Solomon Islands|Vanuatu|Samoa|Tonga|Palau"

UNESCO_sites <- UNESCO_raw %>%
  select(Name_EN, Category, Date_inscri, States_Name, Region, Area_hectar, geometry) %>%
  rename(name = Name_EN, category = Category, year_inscribed = Date_inscri, country = States_Name, region = Region, area_hectares = Area_hectar) %>%
  mutate(year_inscribed = as.numeric(year_inscribed),
    region_group = case_when(
      region == "Europe and North America" ~ "Europe/North America",
      region == "Africa" ~ "Africa",
      region == "Latin America and the Caribbean" ~ "South America",
      region == "Asia and the Pacific" & str_detect(country, oceania_countries_pattern) ~ "Oceania",
      region %in% c("Asia and the Pacific", "Arab States") ~ "Asia/Middle East",
      TRUE ~ region
    ),
    year_established = year_inscribed,
    name_length = str_count(name, "\\w+")
  )

# Reads the non-UNESCO points for each region

africa_points <- read_sf("africa-points-shape/points.shp") %>%
  select(osm_id, name, type, geometry) %>%
  filter(type %in% c("museum", "archaeological_site", "monument", "memorial", "ruins", "attraction")) %>%
  mutate(continent = "Africa")

asia_points <- read_sf("asia-points-shape/points.shp") %>%
  select(osm_id, name, type, geometry) %>%
  filter(type %in% c("museum", "archaeological_site", "monument", "memorial", "ruins", "attraction")) %>%
  mutate(continent = "Asia/Middle East")

oceania_points <- read_sf("aus-oceania-points-shape/points.shp") %>%
  select(osm_id, name, type, geometry) %>%
  filter(type %in% c("museum", "archaeological_site", "monument", "memorial", "ruins", "attraction")) %>%
  mutate(continent = "Oceania")

south_america_points <- read_sf("so-america-points-shape/points.shp") %>%
  select(osm_id, name, type, geometry) %>%
  filter(type %in% c("museum", "archaeological_site", "monument", "memorial", "ruins", "attraction")) %>%
  mutate(continent = "South America")

nonUNESCO_sites <- bind_rows(africa_points, asia_points, oceania_points, south_america_points) %>%
  mutate(region_group = continent, year_established = NA_real_, name_length = str_count(name, "\\w+"))

UNESCO_sites <- UNESCO_sites %>%
  mutate(is_unesco = "UNESCO")

nonUNESCO_sites <- nonUNESCO_sites %>%
  mutate(is_unesco = "Non-UNESCO")

# Combines UNESCO and non-UNESCO and adds lon/lat
coords_unesco <- st_coordinates(UNESCO_sites)

UNESCO_sites <- UNESCO_sites %>%
  mutate(lon = coords_unesco[, 1], lat = coords_unesco[, 2])

coords_non <- st_coordinates(nonUNESCO_sites)

nonUNESCO_sites <- nonUNESCO_sites %>%
  mutate(lon = coords_non[, 1], lat = coords_non[, 2])

all_sites_df <- bind_rows(st_drop_geometry(UNESCO_sites), st_drop_geometry(nonUNESCO_sites))

save(UNESCO_sites, nonUNESCO_sites, all_sites_df, file = "STA404_Project2_data.RData")
