library(ggplot2)
library(ggiraph)
library(tidyverse)
library(mapdata)

library(tmap)

# US Census data for state shapes
library(tigris)
#library for shapefile
library(sf)

accident <- read_csv("CrashDataApp/FARS2021NationalCSV/accident.csv")

state <- map_data("state")
county <- map_data("county")

region1 <- "south dakota"

full_state <- subset(state, region==region1)
full_counties <- subset(county, region==region1)

ca_map <- ggplot(data=full_state, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=full_counties, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank())
ca_map

full_counties <- accident2 %>%
  filter(STATENAME == "south dakota")

county_map <- ggplot(data=full_counties, 
                     mapping=aes(x=long,
                                 y=lat,
                                 group=group,
                                 data_id = COUNTYNAME,
                                 tooltip = paste("County:", 
                                                 toTitleCase(COUNTYNAME),
                                                 "\n% of Fatalities:",
                                                 round(prop * 100, 2),
                                                 "%"))) + 
  coord_fixed(1.3) + 
  geom_polygon_interactive(color="black", aes(fill = prop)) + 
  geom_polygon_interactive(data=full_counties, fill = NA, color="white") + 
  geom_polygon_interactive(color="black", fill=NA) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank())

girafe(ggobj = ca_map, options = opts_hover(css = "fill:green;stroke:black"))

# final cleaning/writing

accident <- read_csv("CrashDataApp/FARS2021NationalCSV/accident.csv")

state <- map_data("state")
state <- state %>%
  rename(STATENAME = region)

county <- map_data("county")
county <- county %>%
  rename(COUNTYNAME = subregion, STATENAME = region)

accident2 <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(COUNTYNAME = tolower(gsub("\\s*\\(\\d+\\)", "",
                                   gsub("\\.", "", COUNTYNAME))),
         STATENAME = tolower(STATENAME)) %>%
  group_by(STATENAME, COUNTYNAME) %>%
  summarize(count = n(), total = sum(FATALS)) %>%
  mutate(fatalcase = total / count,
         prop = total / sum(total))

accident1 <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(STATENAME = tolower(STATENAME)) %>%
  group_by(STATENAME) %>%
  summarize(count = n(), total = sum(FATALS)) %>%
  mutate(fatalcase = total / count,
         prop = total / sum(total))

accident1 <- accident1 %>%
  left_join(x = state, y = accident1, by = "STATENAME")

accident2 <- accident2 %>% 
  left_join(x = county, y = accident2, by = c("STATENAME", "COUNTYNAME"))

accident2 <- accident2 %>%
  replace(is.na(accident2), 0)

write_csv(accident1, file = "accident1.csv")

write_csv(accident2, file = "accident2.csv")


weather <- read_csv("CrashDataApp/FARS2021NationalCSV/weather.csv")

weather <- weather %>% 
  mutate(WEATHERNAME = case_when(WEATHERNAME %in% c("Reported as Unknown", 
                                                    "Not Reported",
                                                    "Other") ~ "Unknown",
                                 WEATHERNAME == "Blowing Snow" ~ "Snow",
                                 WEATHERNAME == "Blowing Sand, Soil, Dirt" ~ "Particulates",
                                 WEATHERNAME == "Fog, Smog, Smoke" ~ "Haze",
                                 WEATHERNAME == "Freezing Rain or Drizzle" ~ "Freezing Rain",
                                 WEATHERNAME == "Sleet or Hail" ~ "Sleet/Hail",
                                 WEATHERNAME == "Severe Crosswinds" ~ "Crosswinds",
                                 TRUE ~ WEATHERNAME)) %>%
  filter(WEATHERNAME != "Unknown") %>%
  group_by(STATENAME, WEATHERNAME) %>%
  summarize(count = n())

write_csv(weather, file = "weather1.csv")

w_plot <- ggplot(data = weather)+ 
  aes(x = WEATHERNAME,
      y = count,
      fill = WEATHERNAME,
      data_id = WEATHERNAME)+
  geom_bar_interactive(stat = "identity")+
  scale_fill_brewer_interactive(palette = "RdYlBu")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))+
  labs(title = "Cases by Weather Condition")

girafe(ggobj = w_plot,
       options = opts_hover(css = "fill:green;stroke:black"))

month <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  select(STATENAME, MONTHNAME) %>%
  group_by(STATENAME, MONTHNAME) %>%
  summarize(count = n())

month$MONTHNAME <- factor(month$MONTHNAME, levels = month.name)

write.csv(month, file = "month1.csv")

accident3 <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(COUNTYNAME = str_to_title(gsub("\\s*\\(\\d+\\)", "",
                                        gsub("\\.", "", COUNTYNAME))),
         STATENAME = str_to_title(STATENAME)) %>%
  select(STATENAME, ST_CASE, COUNTYNAME, MONTHNAME,
         DAYNAME, LATITUDE, LONGITUD, HOUR, 
         ROUTENAME, WEATHERNAME, FATALS)

accident5 <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(COUNTYNAME = tolower(gsub("\\s*\\(\\d+\\)", "",
                                        gsub("\\.", "", COUNTYNAME))),
         STATENAME = tolower(STATENAME),
         HOUR = factor(HOUR, levels = sort(unique(HOUR))),
         HOUR = case_when(HOUR == "99" ~ "Unknown",
                          TRUE ~ HOUR),
         WEATHERNAME = case_when(WEATHERNAME %in% c("Reported as Unknown", 
                                                    "Not Reported",
                                                    "Other") ~ "Unknown",
                                 WEATHERNAME == "Blowing Snow" ~ "Snow",
                                 WEATHERNAME == "Blowing Sand, Soil, Dirt" ~ "Particulates",
                                 WEATHERNAME == "Fog, Smog, Smoke" ~ "Haze",
                                 WEATHERNAME == "Freezing Rain or Drizzle" ~ "Freezing Rain",
                                 WEATHERNAME == "Sleet or Hail" ~ "Sleet/Hail",
                                 WEATHERNAME == "Severe Crosswinds" ~ "Crosswinds",
                                 TRUE ~ WEATHERNAME),
         LGT_CONDNAME = case_when(LGT_CONDNAME %in% c("Reported as Unknown", 
                                              "Not Reported",
                                              "Other") ~ "Unknown",
                                  TRUE ~ LGT_CONDNAME)) %>%
  select(STATENAME, ST_CASE, COUNTYNAME, MONTHNAME,
         DAY_WEEKNAME, LGT_CONDNAME, HOUR, HOURNAME, 
         ROUTENAME, WEATHERNAME, FATALS)

write_csv(accident5, "accident5.csv")

#TMAP PRACTICE
#------------------------------

counties <- counties(cb = TRUE)
states <- states(cb = TRUE)

states <- states %>%
  filter(!GEOID %in% c("60", "66", "69", "78", "72", "15", "02"))

weather2 <- weather %>%
  filter(WEATHERNAME == "Clear") %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii"))

counties1 <- counties %>%
  left_join(x = counties, y = accident2, by = join_by("NAME" == "COUNTYNAME"))

states1 <- states %>%
  left_join(x = states, y = weather2, by = join_by("NAME" == "STATENAME"))

states1 <- st_transform(states1, crs = 4326)

tmap_mode("view")

my_map <- tm_shape(states1)+
  tm_polygons(col = "count", alpha = 0.5)+
  tm_basemap("Esri.WorldTopoMap")

my_map

accident3 <- accident %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(COUNTYNAME = str_to_title(gsub("\\s*\\(\\d+\\)", "",
                                   gsub("\\.", "", COUNTYNAME))),
         STATENAME = str_to_title(STATENAME),
         HOUR = as.factor(HOUR),
         HOUR = case_when(HOUR == "99" ~ "Unknown",
                          TRUE ~ HOUR),
         WEATHERNAME = case_when(WEATHERNAME %in% c("Reported as Unknown", 
                                                    "Not Reported",
                                                    "Other") ~ "Unknown",
                                 WEATHERNAME == "Blowing Snow" ~ "Snow",
                                 WEATHERNAME == "Blowing Sand, Soil, Dirt" ~ "Particulates",
                                 WEATHERNAME == "Fog, Smog, Smoke" ~ "Haze",
                                 WEATHERNAME == "Freezing Rain or Drizzle" ~ "Freezing Rain",
                                 WEATHERNAME == "Sleet or Hail" ~ "Sleet/Hail",
                                 WEATHERNAME == "Severe Crosswinds" ~ "Crosswinds",
                                 TRUE ~ WEATHERNAME)) %>%
  select(STATENAME, ST_CASE, COUNTYNAME, MONTHNAME,
         DAY_WEEKNAME, LATITUDE, LONGITUD, HOUR, 
         ROUTENAME, WEATHERNAME, FATALS) %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE"), crs = 4326)

accident3 <- accident3 %>%
  filter(!(st_coordinates(.)[, 1] >= 180))

write_sf(accident3, "accident3.shp")


my_map <- tm_shape(accident3)+
  tm_dots(alpha = 0.5, size = "FATALS", col = "ROUTENAME")+
  tm_basemap("Esri.WorldTopoMap")

my_map

accident4 <- read_sf("CrashDataApp/accident3.shp")
