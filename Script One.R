library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(readr)

homicide <- read_csv("homicide-data.csv")
homicide_dc <- homicide %>%
  filter(city == 'Washington')

black <- homicide_dc %>% filter(victim_race == 'Black')
white <- homicide_dc %>% filter(victim_race == 'White')
hispanic <- homicide_dc %>% filter(victim_race == 'Hispanic')
asian <- homicide_dc %>% filter(victim_race == 'Asian')
other <- homicide_dc %>% filter(victim_race == 'Other')

open_n.a <- homicide_dc %>% filter(disposition == 'Open/No arrest')
closed_b.a <- homicide_dc %>% filter(disposition == 'Closed by arrest')
closed_w.a <- homicide_dc %>% filter(disposition == 'Closed without arrest')

pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"),
                   levels = c("Black", "Hispanic", "White", "Asian", "Other"))


leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = black, radius = 2, color = ~pal(victim_race), group = "Black", label = ~disposition) %>%
  addCircleMarkers(data = white, radius = 2, color = ~pal(victim_race), group = "White", label = ~disposition) %>%
  addCircleMarkers(data = hispanic, radius = 2, color = ~pal(victim_race), group = "Hispanic", label = ~disposition) %>%
  addCircleMarkers(data = asian, radius = 2, color = ~pal(victim_race), group = "Asian", label = ~disposition) %>%
  addCircleMarkers(data = other, radius = 2, color = ~pal(victim_race), group = "Other", label = ~disposition) %>%
  addLayersControl(overlayGroups = c("Black", "White", "Hispanic", "Asian", "Other")) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = c("Black", "Hispanic", "White", "Asian", "Other"))



leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = open_n.a, radius = 2, color = ~pal(victim_race), group = "Open/No arrest", label = ~disposition) %>%
  addCircleMarkers(data = closed_b.a, radius = 2, color = ~pal(victim_race), group = "Closed by arrest", label = ~disposition) %>%
  addCircleMarkers(data = closed_w.a, radius = 2, color = ~pal(victim_race), group = "Closed without arrest", label = ~disposition) %>%
  addLayersControl(overlayGroups = c("Open/No arrest","Closed by arrest","Closed without arrest")) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = c("Black", "Hispanic", "White", "Asian", "Other"))


homicide_dc %>%
  filter(victim_race != 'Other') %>%
  group_by(disposition, victim_race) %>%
  summarize(count = n()) %>%
  spread(disposition, count) %>%
  mutate(Arrest_Rate = (`Closed by arrest`/ (`Closed by arrest` + `Closed without arrest` + `Open/No arrest`))*100)



homicide_ny <- homicide %>% filter(city == "New York")

homicide_ny %>%
  filter(victim_race != 'Unknown') %>%
  group_by(disposition, victim_race) %>%
  summarize(count = n()) %>%
  spread(disposition, count) %>%
  mutate(Arrest_Rate = (`Closed by arrest`/ (`Closed by arrest` + `Closed without arrest` + `Open/No arrest`))*100)