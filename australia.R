#load package----
library(sf)
library(tmap)
library(haven)
library(dplyr)
library(ggplot2)

#import shapefile----
shape= st_read("G:/quarantine/map/shape_file/Australia/1/AUS_adm1.shp", stringsAsFactors= F)

shape %>% tm_shape()+
  tm_fill()+
  tm_borders()

#import data----
covid= read_sav("G:/covid-19/mushi/Covid-19_mushi.sav")
names(covid)
View(covid)

#data wrangling----
covid_wrangled= covid %>% filter(Country == "Australia") %>%
  group_by(State) %>% 
  summarise(total= sum(Confirmed)) %>%
  ungroup()
covid_wrangled

#data joining----
covid_shape= left_join(covid_wrangled, shape, by= c("State"= "NAME_1")) %>%
  st_as_sf()


#map visualization using tmap----
covid_shape %>% tm_shape()+
  tm_fill(col = "total")+
  tm_polygons()


#map visualization using ggplot2----
covid_shape %>% ggplot()+
  geom_sf(aes(fill= total))+
  scale_fill_viridis_c(trans= "sqrt")+
  theme_light()+
  labs(title = "Confirmed Cases of Covid-19 in Australia",
       x= "Latitude", y= "longitude")+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"))




####try for animation map
covid_filter= covid %>% filter(Country == "Australia") %>%
  select(-Lat, -Long, -Recovered, -Deaths) %>% 
  mutate(State= as.character(State))

unique(covid_filter$State)
setdiff(shape$NAME_1, covid_filter$State)

covid_filter_shape= left_join(covid_filter, shape_filter, by= c("State" = "NAME_1")) %>%
  st_as_sf()


covid_filter_shape %>% ggplot()+
  geom_sf(aes(fill= Confirmed))+
  transition_states(date)


shape_filter= shape %>% select(NAME_1, geometry)

shape_filter %>% ggplot()+
  geom_sf()
