library(haven)
library(sf)
library(ggplot2)
library(dplyr)

#data----
covid= read_sav("G:/covid-19/mushi/Covid-19_mushi.sav")

covid_filter= covid %>% filter(Country == "China") %>% 
  group_by(State) %>%
  summarise(Confirmed= sum(Confirmed))

#shapefile----
shape= st_read("G:/quarantine/map/shape_file/china/2/CHN_adm2.shp", stringsAsFactors= F)
names(shape)
shape$NAME_1 %>% unique()

shape_edited= shape %>% select(State= NAME_1, geometry)

#Spelling correction----
setdiff(covid_filter$State, shape_edited$State)

covid_filter$State[grep("Ning", covid_filter$State)] = "Ningxia Hui"

covid_filter$State[grep("Xinj", covid_filter$State)] = "Xinjiang Uygur"

covid_filter$State[grep("Mon", covid_filter$State)] = "Nei Mongol"

#manipualtion rough----
covid_filter$State[grep("Xiz", covid_filter$State)] 

grep("Mon", shape_edited$State, value = T)

covid_filter$State

#joining----
covid_shape= right_join(shape_edited, covid_filter,
                       by= c("State" = "State")) %>% 
  st_as_sf()

covid_shape
#map----
covid_shape %>% ggplot()+
  geom_sf(aes(fill= Confirmed))+
  scale_fill_viridis_c(trans= "sqrt")+
  theme_dark()

covid_shape %>% ggplot()+
  geom_sf(aes(fill= Confirmed))+
  scale_fill_gradient(low = "yellow", high = "blue")+
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)
