# Load packages
library(sf)
library(readxl)
library(tidyverse)
library(rayshader)

# Import shape files
shape= st_read("G:/quarantine/map/hridoy vai/2/bgd_admbnda_adm2_bbs_20180410.shp",stringsAsFactors = F)

# Import data
covid= read_excel("G:/covid-19_region.xlsx") 

class(shape)

names(shape)

# Shape wrangling
shape %>% 
    select(Division = ADM1_EN, District = ADM2_EN,geometry) -> edited_shape_df


# Necessary wrangling for data
covid= covid %>% mutate(District= recode(`District/City`,
                                         "B. Baria"= "Brahamanbaria",
                                         "Dhaka City"= "Dhaka",
                                         "Dhaka (District)"= "Dhaka",
                                         "Munshigonj"= "Munshiganj",
                                         "Narshingdi"= "Narsingdi",
                                         "Chattogram"= "Chittagong",
                                         "Cox's bazar"= "Cox's Bazar",
                                         "Cumilla"= "Comilla",
                                         "Khagrachari"= "Khagrachhari",
                                         "Laksmipur"= "Lakshmipur",
                                         "Rangmati"= "Rangamati",
                                         "Moulovi Bazar"= "Maulvibazar",
                                         "Hobiganj"= "Habiganj",
                                         "Panchagar"= "Panchagarh",
                                         "Netrokona"= "Netrakona",
                                         "Barishal"= "Barisal",
                                         "Potuakhali"= "Patuakhali",
                                         "Jhalokathi"= "Jhalokati"))


# Fix spelling errors
 # 1. Divisions 

covid$Division[grepl(x = covid$Division,pattern = "Ch")]  <- "Chittagong"
covid$Division[grepl(x = covid$Division,pattern = "Bar")] <- "Barisal"
 # 2. Districts

covid$District[grepl(x = covid$District,pattern = "Cox")] <- "Cox's Bazar"

# Cannot find this: Chapainawabganj SO there will be one NA

#  You have two rows where The District and the Division is Dhaka so lets summarize the confirmed cases by Division and district
covid %>% 
  count(Division,District,wt = confirmed,sort = T,name = "confirmed") -> covid_summary_df
  

# Perform inner join
#  Note that it will remove Chapainawabganj because it cannot find a polygon for it

shape_file_for_plotting <- sf::st_as_sf(merge(covid_summary_df,edited_shape_df,by = c("Division","District")))

# Basic plot to get you started
theme_set(theme_minimal())

gg= ggplot(shape_file_for_plotting)+
  geom_sf(aes(fill = confirmed))+
  # scale_fill_gradient2(low = "#2C3E50",high = "darkred")+
  scale_fill_viridis_c(trans = "sqrt")+
  labs(title = "Confirmed Cases of Covid-19",
       x= "Longituge", y= "Latitude")+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

gg


# Convert 2D to 3D visualization
plot_gg(gg, height = 5, width = 7, scale = 250, zoom= .6,
        multicore = T, windowsize= c(800, 800))


# Necessary vector for video
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 60 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))


# Map video
render_movie(filename = "BD_map", type = "custom", 
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
