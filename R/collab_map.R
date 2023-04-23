#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Collab Map 
# Date: 4/22/2023
# Coder: Nate Jones (cnjones7@ua.edu)
# Purpose: Create a map of collaborator locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list = ls())

#load packages of interest
library(tidyverse)
library(scholar)
library(sf)
library(tigris)
library(rcrossref)
library(mapsapi)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

#Dfine Scholar ID
orcid <- "0000-0002-5804-0510"

#Define google api key
api_key <- #Obtain from google maps platform

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create list of collaborators -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Search cross reference with ORCID
df <- cr_works(filter = c(orcid = orcid))

#Define coathors
df <- bind_rows(df$data$author)

#tidy data
df <- df %>% 
  mutate(name = paste(given,family)) %>%  
  select(ORCID, name, affiliation.name) %>% 
  arrange(name)

#Manually insert missing data
df$ORCID[is.na(df$ORCID)] <- seq(1, length(df$ORCID[is.na(df$ORCID)]))
df$affiliation.name[df$name == "Judson W. Harvey"] <- "USGS Reston, VA"

#Remove NA and duplicates
df <- df %>% drop_na()

#Remove duplicates
df <- df %>% 
  group_by(ORCID, name) %>% 
  slice_head() %>% 
  ungroup() %>% 
  arrange(name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define XY Coordinates  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Create function to access Google API
# google_api<-function(UID=NULL, address = NULL, api_key = NULL){
#   
#   #Download xml file
#   doc<-mp_geocode(address, key=api_key)
#   
#   #Convert to points
#   pnts<-mp_get_points(doc)
#   
#   #Convert to coordinates
#   xy<-st_coordinates(pnts)
#   
#   #export coordinates with Unique ID
#   data.frame(UID, xy)
# }
# 
# #Create error catching wrapper
# fun<-function(n){
#   tryCatch(google_api(UID     = df$ORCID[n],
#                       address = df$affiliation.name[n], 
#                       api_key = api_key), 
#            error=function(e) data.frame(UID=df$Key[n], 
#                                         X=0, 
#                                         Y=0))
# }
# 
# #apply error catching wrapper
# pnts <- lapply(seq(1, nrow(df)), fun) %>% bind_rows()
# 
# #joint to df
# df <- left_join(df, pnts %>% rename(ORCID = UID))
# 
# #export data
# write_csv(df, "data//locations.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create map -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Starting point; https://axelhodler.medium.com/creating-a-heat-map-from-coordinates-using-r-780db4901075

#Read in data
df <- read_csv("data//locations.csv")

#Add extra alabama folks (n = 10; Jon, Carla, Arial, Christie, Matt, DMP)
df <- bind_rows(
  df, 
  df %>% slice(rep(9,10))
)

#Create pnts shape
pnts <- df %>% 
  st_as_sf(
    coords = c("X","Y"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs'
  ) %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Edit States shape
states <- states() %>% 
  filter(
    NAME != 'Alaska', 
    NAME != 'Hawaii', 
    NAME != 'Guam', 
    NAME != 'Commonwealth of the Northern Mariana Islands', 
    NAME != 'American Samoa',
    NAME != 'Puerto Rico',
    NAME != 'United States Virgin Islands') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Select pnts that intersect with states
pnts <- pnts[states,]

#Add projected x and y coordinates to pnts
pnts <- pnts %>% 
  mutate(x_proj = st_coordinates(pnts)[,1],
         y_proj = st_coordinates(pnts)[,2])

#plot
ggplot()+
  geom_sf(data = states) + 
  stat_density2d(
    data = pnts, 
    aes(x=x_proj, y=y_proj, fill=..level.., alpha=..level..), 
    geom="polygon", 
    adjust = c(0.25,0.5)) +
  geom_sf(data = pnts,
          color = 'grey30',
          alpha = 0.70) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +
  theme_bw()+
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14,color="black"), 
    axis.text  = element_text(size = 12,color="black")
  ) +
  xlim(-2242156,2132371) + ylim(-1711219, 1333267) +
  xlab(NULL) +ylab(NULL) + 
  guides(fill = "none", alpha = "none")



  
