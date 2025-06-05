library(tidyverse)
library(terra) # good for raster data, also supports vector
library(tidyterra) # convenient for filtering raster data
library(sf) # Good for vector data, does well in dplyr

# Optional: download data from google drive
library(googledrive)
drive_download(file = "https://drive.google.com/file/d/1iNYvuu--H_V3npEokqq4RyI2jUOMU7jm/view?usp=drive_link", 
path = "data/2015-2020_Conifer Habitat Suitability.tif")

drive_download(file = "https://drive.google.com/file/d/1rdA3-XkWlKyLQnPRW9m-2P5mcR0nDiI0/view?usp=drive_link", 
path = "data/SierraNevadaVCM_2015-2020_highres.gpkg")

drive_download(file = "https://drive.google.com/file/d/1La7lUO_Y_dWkTulZYFJHVBuZt2emMDT8/view?usp=drive_link", 
path = "data/fire19_1.gpkg")
