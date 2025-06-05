library(tidyverse)
library(terra) # good for raster data, also supports vector
library(tidyterra) # convenient for filtering raster data
library(sf) # Good for vector data, does well in dplyr
sf_use_s2(FALSE) # Disables the s2 geometry engine, which can cause issues with geographic operations.
# The s2 engine enforces strict validity checks on spherical geometries,
# but sometimes we want simpler planar operations for speed/compatibility

# Optional: download data from google drive
library(googledrive)

drive_download(
  file = "https://drive.google.com/file/d/1iNYvuu--H_V3npEokqq4RyI2jUOMU7jm/view?usp=drive_link",
  path = "data/2015-2020_Conifer Habitat Suitability.tif"
)

drive_download(
  file = "https://drive.google.com/file/d/1rdA3-XkWlKyLQnPRW9m-2P5mcR0nDiI0/view?usp=drive_link",
  path = "data/SierraNevadaVCM_2015-2020_highres.gpkg"
)

drive_download(
  file = "https://drive.google.com/file/d/1La7lUO_Y_dWkTulZYFJHVBuZt2emMDT8/view?usp=drive_link",
  path = "data/fire19_1.gpkg"
)



# Vector intersections ---------------------------------------
#' Read in vector data from the Sierra Nevada VCM and intersect it with
#' FRAP fire perimeters

## Prepping the data  ---------------------------------------
sierra_nevada_vcm <- st_read("data/SierraNevadaVCM_2015-2020_highres.gpkg")
frap_19 <- st_read("data/fire19_1.gpkg")

# I'm going to filter to polygons north of Tahoe to make sure this isn't too
# computationally intensive, and west of -120.75
nw_vcm <- sierra_nevada_vcm |>
  mutate(
    mean_latitude = st_coordinates(st_centroid(geom))[, "Y"],
    mean_longitude = st_coordinates(st_centroid(geom))[, "X"]
  ) |>
  filter(mean_latitude > 39.25, mean_longitude < -120.75) |>
  select(conifer_class) # Drop mean_latitude and mean_longitude columns

nw_frap <- frap_19 |>
  st_transform(st_crs(nw_vcm)) |> # transform to the same CRS as the VCM (epsg:4326)
  st_crop(st_bbox(nw_vcm)) # crop to the extent of the VCM

## Mapping ---------------------------------------

ggplot() +
  geom_sf(data = nw_vcm, aes(fill = conifer_class), color = NA) +
  geom_sf(data = nw_frap, fill = NA, color = "black")
# Cool! Looks fine

## Simple spatial join  ---------------------------------------
# let's do a spatial join to get the conifer_class for each fire, so that we can
# explore the relationship between any column in nw_frap and the corresponding conifer_class
nw_frap_vcm <- nw_frap |>
  st_join(nw_vcm)

# now you can see that conifer_class is tacked on to each fire row in nw_frap
head(nw_frap_vcm)

# And Now we can explore all sorts of relationships. Let's try GIS_ACRES
ggplot(nw_frap_vcm) +
  geom_boxplot(aes(x = conifer_class, y = GIS_ACRES))

## Logistic regression ---------------------------------------
# let's do logistic regression between GIS_ACRES and conifer_class (VCM vs Stable)
nw_frap_vcm <- nw_frap_vcm |>
  mutate(vcm_overlap = case_when(
    conifer_class == "VCM" ~ 1,
    conifer_class == "VCM (Severe)" ~ 1,
    conifer_class == "Stable Conifer Distribution" ~ 0
  ))

# plot logistic regression between GIS_ACRES and vcm_or_stable
nw_frap_vcm |>
  filter(!is.na(vcm_overlap)) |>
  ggplot(
    aes(x = GIS_ACRES, y = vcm_overlap) # turn y into a factor
  ) +
  # jitter raw points so they overlap less
  geom_jitter(height = 0.02, width = 0, alpha = 0.4, size = 1.5, color = "steelblue") +
  # add fitted logistic curve:
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = TRUE, # set se = FALSE if you don't want the confidence band
    color = "darkred",
    size = 1
  )

# Nothing that looks significant!
#' but worth noting that the way we did the spatial join, many of the big fires
#' could have overlapped with VCM and stable forest, and therefore would show up
#' with both.
#' More complex analysis could include looking at the area of the overlap between fires and VCM
#' and for that you'd want to use st_intersection and st_area functions



# Raster extractions ---------------------------------------

## Prepping the data  ---------------------------------------
conifer_hsm <- rast("data/2015-2020_Conifer Habitat Suitability.tif")
frap_19 <- st_read("data/fire19_1.gpkg")
