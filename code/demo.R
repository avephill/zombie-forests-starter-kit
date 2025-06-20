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
  file = "https://drive.google.com/file/d/1NwDP16rknhvqXXutCwj75avjq_NFDEE_/view?usp=drive_link",
  path = "data/frap_perimeters24_1.gpkg"
)



# Vector intersections ---------------------------------------
#' Read in vector data from the Sierra Nevada VCM and intersect it with
#' FRAP fire perimeters

## Prepping the data  ---------------------------------------
sierra_nevada_vcm <- st_read("data/SierraNevadaVCM_2015-2020_highres.gpkg")
frap_24 <- st_read("data/frap_perimeters24_1.gpkg")

# I'm going to filter to polygons north of Tahoe to make sure this isn't too
# computationally intensive, and west of -120.75
nw_vcm <- sierra_nevada_vcm |>
  mutate(
    mean_latitude = st_coordinates(st_centroid(geom))[, "Y"],
    mean_longitude = st_coordinates(st_centroid(geom))[, "X"]
  ) |>
  filter(mean_latitude > 39.25, mean_longitude < -120.75) |>
  select(conifer_class) # Drop mean_latitude and mean_longitude columns

nw_frap <- frap_24 |>
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
# Extract values from rasters data using vector data (points, lines, polygons)

## Prepping the data  ---------------------------------------
conifer_hsm <- rast("data/2015-2020_Conifer Habitat Suitability.tif")
frap_24 <- st_read("data/frap_perimeters24_1.gpkg")

# Some geometries are invalid, so we need to filter them out
frap_valid <- frap_24 |>
  filter(st_is_valid(geom))

# Filter fire perimeters to those within the extent of the HSM raster
frap_masked <- frap_valid |>
  # Change to the same Coordinate Reference System (CRS) as the HSM raster
  st_transform(st_crs(conifer_hsm)) |>
  # Crop to the bounding box of the HSM raster
  st_crop(st_bbox(conifer_hsm)) |>
  # also add a unique identifier for each fire row
  mutate(FIRE_ID = row_number())

# # For some, there are many rows per fire. Not sure why this is. To simplify analysis
# # later on we are going to UNION the polygons into one row per fire based on the fire id (IRWINID)
# frap_unioned <- frap_masked |>
#   group_by(IRWINID) |>
#   # after unioning the geometries, this just keeps the first row of the other FIRE data
#   # Note that this is a bit of a hack, and we should probably do this in a more robust way
#   summarise(across(-geom, first), geom = st_union(geom))

### Here's some cool stuff you can do with tidyterra ---
conifer_hsm |> plot() # regular plot

# Filter to just Stable Conifer forest (layer >= .52)
conifer_hsm |>
  filter(layer > .52) |> # the layer name is just 'layer'
  plot()

# Bin the raster by the HSM thresholds we use for VCM
conifer_hsm |>
  mutate(conifer_suitability = case_when(
    layer < .18 ~ "Severely Unsuitable",
    layer < .52 ~ "Unsuitable",
    layer >= .52 ~ "Suitable"
  )) |>
  plot()
# Notice that we created a new raster layer called conifer_suitability
# that is plotted alongside the original 'layer' layer.

## Extract HSM values for each fire ---------------------------------------

# So this gets the HSM values for every cell within all fires
extracted_hsm <- extract(conifer_hsm, frap_masked)

# Now combine into ggplot friendly table
extract_df <- extracted_hsm |>
  as_tibble() |>
  mutate(hsm_value = layer, group = "infire") |>
  select(hsm_value, group)

all_df <- conifer_hsm |>
  as_tibble() |>
  mutate(hsm_value = layer, group = "overall") |>
  select(hsm_value, group)

hsm_plot_df <- bind_rows(extract_df, all_df)

ggplot(hsm_plot_df) +
  geom_histogram(aes(x = hsm_value),
    alpha = 0.7, position = "identity",
    linewidth = 1
  ) +
  facet_wrap(~group, scales = "free_y")


# But it's probably more interesting to group HSM values by fire
# Here's how we do that

# Group the fire perimeters by unique Fire ID (IRWINID) and extract HSM values for each fire using the map function
# This takes a little bit to run.

nrow(frap_masked)
frap_masked |>
  distinct(INC_NUM) |>
  nrow()
frap_masked |>
  distinct(IRWINID) |>
  nrow()

fire_hsm_extract_df <- frap_masked |>
  slice_max(GIS_ACRES, n = 100) |> # pull 50 largest fires so this runs faster
  # Split into list of rows by Fire ID
  group_split(FIRE_ID) |>
  # Now you can analyze HSM values by fire. The following applies the
  # function defined in the map function to each set of rows in each unique fire
  map_dfr(function(fire_row) {
    fire_hsm <- extract(conifer_hsm, fire_row) # extract HSM values for this fire

    # Create table with columns FIRE_ID and hsm_value
    fire_hsm |>
      as_tibble() |>
      mutate(FIRE_ID = unique(fire_row$FIRE_ID)) |>
      select(FIRE_ID, hsm_value = layer)
  })

# Now we have a table with every extracted HSM value associated with the corresponding fire ID
# which that HSM cell was in. Now let's join this table to the original fire table so that we have
# a large table with every fire and its HSM values.

fire_hsm_extract_full <- fire_hsm_extract_df |>
  filter(!is.na(hsm_value)) |> # We don't care about rows with NA HSM values
  left_join(frap_masked, by = "FIRE_ID")

# Map of Fires and avg HSM values
fire_hsm_extract_full |>
  group_by(FIRE_NAME) |>
  summarize(hsm_mean = mean(hsm_value), geom = st_union(geom)) |>
  st_sf() |>
  ggplot() +
  geom_sf(aes(fill = hsm_mean)) +
  geom_sf_text(aes(label = FIRE_NAME), check_overlap = TRUE, nudge_x = -.25, nudge_y = .25)


# Ok now let's look at Habitat Suitability of fire over time
fire_hsm_extract_full |>
  # filter(YEAR_ > 2000) |>
  ggplot() +
  geom_boxplot(aes(x = as.factor(YEAR_), y = hsm_value)) +
  labs(title = "Conifer Habitat Suitability for 100 Largest fires by year")
