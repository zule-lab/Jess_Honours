# =============================================================================================== #
# COVARIATES FOR TRACKED BIRDS DURING 2022-2023 STOPOVERS AT MBO                                  #
# =============================================================================================== #

# Code written by Barbara Frei
# Last edited February 4 2026

library(cancensus)
library(sf)
library(terra)
# library(raster)
library(plyr)
library(ggplot2)
library(amt)
library(sp)
library(landscapemetrics)
library(purrr)
library(dplyr)
library(pdftools)
library(tidyr)
library(readr)
library(lubridate)
library(progressr)


# =============================================================================================== #
# 1. LOADING FILES FOR TRACKING DATA, ALONG WITH RASTERS AND DESCRIPTION DATAFRAME #
# =============================================================================================== #

## --- 1.1. SPATIAL DATA --- ##

# Load spatial data as a raster from IndiceCanopee https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee
indice_660.ras <- rast("2 - Cleaned/660_IndiceCanopee_2023.tif")



## --- 1.2. YARD CENTROIDS --- ##

# Load yard centroid data
centroid_data <- read_csv("2 - Cleaned/centroid_data.csv")

# Prepare centroid data frame to contain only spatial information
centroid_deg_df <-
  select(centroid_data, c(Yard.Code,lat,long)) # lat and long centroids

centroid_utm_df <-
  select(centroid_data, c(Yard.Code,utm_zone,utm_easting,utm_northing)) # utm centroids

# Make sure Yard ID is a factor.
centroid_deg_df$Yard.Code <- as.factor(centroid_deg_df$Yard.Code)
centroid_utm_df$Yard.Code <- as.factor(centroid_utm_df$Yard.Code)



"# not applicable:
# Loading the csv file for the tracking data
tracked.df <- read.csv("2 - Cleaned/tracking_all.csv")
#create a new ID that is unique for individual ID and date 
tracked.df <- tracked.df %>%
  mutate(
    ID2 = paste(ID, format(as.Date(Date), "%Y%m%d"), sep = "_")
  )
# Make sure ID is a factor.
tracked.df$ID2 <- as.factor(tracked.df$ID2)
# for random need to include the repeated 1-5 randoms for each tracked point 
# to prevent boisfriche.df from summarising across the ID2
random.df <- random.df %>%
  group_by(ID2) %>%
  mutate(ID2 = paste0(ID2, "_", row_number())) %>%
  ungroup()
## --- 1.3. DATA PREPARATION --- ##
# Filter the true points dataframe to only contain the randlat, randlon and ID.
tracked.df <- tracked.df %>% 
dplyr::select(Date, Species, ID, ID2, Lat, Lon)
head(tracked.df)
# Filter the random points dataframe to only contain the randlat, randlon and ID.
random.df <- random.df %>% 
dplyr::select(ID2, randLat, randLon)
head(random.df)
# In this dataframe (random.df), the column names for lat and lon are reversed.
# We'll switch it back to normal.
random.df <- random.df %>%
  mutate(temp = randLat,  # Store randLat in a temporary column
         randLat = randLon, 
         randLon = temp) %>%
  dplyr::select(-temp)  # Remove the temporary column
head(random.df)"


## =============================================================================================== #
# 2.  EXTRACTING COVARIATES FROM NDG YARDS AND INDICE CANOPEE RASTER                                 #
# ================================================================================================ #

# Convert centroid data frame to a spatial data frame
centroid_deg_sf <- st_as_sf(centroid_deg_df, coords = c("long", "lat"), crs = 4326)
centroid_utm_sf <- st_as_sf(centroid_utm_df, coords = c("utm_easting", "utm_northing"), crs = 32618) # UTM zone 18
st_crs(centroid_utm_sf)

# Ensure the CRS matches the CRS of the raster before buffering.
centroid_deg_sf <- st_transform(centroid_deg_sf, crs = st_crs(indice_660.ras))
centroid_utm_sf <- st_transform(centroid_utm_sf, crs = st_crs(indice_660.ras))
st_crs(indice_660.ras) # seems to be in MTM coordinates

# Define buffer distances
buffers <- c(25, 50, 100, 200, 400)

# Initialize list to store results
results_list <- list()

# Loop through canopy raster at each buffer and extract data
for (i in 1:length(buffers)) {
  
  # Create buffer
  buffer <- st_buffer(centroid_deg_sf, buffers[i])
  
  # Convert buffer to to terra format
  buffer_vect <- vect(buffer)
  
  # Crop raster to buffer
  r_crop <- crop(indice_660.ras, buffer_vect)
  
  # Extract raster values within the buffer
  values <- terra::extract(r_crop, buffer_vect)
  names(values)
  
  # Count cells of each land cover types (type 1 or 2)
  summary <- values %>%
    dplyr::group_by(ID, Type = `660_IndiceCanopee_2023`) %>%   # lyr.1 is raster value column
    dplyr::summarise(n = n()) %>%
    ungroup()
  
  # Calculate total number of cells (proxy for area)
  total_cells <- sum(summary$n)
  
  # Convert counts to percentages of land cover types
  summary <- summary %>%
    mutate(
      buffer_radius_m = buffers[i],
      percentage = 100 * n / total_cells
    )
  
  results_list[[i]] <- summary
  names(results_list)[i] <- buffers[i]
}
  
results_list
  
  
"
  # Intersect land cover polygons with buffer
  land_cover_in_buffer <- st_intersection(indice_660.ras, buffer)
  # Calculate area of each intersected polygon
  land_cover_in_buffer <- land_cover_in_buffer %>%
    mutate(area = sf::st_area(.))
  
  # Summarize area by land cover type (type 1 or 2)
  summary <- land_cover_in_buffer %>%
    group_by(Yard.Code, Type) %>%
    summarise(area2 = mean(area)) %>%
    ungroup()
  
  # Calculate total buffer area for percentage calculation
  total_area <- pi *  buffers[i]^2
  
  summary <- summary %>%
    mutate(
      buffer_radius_m =  buffers[i],
      percentage = 100 * as.numeric(area2) / as.numeric(total_area)
    )
  
  # complete cases
  summary <- summary %>% complete(Yard.Code, Type, fill = list(percentage = 0, buffer_radius_m = buffers[i]))
  results_list[[i]] <- summary
  names(results_list)[[i]] <- buffers[i]
"




# Convert to spatial data
tracked.sf <- st_as_sf(tracked.df, coords = c("Lon", "Lat"), crs = 4326)
# Ensure the CRS matches the CRS of the raster before buffering.
tracked.sf <- st_transform(tracked.sf, crs = st_crs(Boisfriche.shp))
# Ensure the CRS matches the CRS of the raster before buffering.
random.sf <- st_as_sf(random.df, coords = c("randLon", "randLat"), crs = 4326)
random.sf <- st_transform(random.sf, crs = st_crs(Boisfriche.shp))
# Define buffer distances
buffers <- c(25, 50, 100, 200)
# Initialize list to store results
results_list <- list()

for (i in 1:length(buffers)) {
  # Create buffer
  buffer <- sf::st_buffer(tracked.sf, buffers[i])
  
  # Intersect land cover polygons with buffer
  land_cover_in_buffer <- st_intersection(Boisfriche.shp, buffer)
  
  # Calculate area of each intersected polygon
  land_cover_in_buffer <- land_cover_in_buffer %>%
    mutate(area = sf::st_area(.))
  
  # Summarize area by land cover type (type 1 or 2)
  summary <- land_cover_in_buffer %>%
    group_by(ID2, Type) %>%
    summarise(area2 = mean(area)) %>%
    ungroup()
  
  # Calculate total buffer area for percentage calculation
  total_area <- pi *  buffers[i]^2
  
  summary <- summary %>%
    mutate(
      buffer_radius_m =  buffers[i],
      percentage = 100 * as.numeric(area2) / as.numeric(total_area)
    )
  
  #complete cases
  summary <- summary %>% complete(ID2, Type, fill = list(percentage = 0, buffer_radius_m = buffers[i]))
  results_list[[i]] <- summary
  names(results_list)[[i]] <- buffers[i]
}

BoisFriche <- as.data.frame(rbind(results_list[[1]], results_list[[2]], results_list[[3]], results_list[[4]]))
BoisFriche_tracked.df <- BoisFriche[,c(1,2,5,6)]

BoisFriche_tracked.df <- BoisFriche_tracked.df %>% 
  pivot_wider(names_from = c(Type, buffer_radius_m), values_from = percentage)


### RANDOM POINT EXTRACTION #####

# Initialize list to store results
results_list <- list()

for (i in 1:length(buffers)) {
  # Create buffer
  buffer <- st_buffer(random.sf, buffers[i])
  
  # Intersect land cover polygons with buffer
  land_cover_in_buffer <- st_intersection(Boisfriche.shp, buffer)
  
  # Calculate area of each intersected polygon
  land_cover_in_buffer <- land_cover_in_buffer %>%
    mutate(area = sf::st_area(.))
  
  # Summarize area by land cover type (type 1 or 2)
  summary <- land_cover_in_buffer %>%
    group_by(ID2, Type) %>%
    summarise(area2 = mean(area)) %>%
    ungroup()
  
  # Calculate total buffer area for percentage calculation
  total_area <- pi *  buffers[i]^2
  
  summary <- summary %>%
    mutate(
      buffer_radius_m =  buffers[i],
      percentage = 100 * as.numeric(area2) / as.numeric(total_area)
    )
  
  #complete cases
  summary <- summary %>% complete(ID2, Type, fill = list(percentage = 0, buffer_radius_m = buffers[i]))
  
  results_list[[i]] <- summary
  names(results_list)[[i]] <- buffers[i]
}

BoisFriche <- as.data.frame(rbind(results_list[[1]], results_list[[2]], results_list[[3]], results_list[[4]]))
BoisFriche_random.df <- BoisFriche[,c(1,2,5,6)]

BoisFriche_random.df <- BoisFriche_random.df %>% 
  pivot_wider(names_from = c(Type, buffer_radius_m), values_from = percentage)

### MERGING TRACKED AND RANDOM DATA #####

BoisFriche_tracked.df$Type = 1
BoisFriche_random.df$Type = 0

RSF.df <- rbind(BoisFriche_tracked.df, BoisFriche_random.df)

write.csv(RSF.df, file = "02_processed_data/obs.rand.covariates.csv")


## =============================================================================================== #
# 3.  EXTRACTING COVARIATES FROM CANOPY HEIGHT RASTER                               #
# ================================================================================================ #

tracked.sf <- st_as_sf(tracked.df, coords = c("Lon", "Lat"), crs = 4326)
# Ensure the CRS matches the CRS of the raster before buffering.
tracked.sf <- st_transform(tracked.sf, crs = st_crs(canopy_height.ras))

# Ensure the CRS matches the CRS of the raster before buffering.
random.sf <- st_as_sf(random.df, coords = c("randLon", "randLat"), crs = 4326)
random.sf <- st_transform(random.sf, crs = st_crs(canopy_height.ras))

# Define buffer distances
buffers <- c(25, 50, 100, 200)

# for tracked points

points_vect <- terra::vect(tracked.sf)

out_tracked <- as.data.frame(tracked.sf)

for (b in buffers) {
  
  # create buffer around points
  buf <- terra::buffer(points_vect, width = b)
  
  # extract mean and SD
  vals <- terra::extract(
    canopy_height.ras,
    buf,
    fun = function(x) c(mean = mean(x, na.rm = TRUE),
                        sd   = sd(x, na.rm = TRUE)),
    touches = TRUE
  )
  
  # add to output using column index
  out_tracked[[paste0("canopy_mean_", b, "m")]] <- vals[,2]
  out_tracked[[paste0("canopy_sd_", b, "m")]]   <- vals[,3]
}


# for random points

random_vect <- terra::vect(random.sf)


out_random <- as.data.frame(random.sf)

for (b in buffers) {
  
  # create buffer around points
  buf <- terra::buffer(random_vect, width = b)
  
  # extract mean and SD
  vals <- terra::extract(
    canopy_height.ras,
    buf,
    fun = function(x) c(mean = mean(x, na.rm = TRUE),
                        sd   = sd(x, na.rm = TRUE)),
    touches = TRUE
  )
  
  # add to output using column index
  out_random[[paste0("canopy_mean_", b, "m")]] <- vals[,2]
  out_random[[paste0("canopy_sd_", b, "m")]]   <- vals[,3]
}

  

write.csv(out_tracked, file = "02_processed_data/obs.treeheight.csv")
write.csv(out_random, file = "02_processed_data/random.treeheight.csv")




## =============================================================================================== #
# 4.  EXTRACTING COVARIATES FROM INDICE CANOPEE RASTER                                             #
# ================================================================================================ #

# https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee

tracked.sf <- st_as_sf(tracked.df, coords = c("Lon", "Lat"), crs = 4326)
# Ensure the CRS matches the CRS of the raster before buffering.
tracked.sf <- st_transform(tracked.sf, crs = st_crs(indice_660.ras))

# Ensure the CRS matches the CRS of the raster before buffering.
random.sf <- st_as_sf(random.df, coords = c("randLon", "randLat"), crs = 4326)
random.sf <- st_transform(random.sf, crs = st_crs(indice_660.ras))

# Define buffer distances
buffers <- c(25, 50, 100, 200)

# function for proportion of raster classes

class_props <- function(x, classes = 0:5) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(rep(NA, length(classes)))
  
  tab <- table(factor(x, levels = classes))
  prop <- as.numeric(tab) / sum(tab)
  return(prop)
}


# for tracked points

points_vect <- terra::vect(tracked.sf)
out_tracked <- as.data.frame(tracked.sf)

for (b in buffers) {
  
  buf <- terra::buffer(points_vect, width = b)
  
  vals <- terra::extract(
    indice_660.ras,
    buf,
    fun = class_props,
    touches = TRUE
  )
  
  # add 6 class columns
  for (i in 1:6) {
    out_tracked[[paste0("indice660_class", i, "_", b, "m")]] <- vals[, i + 1]
  }
}


# for random points
random_vect <- terra::vect(random.sf)
out_random <- as.data.frame(random.sf)

for (b in buffers) {
  
  buf <- terra::buffer(random_vect, width = b)
  
  vals <- terra::extract(
    indice_660.ras,
    buf,
    fun = class_props,
    touches = TRUE
  )
  
  for (i in 1:6) {
    out_random[[paste0("indice660_class", i, "_", b, "m")]] <- vals[, i + 1]
  }
}



# need to get rid of geometry list or it saved csv strangely

out_tracked$geometry <- NULL
out_random$geometry <- NULL


write.csv(out_tracked, "02_processed_data/obs.indice660.csv")
write.csv(out_random, "02_processed_data/random.indice660.csv")

