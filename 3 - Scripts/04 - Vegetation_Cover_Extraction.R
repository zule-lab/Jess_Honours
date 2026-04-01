# 12-Mar-2026
# ============================================================================ #
#                    EXTRACTION OF LANDSCAPE HABITAT FEATURES
# ============================================================================ #

# DESCRIPTION: 
# Extracting values for landscape habitat features (greenness, vegetation height)
# in buffers (25m, 50m, 100m, 200m, 400m) around yards.

# PACKAGES USED
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


# ============================================================================ #
# 1. LOADING FILES FOR YARD DATA, ALONG WITH RASTERS AND DESCRIPTION DATAFRAME 
# ============================================================================ #

# --- 1.1. SPATIAL DATA --- #

# https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee
# Load spatial data as a raster from IndiceCanopee
indice_660.ras <- rast("2 - Cleaned/660_IndiceCanopee_2023.tif")


# --- 1.2. YARD CENTROIDS --- #

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





## =========================================================================== #
# 2.  EXTRACTING COVARIATES FROM INDICE CANOPEE RASTER                                
# ============================================================================ #

# https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee

## --- 2.1. PREPARING DATA --- ##

# Convert centroid data frame to a spatial data frame
centroid_deg_sf <- st_as_sf(centroid_deg_df, coords = c("long", "lat"), crs = 432)

# Ensure the CRS matches the CRS of the raster before buffering.
centroid_deg_sf <- st_transform(centroid_deg_sf, crs = st_crs(indice_660.ras))

# Define buffer distances
buffers <- c(25, 50, 100, 200, 400)


## --- 2.2. EXTRACTING CANOPY DATA FROM NDG YARDS --- ##

# Function to calculate the proportion of each raster class inside a buffer
class_props <- function(x, classes = 0:5) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(rep(NA, length(classes)))
  
  tab <- table(factor(x, levels = classes))
  prop <- as.numeric(tab) / sum(tab)
  return(prop)
}

# Vectorize yard centroid data
centroid_vect <- terra::vect(centroid_deg_sf)

# Create data frame to store output
canopy_output <- as.data.frame(centroid_deg_sf)

# Loop through canopy raster at each buffer and extract data
for (b in buffers) {
  
  # Create buffer around each centroid at radius b
  buf <- terra::buffer(centroid_vect, width = b)
  
  # Extract raster values from within each buffer of each yard
  vals <- terra::extract(
    indice_660.ras,
    buf,
    fun = class_props,
    touches = TRUE # any raster cell touched by the buffer is included
  )
  
  # Add 6 class columns to result data frame
  for (i in 1:6) {
    canopy_output[[paste0("indice660_class", i, "_", b, "m")]] <- vals[, i + 1]
  }
}

# Remove geometry list or it saved csv strangely
canopy_output$geometry <- NULL

# Write csv with new canopy height data
write.csv(canopy_output, "1 - Input/canopy_output.csv")






# code that I wrote, but realized was longer than necessary
'
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
  
  # Count cells of each land cover types (types 1, 2, 3, 4)
  summary <- values %>%
    dplyr::group_by(ID, Type = `660_IndiceCanopee_2023`) %>%   # lyr.1 is raster value column
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::group_by(ID) %>% # group by yard to count number of cells
    dplyr::mutate(
      total_cells = sum(n), # total number of cells per buffer
      percentage = 100 * n / total_cells, # convert to percentages of land cover
      buffer_radius_m = buffers[i]
    ) %>%
    dplyr::ungroup()
  
  results_list[[i]] <- summary
  names(results_list)[i] <- buffers[i]
}

# Convert results into df and matrix
Canopy_centroid_deg_df <- as.data.frame(rbind(results_list[[1]], results_list[[2]], results_list[[3]], results_list[[4]]))

Canopy_centroid_deg_matrix <- Canopy_centroid_deg_df %>% 
  pivot_wider(names_from = c(Type, buffer_radius_m), values_from = percentage)

remove(Canopy_centroid_deg_df,Canopy_centroid_deg_matrix)
'

