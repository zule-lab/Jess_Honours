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
library(stringr)


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





# ============================================================================ #
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





# ============================================================================ #
# 3.  CLEANING DATA EXTRACTED FROM INDICE CANOPEE RASTER                                
# ============================================================================ #

# --- 1.1 LANDSCAPE CHARACTERISTICS DATA FRAME PREPARATION --- #

# VARIABLES FOR DF
#   Yard.Code
#   Variable 4 (canopy cover below 3 m)
#   Variable 5 (canopy cover above 3 m)
#   Season
#   Year
#   Richness



# 1.11 Remove unnecessary variables from canopy_output data frame
canopy_output <- read_csv("1 - Input/canopy_output.csv")

canopy_cleaned <- canopy_output %>%
  # select class 3 (canopy below 3 m) and class 4 (canopy above 3 m) variables
  select(Yard.Code, starts_with("indice660_class3"), starts_with("indice660_class4")) %>%
  # remove indice660_ prefix of columns
  rename_with(~ str_remove(.x,"^indice660_")) %>%
  # rename class3 columns (canopy below 3 m) for clarity
  rename_with(~ str_replace(.x, "^class3_", "low_canopy_"), .cols = contains("class3")) %>%
  # rename class4 columns (canopy above 3 m) for clarity
  rename_with(~ str_replace(.x, "^class4_", "high_canopy_"), .cols = contains("class4"))



# 1.12 Bind species richness data for each period with canopy data
SR_wide <- read_csv("2 - Cleaned/SR_wide.csv")

# Create landscape characteristics data frame
landscape_characteristics <- Reduce(
  function(x, y) full_join(x, y, by = "Yard.Code"),
  list(canopy_cleaned,SR_wide))

write.csv(landscape_characteristics, "2 - Cleaned/landscape_characteristics.csv", row.names = FALSE)


# 1.13 Make split landscape data frame by splitting season and year
landscape_characteristics_split <- landscape_characteristics %>%
  # remove unnecessary SR variables
  select(-c(SR_total, SR_mig_2024, SR_mig_2025, SR_breed_2024, SR_breed_2025)) %>%
  # split richness and season into different columns
  pivot_longer(
    cols = starts_with("SR_"), # select columns starting with SR_
    names_to = "season", # name of new column to store old column names
    values_to = "richness") %>% # name of new column to store values
  # rename seasons for clarity
  mutate(season = case_when(str_detect(season,"SR_mig") ~ "migration",
                            str_detect(season, "SR_breed") ~ "breeding"))

write.csv(landscape_characteristics_split, "2 - Cleaned/landscape_characteristics_split.csv", row.names = FALSE)


# 1.14 Make long landscape characteristics data frame by stacking scales
landscape_characteristics_long <- landscape_characteristics %>%
  pivot_longer(
    cols = contains("canopy"), # Holds Yard.Code and SR columns the same
    names_to = "Type_Scale", # Stores column names (e.g., high_canopy_25m)
    values_to = "Cover" # Values of new column are vegetation cover values
  )

write.csv(landscape_characteristics_long, "2 - Cleaned/landscape_characteristics_long.csv", row.names = FALSE)



