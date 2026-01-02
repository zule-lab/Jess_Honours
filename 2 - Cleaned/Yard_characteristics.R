# 03-Dec-2025           CREATING YARD CHARACTERISTIC DATASET

# DESCRIPTION: Extraction of data pertaining to each yard and compilation into 
# a single data frame called yard_characteristics. In other words, calculation 
# of yard features.

library(readr)
library(dplyr)


##### 1. CENTROID POINT ######
# Find the middle-most point in each backyard in decimal degrees and UTM. 
# (Note: front yards were not considered in this calculation because this would 
# complicate the scaled calculations of yard features and so few of the 
# observations occurred in front yards anyway.)
# Centroid points were calculated using Google Maps by: 
    # creating quadrilateral encircling the backyard
    # finding midpoint by drawing two diagonals
    # (also calculated shortest and longest radii from the centroid within the yard)
# Import data here:
centroid_data <- read_csv("~/Desktop/Jess_Honours/1 - Input/centroid_data.csv")


###### 2. AREA ######
# Yard area calculated using Google Maps' Polygon Tool by visually drawing 
# quadrilateral around yard.
# Area should also be found in Kayleigh's work, as this would also include front yards.
  # NOTE: need Mackenzie's list of yards to accomplish this
# Extract area from centroid data:
back_area <- centroid_data[centroid_data$back_area_ha] #FIX

##### 3. NUMBER OF TREES AND SHRUBS PER YARD #####
# Count the number of trees (1 stem) and shrubs (>1 stem) in yards from 
# yard_trees_verified data frame.
# Import yard_trees_verified:
yard_trees_verified <- read_csv("~/Desktop/Jess_Honours/1 - Input/yard_trees_verified.csv")

# Number of trees per yard:


# Number of shrubs per yard




##### 4.TREE AND SHRUB DENSITY #####
# Calculated the density of tree and shrubs in backyards based on their count yard area.

# Write function



##### 5. AVERAGE DBH #####
# Find the average DBH for the plants in each yard from the DBHs in yard_trees_verified.

# Sum the DBHs & divide by the number of stems for each yard:




##### 6. NUMBER OF BIG TREES #####
# Find the number of trees in yards_trees_verified with a DBH greater than INSERT threshold.

# Extract only tree data from yards_trees_verified:

# Create histogram to explore the range of DBHs to find threshold X:

# Find number of trees with DBH greater than X:




##### 7. NUMBER OF FRUITING PLANTS #####
# Determine the number of fruiting plants in each yard.

# Using the species listed in yard_trees_verified, determine which species are fruiting:

# Count the number of fruiting species in each yard:



##### 8. BIRD SR (MIGRATORY & BREEDING) #####
# Import bird data: cleaned_yard_data.csv, cleaned_migratory_data.csv, cleaned_breeding_data.csv
# Find SR in yards in migration 2024

# Find SR in yards in migration 2025

# Find SR in yards in migration total

# Find SR in yards in breeding 2024

# Find SR in yards in breeding 2025

# Find SR in yards in 2024

# Find SR in yards in 2025

# Find SR in yards total





##### Create & export a new dataset called yard_characteristics with these values





