# 03-Dec-2025           CREATING YARD CHARACTERISTIC DATASET

library(readr)
library(dplyr)
yard_trees <- read_csv("~/Desktop/Jess_Honours/1 - Input/yard_trees_verified.csv")


# The following metrics need to be calculated for each yard, then exported


##### 1. CENTROID POINT ######
# Use Google maps or Earth



###### 2. AREA ######
# Find from Kayleigh's work



##### 3. NUMBER OF TREES AND TREE DENSITY #####
# Split into trees and shrubs (by stem number)
# Count number of trees per yard






##### 4. NUMBER OF SHRUBS AND SHRUB DENSITY #####
# Split into trees and shrubs (by stem number)
# Count number of shrubs per yard



##### 5. NUMBER OF BIG TREES #####
# Create histogram to explore the range of DBHs to find threshold
# Number of trees with DBH greater than X



##### 6. NUMBER OF FRUITING PLANTS #####
# Create list of vegetation species
# Determine which ones are fruiting through research
# Count number of those species for each yard



##### 7. BIRD SR (MIGRATORY & BREEDING) #####
# Import bird data: cleaned_yard_data.csv, cleaned_migratory_data.csv, cleaned_breeding_data.csv
# Find SR in yards in migration 2024

# Find SR in yards in migration 2025

# Find SR in yards in migration total

# Find SR in yards in breeding 2024

# Find SR in yards in breeding 2025

# Find SR in yards in 2024

# Find SR in yards in 2025

# Find SR in yards total





##### Create & export a new dataset with these values





