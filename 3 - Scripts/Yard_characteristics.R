# 07-Jan-2026
# ============================================================================ # 
#                        YARD CHARACTERISTIC DATA FRAME
# ============================================================================ # 

# DESCRIPTION: 
# Extraction of habitat features of each yard and compilation into 
# a single data frame called yard_characteristics.

# PACKAGES USED:
library(readr)
library(tidyverse)

# PROBLEMS I am currently having:
# *area: how do I get the conversion to Kayleigh's yards?
# **yards: Y50 is missing from the tree data, Y25 is missing from centroid data
# ***trees/shrubs: is the best way to differentiate them by stem number?
# ****dbh: should i include shrubs in average DBH?
# ******fruit: what are the fruiting species?

# ============================================================================ # 
# 1. YARD MEASUREMENTS
# ============================================================================ # 
# --- 1.1 CENTROID POINTS ** --- #
# DEF: Find the middle-most point in each backyard in decimal degrees and UTM. 
# (Note: front yards were not considered in this calculation because this would 
# complicate the scaled calculations of yard features and so few of the 
# observations occurred in front yards anyway.)

# Centroid points were calculated using Google Maps by: 
    # creating quadrilateral encircling the backyard
    # finding midpoint by drawing two diagonals
    # (also calculated shortest and longest radii from the centroid within the yard)

# Import data here:
centroid_data <- read_csv("~/Desktop/Jess_Honours/1 - Input/centroid_data.csv")
# remove extra row that appeared in import
centroid_data <- centroid_data[rowSums(is.na(centroid_data)) != ncol(centroid_data), ]



# --- 1.2 AREA * ** --- #
# DEF: Yard area calculated using Google Maps' Polygon Tool by visually drawing 
# quadrilateral around yard. Included in centroid_data.
# Area should also be found in Kayleigh's work, as this would also include front yards.
  # NOTE: need Mackenzie's list of yards to accomplish this and compare
back_area <- subset(centroid_data, select = c(Yard.Code, back_area_ha))




# ============================================================================ # 
# 2. TREES AND SHRUBS: COUNT, DENSITY, DBH, & FRUITS
# ============================================================================ # 

##### PROBLEM: NOA'S YARD HAS NO TREES OR SHRUBS??? ASK MACKENZIE
##### ALSO: IS THE BEST WAY TO DIFFERENTIATE TREES AND SHRUBS BY THE NUMBER OF STEMS???

# --- 2.1 NUMBER OF TREES AND SHRUBS PER YARD --- # ** ***
# DEF: Count the number of trees (1 stem) and shrubs (>1 stem) in yards from 
# yard_trees_verified data frame.

# Import yard_trees_verified:
yard_plants_verified <- read_csv("~/Desktop/Jess_Honours/1 - Input/yard_trees_verified.csv")

# Write and use function that counts number of trees and shrubs in each yard
count_trees_shrubs <- function(plant_df) {
  plant_df %>%
    # classify trees and shrubs by number of stems
    mutate(plant_type = if_else(Number.stems == 1, "tree_count", "shrub_count")) %>%
    # treat each combination of yard and plant type as its own group
    group_by(Yard.Code, plant_type) %>%
    # collapse each group into one row
    summarise(n = n(), .groups = "drop") %>% # n: row count, "drop" removes grouping structure
    # reshape to yard-level
    pivot_wider(
      names_from  = plant_type, # becomes column names
      values_from = n, # fills column with n values
      values_fill = 0 # inserts 0 if there's no trees/shrubs
    )
}
tree_shrub_count <- count_trees_shrubs(yard_plants_verified)



# --- 2.2 TREE AND SHRUB DENSITY --- #
# DEF: Calculated the density of tree and shrubs in backyards based on their 
# count yard area.

# Join back_area with tree_shrub_count for density calculation
area_and_veg_df <- back_area %>%
  left_join(tree_shrub_count, by = c("Yard.Code" = "Yard.Code"))

# Write & use function to calculate density (count/area)
calc_tree_shrub_density <- function(data, area_col = back_area_ha) {
  data %>%
    mutate(
      tree_density  = tree_count  / {{ area_col }},
      shrub_density = shrub_count / {{ area_col }}
    )
}
density_df <- calc_tree_shrub_density(area_and_veg_df)

# Remove the back_area_ha, shrub, and tree columns
density_df <- density_df %>%
  select(-back_area_ha, -tree_count, -shrub_count)



# --- 2.3 AVERAGE DBH --- # ** ****
# DEF: Find the average DBH for trees, shrubs, and all plants in each yard from 
# the DBHs in yard_plants_verified

# Function that finds DBH of trees, shrubs, and all plants from data frame
mean_dbh_by_yard <- function(plant_df) {
  plant_df %>%
    # Sort each row as a tree or shrubs based on number of stems
    mutate(plant_type = if_else(Number.stems == 1, "tree", "shrub")) %>%
    # Group by yards
    group_by(Yard.Code) %>%
    # Find DBHs for all plants, trees, and shrubs
    summarise(
      mean_dbh_all = mean(DBH, na.rm = TRUE),
      
      mean_dbh_tree = if_else( # If trees are present, find mean DBH
        sum(plant_type == "tree") > 0,
        mean(DBH[plant_type == "tree"], na.rm = TRUE),
        NA_real_ # Else, NA
      ),
      
      mean_dbh_shrub = if_else( # If shrubs are present, find mean DBH
        sum(plant_type == "shrub") > 0,
        mean(DBH[plant_type == "shrub"], na.rm = TRUE),
        NA_real_ # Else, NA
      ),
      
      .groups = "drop"
    )
}
mean_DBH_df <- mean_dbh_by_yard(yard_plants_verified)



# --- 2.4 NUMBER OF FRUITING PLANTS--- # ******
# DEF: Calculate the number of fruiting plants in each yard.

# Using the species listed in yard_plants_verified, determine which species are fruiting:
unique(yard_plants_verified$Plant.sci) # get list of species

# Determine which ones are fruiting based on research (Missouri Botanical Garden)
fruiting_sp <- c("Amelanchier canadensis","Celtis occidentalis","Euonymus alatus",
                 "Berberis thunbergii","Lonicera xylosteum","Magnolia soulangeana",
                 "Malus baccata","Malus domestica","Malus sylvestris","Morus alba",
                 "Prunus cerasus","Prunus communis","Prunus japonica","Pyrus communis",
                 "Rhamnus cathartica","Sambucus nigra","Sorbus aucuparia","Taxus canadensis",
                 "Tilia cordata") 
                # Magnolia fruits in late summer/fall. 
                # Check Tilia again (fruits don't seem to be good for birds)

fruiting_sp <- c("Amelanchier canadensis", # ripe in June
                 "Berberis thunbergii", # ripe in October and through winter
                 "Lonicera xylosteum", # ripe in July and August
                 "Malus baccata", # 
                 "Malus domestica",
                 "Malus sylvestris",
                 "Morus alba", # ripe June to July
                 "Prunus cerasus",#cerasifera, ripe June, July, August
                 "Prunus communis",
                 "Prunus japonica",
                 "Pyrus communis",
                 "Rhamnus cathartica",
                 "Sambucus nigra",
                 "Sorbus aucuparia",
                 "Taxus canadensis",
                 "Tilia cordata") # nutlet
# "Euonymus alatus" fruits in fall
# "Celtis occidentalis", nutlets persisting through fall
# "Magnolia soulangeana" fruits in late summer/fall - and rarely and follicles
# Check Tilia again (fruits don't seem to be good for birds)

# Count the number of fruiting species in each yard:
fruiting_count_by_yard <- yard_plants_verified %>%
  mutate(is_fruiting = Plant.sci %in% fruiting_sp) %>%
  group_by(Yard.Code) %>%
  summarise(
    n_fruiting_plants = sum(is_fruiting),
    .groups = "drop"
  )




# ============================================================================ # 
# 3. BIRD SPECIES RICHNESS
# ============================================================================ # 
# DEF: Take the bird species richness measures for each yard calculated in SR.R 
# script and stored in "SR_long.csv" and convert them to a wide data frame.

# Import SR_long.csv for species richnesses
SR_long <- read_csv("3 - Extraction/SR_long.csv")

# Convert into a wide data frame with the datasets as the columns and yards as rows
richness_wide <- SR_long %>%
  pivot_wider(
    id_cols    = Code,
    names_from = dataset,
    values_from = richness
  )

# Rename Code column to Yard.Code for consistency, and add SR to richness columns
# for clarity
richness_wide <- richness_wide %>% 
  rename(Yard.Code = Code,
         SR_total = total,
         SR_mig_2024 = mig_2024,
         SR_mig_2025 = mig_2025,
         SR_mig = mig_total,
         SR_breed_2024 = breed_2024,
         SR_breed_2025 = breed_2025,
         SR_breed = breed_total)




# ============================================================================ # 
# 4. WRITE & EXPORT
# ============================================================================ # 
# Def: write and export yard_characteristics.csv by binding the following data 
# frames by yard codes:
  # centroid & area: centroid_data
  # number of trees and shrubs: tree_shrub_count
  # tree and shrub density: density_df
  # average DBH: mean_DBH_df
  # number of big trees: count_big_trees
  # number of fruiting plants: fruiting_count_by_yard
  # bird SR across seasons: richness_wide

yard_characteristics <- Reduce(
  function(x, y) full_join(x, y, by = "Yard.Code"),
  list(
    centroid_data,
    tree_shrub_count,
    density_df,
    mean_DBH_df,
    count_big_trees,
    fruiting_count_by_yard,
    richness_wide
  )
)

# Export yard_characteristics data frame
write.csv(yard_characteristics, file="yard_characteristics.csv", row.names=FALSE)







# not used
##### 6. NUMBER OF BIG TREES ** *** ***** #####
# DEF: Find the number of trees in yard_plants_verified with a DBH greater than 
# a threshold. 

#Threshold determined based on avian ecology (30-60cm)
# https://link-springer-com.proxy3.library.mcgill.ca/article/10.1007/s11676-024-01714-w#:~:text=Based%20on%20decision%20tree%20modelling,trees%20over%2010%20cm%20DBH.
# "We wanted the model to reﬂectthe mean diameter of the cavity limb (21.6 cm; Jackson, 1976)so only included trees greater than 23 cm dbh and adjusted the densities to reﬂect these conditions"
# From: https://www-sciencedirect-com.proxy3.library.mcgill.ca/science/article/pii/S0169204613002077?via%3Dihub

# Subset yard_plants_verified to include only trees (i.e., 1 stem)
yard_trees_verified <- yard_plants_verified[yard_plants_verified$Number.stems == 1,]

# Examine distribution of DBH values
summary(yard_trees_verified) 
# extract mean, median, 1st and 3rd quartiles
mean_val = 21.04
median_val = 14.50
first_val = 8.00
third_val = 27.25
# plot histgram
ggplot(data = yard_trees_verified, aes(x = DBH)) +
  geom_histogram(binwidth = 2) +  
  geom_vline(aes(xintercept = mean_val), color = "red", linetype = "solid", size = 0.5) + 
  geom_vline(aes(xintercept = median_val), color = "blue", linetype = "solid", size = 0.5) + 
  geom_vline(aes(xintercept = first_val), color = "purple", linetype = "solid", size = 0.5) + 
  geom_vline(aes(xintercept = third_val), color = "purple", linetype = "solid", size = 0.5) + 
  theme_bw()
# threshold will be 45 because that is generally considered big tree


# Find number of trees with DBH greater than X in each yard:
count_big_trees <- yard_trees_verified %>%
  group_by(Yard.Code) %>%
  summarise(
    n_big_trees = sum(DBH > 45, na.rm = TRUE), # threshold is 45
    .groups = "drop"
  )
