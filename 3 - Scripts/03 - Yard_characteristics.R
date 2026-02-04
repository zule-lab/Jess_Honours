# 03-Feb-2026
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
# **yards: Y50 is missing from the tree data


# ============================================================================ # 
# 1. YARD MEASUREMENTS
# ============================================================================ # 

# --- 1.1 CENTROID POINTS --- #

# DEF: Find the middle-most point in each backyard in decimal degrees and UTM. 
# (Note: front yards were not considered in this calculation because this would 
# complicate the scaled calculations of yard features and so few of the 
# observations occurred in front yards anyway.)

# Centroid points were calculated using Google Maps by: 
    # creating quadrilateral encircling the backyard
    # finding midpoint by drawing two diagonals
    # (also calculated shortest and longest radii from the centroid within the yard)

# Import data here:
centroid_data <- read_csv("2 - Cleaned/centroid_data.csv")
# remove extra rows and columns that appeared in import
centroid_data <- centroid_data[rowSums(is.na(centroid_data)) != ncol(centroid_data), 
                               colSums(is.na(centroid_data)) != nrow(centroid_data)]



# --- 1.2 AREA ** --- #

# DEF: Backyard area calculated using Google Maps' Polygon Tool by visually drawing 
# quadrilateral around yard, and total yard area (front yard included) from
# Kayleigh's work.
back_area <- subset(centroid_data, select = c(Yard.Code, area, back_area_ha))



# ============================================================================ # 
# 2. TREES AND SHRUBS: COUNT, DENSITY, DBH, & FRUITS
# ============================================================================ # 

##### PROBLEM: NOA'S YARD HAS NO TREES OR SHRUBS??? ASK MACKENZIE

# --- 2.1 NUMBER OF TREES AND SHRUBS PER YARD --- # ** ***

# DEF: Count the number of trees and shrubs in yards from yard_trees_verified
# I differentiated between trees and shrubs based on species, size, and stem number.
# In particular, SYVU was defined as a tree if its DBH > 10 and stem = 1, and 
# THOC was defined as a tree if DBH > 20 and stem = 1. 

# Import yard_trees_verified:
yard_trees_verified <- read_csv("2 - Cleaned/yard_trees_verified.csv")

# Write and use function that counts number of trees and shrubs in each yard
count_trees_shrubs <- function(plant_df) {
  plant_df %>%
    # treat each combination of yard and plant type as its own group
    group_by(Yard.Code, Type) %>%
    # collapse each group into one row
    summarise(n = n(), .groups = "drop") %>% # n: row count, "drop" removes grouping structure
    # reshape to yard-level
    pivot_wider(
      names_from  = Type, # becomes column names
      values_from = n, # fills column with n values
      values_fill = 0 # inserts 0 if there's no trees/shrubs
    )
}
tree_shrub_count <- count_trees_shrubs(yard_trees_verified)



# --- 2.2 TREE AND SHRUB DENSITY --- #

# DEF: Calculated the density of vegetation (tree + shrubs) in backyards based on 
# their area.

# Join back_area with tree_shrub_count for density calculation
area_and_veg_df <- back_area %>%
  left_join(tree_shrub_count, by = c("Yard.Code" = "Yard.Code"))

# Write & use function to calculate density (count/area)
calc_density <- function(data, area_col=area) {
  data %>%
    mutate(
      density = (tree + shrub) / {{ area_col }}
    )
}
density_df <- calc_density(area_and_veg_df, area_col=area)

# Remove the back_area_ha, shrub, and tree columns
density_df <- density_df %>%
  select(-back_area_ha, -tree, -shrub, -area)



# --- 2.3 AVERAGE DBH --- # ** ****

# DEF: Find the average DBH for trees, shrubs, and all plants in each yard from 
# the DBHs in yard_trees_verified

# Function that finds DBH of trees, shrubs, and all plants from data frame
mean_dbh_by_yard <- function(plant_df) {
  plant_df %>%
    mutate(
      # Convert any non-numeric values
      DBH = as.numeric(DBH),
      Number.stems = as.numeric(Number.stems),
      adjusted_DBH = DBH/Number.stems) %>%
    # Remove non-numeric values
    filter(!is.na(adjusted_DBH),Number.stems>0) %>%
    # Group by yards
    group_by(Yard.Code) %>%
    # Find DBHs for all plants, trees, and shrubs
    summarise(
      mean_dbh = mean(adjusted_DBH, na.rm=TRUE),
      .groups="drop"
    )
}
mean_DBH_df <- mean_dbh_by_yard(yard_trees_verified)



# --- 2.4 NUMBER OF FRUITING PLANTS--- # ******

# DEF: Calculate the number of fruiting plants in each yard.
# Information gathered from "Manual of Woody Landscape Plants" by Michael A. Dirr
# Plants are considered fruiting if they produce fleshy fruit during spring or summer

# Using the species listed in yard_trees_verified, determine which species are fruiting:
unique(yard_trees_verified$Plant.sci) # get list of species

# Create function that counts fruiting plants in each season and overall per yard
fruiting_count_by_yard <- function(plant_df){
  plant_df %>%
    mutate(
      is_fruiting = Fleshy == "yes" &
        Fruit_season %in% c("spring", "summer")
    ) %>%
    group_by(Yard.Code) %>%
    summarise(
      n_fruiting_plants = sum(is_fruiting, na.rm = TRUE),
      .groups = "drop"
    )
}
fruiting_df <- fruiting_count_by_yard(yard_trees_verified)



# --- 2.5 NATIVITY--- #

# DEF: Calculate the number of native tree and shrub species are in each yard.
native_count_by_yard <- function(plant_df){
  plant_df %>%
    mutate(is_native = Native == "yes") %>%
    group_by(Yard.Code) %>%
    summarise(
      n_native_plants = sum(is_native, na.rm = TRUE),
      .groups = "drop"
    )
}
native_df <- native_count_by_yard(yard_trees_verified)



# ============================================================================ # 
# 3. BIRD SPECIES RICHNESS
# ============================================================================ # 

# DEF: Take the bird species richness measures for each yard calculated in SR.R 
# script and stored in "SR_long.csv" and convert them to a wide data frame.

# Import SR_long.csv for species richnesses
SR_long <- read_csv("2 - Cleaned/SR_long.csv")

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

# Create yard_characteristics data frame
yard_characteristics <- Reduce(
  function(x, y) full_join(x, y, by = "Yard.Code"),
  list(
    centroid_data,
    tree_shrub_count,
    density_df,
    mean_DBH_df,
    fruiting_df,
    native_df,
    richness_wide
  )
)

# Export yard_characteristics data frame
write.csv(yard_characteristics, file="2 - Cleaned/yard_characteristics.csv", row.names=FALSE)




