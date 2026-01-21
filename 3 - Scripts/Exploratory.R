# 18-Jan-2026                          
# ============================================================================ # 
#                                 DATA EXPLORATION
# ============================================================================ # 

# DESCRIPTION: 
# Exploration of data and trends in data.

# PACKAGES USED:
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)


# ============================================================================ # 
# SUMMARY STATISTICS  
# ============================================================================ # 

cleaned_breeding_data_2024 <- read_csv("4 - Cleaned/cleaned_breeding_data_2024.csv")
cleaned_breeding_data_2025 <- read_csv("4 - Cleaned/cleaned_breeding_data_2025.csv")
cleaned_breeding_data <- read_csv("4 - Cleaned/cleaned_breeding_data.csv")
cleaned_migratory_data_2024 <- read_csv("4 - Cleaned/cleaned_migratory_data_2024.csv")
cleaned_migratory_data_2025 <- read_csv("4 - Cleaned/cleaned_migratory_data_2025.csv")
cleaned_migratory_data <- read_csv("4 - Cleaned/cleaned_migratory_data.csv")
cleaned_yard_data <- read_csv("4 - Cleaned/cleaned_yard_data.csv")
SR_long <- read_csv("4 - Outputs/SR_long.csv")
total_visit_wide <- read_csv("4 - Outputs/SR matrices/total_visit_wide.csv")
b24_visit_wide <- read_csv("4 - Outputs/SR matrices/b24_visit_wide.csv")
b25_visit_wide <- read_csv("4 - Outputs/SR matrices/b25_visit_wide.csv")
b_visit_wide <- read_csv("4 - Outputs/SR matrices/b_visit_wide.csv")
m24_visit_wide <- read_csv("4 - Outputs/SR matrices/m24_visit_wide.csv")
m25_visit_wide <- read_csv("4 - Outputs/SR matrices/m25_visit_wide.csv")
m_visit_wide <- read_csv("4 - Outputs/SR matrices/m_visit_wide.csv")

# --- 1. YARD HABITAT FEATURES --- #

#### 1.1 Find mean, range, and SD of the following: ####
      # •	Average area
      # •	Number of trees
      # •	Number of shrubs
      # •	Tree density
      # •	Shrub density
      # •	Average DBH
      # •	Number of big trees
      # •	Number of fruiting plants

# Import data
yard_characteristics <- read_csv("4 - Output/yard_characteristics.csv") #not there yet

# Select only relevant columns from yard_characteristics
yard_features <- yard_characteristics %>% 
  select(back_area_ha,shrub_count,tree_count,shrub_density,tree_density,
         mean_dbh_all,mean_dbh_shrub,mean_dbh_tree,n_fruiting_plants)

# Calculate n, mean, SD, and range for yard features and create new data frame
summary_yard_features <- as.data.frame( # return data frame, not matrix
  rbind( # count non-NA observations for n, and remove NA observations for each variable
    n    = colSums(!is.na(yard_features)), # count number of valid observations
    mean = apply(yard_features, 2, function(x) mean(x, na.rm = TRUE)), 
    sd   = apply(yard_features, 2, function(x) sd(x, na.rm = TRUE)), 
    min  = apply(yard_features, 2, function(x) min(x, na.rm = TRUE)), 
    max  = apply(yard_features, 2, function(x) max(x, na.rm = TRUE)) 
  )
)


#### 1.2 Find abundance of plant species in yards ####

# Import data
yard_trees_verified <- read_csv("1 - Input/yard_trees_verified.csv")

# Abundance of plant species by counting frequency of species code
species_abundance <- yard_trees_verified %>%
  count(Species.Code, Plant.sci, name = "abundance")

# Calculate percentage of total records
species_abundance <- species_abundance %>%
  mutate(percent = 100 * abundance / sum(abundance))

# Rank species by abundance
species_abundance <- species_abundance %>%
  arrange(desc(abundance)) %>%
  mutate(rank = row_number())

# Bar plot
ggplot(species_abundance,
       aes(x = reorder(Plant.sci, -abundance),
           y = abundance)) +
  geom_col() +
  labs(x = "Species", y = "Abundance (number of observations)",
       title = "Plant species abundance across backyards") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# why does ULPU do that :'(

# Rank-abundance curve
ggplot(species_abundance,
       aes(x = rank, y = abundance)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  labs(
    x = "Species rank",
    y = "Number of observations (log scale)",
    title = "Rank–abundance distribution"
  ) +
  theme_bw()






total_yard_wide <- read_csv("4 - Outputs/SR matrices/total_yard_wide.csv")



# --- 2. SPECIES RICHNESS OVERALL --- #

#### .1 Total number of observations
# Load observation data

# Count number of rows


#### 2.2 Species richness ####
# Load presence-absence matrix for birds species in yards

# SR is number of columns (minus NA and Unknown)


# 2.3 Species frequency of presence in yards
# Load presence-absence matrix for birds species in yards

# Calculate frequency of presence for each species


# 2.4 Average number of species per yard
# Load species richness per yard long data frame

# Take mean of species richness column values for the season


# 2.5 Average number of species per visit
# Load presence-absence matrix for bird species per visit

# Find species richness per visit

# Take mean of species richness column values for the season




# --- 3. SPECIES RICHNESS MIGRATION --- #

#### 3.1 Total number of observations
# Load observation data

# Count number of rows

#### 3.2 Species richness ####
# Load presence-absence matrix for birds species in yards

# SR is number of columns (minus NA and Unknown)


# 3.3 Species frequency of presence in yards
# Load presence-absence matrix for birds species in yards

# Calculate frequency of presence for each species


# 3.4 Average number of species per yard
# Load species richness per yard long data frame

# Take mean of species richness column values for the season


# 3.5 Average number of species per visit
# Load presence-absence matrix for bird species per visit

# Find species richness per visit

# Take mean of species richness column values for the season




# --- 4. SPECIES RICHNESS BREEDING --- #

#### 4.1 Total number of observations
# Load observation data

# Count number of rows

#### 4.2 Species richness ####
# Load presence-absence matrix for birds species in yards

# SR is number of columns (minus NA and Unknown)


# 4.3 Species frequency of presence in yards
# Load presence-absence matrix for birds species in yards

# Calculate frequency of presence for each species


# 4.4 Average number of species per yard
# Load species richness per yard long data frame

# Take mean of species richness column values for the season


# 4.5 Average number of species per visit
# Load presence-absence matrix for bird species per visit

# Find species richness per visit

# Take mean of species richness column values for the season




# --- 5. SPECIES-SPECIFIC DATA --- #

# 5.1. Species presence in each season


# 5.2 Frequency of presence (number of visits OR yards in which it was present)


# 5.3 Number of yards in which it was presnde




# ============================================================================ # 
# LANDSCAPE HABITAT FEATURES  
# ============================================================================ # 

# --- 6. insert --- #







# ============================================================================ # 
# DISTRIBUTION OF VARIABLES  
# ============================================================================ # 

# --- X. SPECIES RICHNESS --- #

# X.1 Migration
# 2024

# 2025

# Both


# X.2 Breeding
# 2024

# 2025

# Both


# --- X. YARD CHARACTERISTICS --- #
yard_characteristics <- read_csv("yard_characteristics.csv")
# X.1 Area


# X.2 Number of trees


# X.3 Number of shrubs


# X.4 Tree density


# X.5 Shrub density


# X.6 Average DBH


# X.7 Number of fruiting plants



# --- X. LANDSCAPE CHARACTERISTICS --- #

# INSERT


# ============================================================================ # 
# RELATIONSHIPS TO SPECIES RICHNESS
# ============================================================================ # 

# --- X. SR ~ YARD HABITAT FEATURES --- #
# Linear ggplot relationships


# --- X. SR ~ LANDSCAPE HABITAT FEATURES --- #
# Linear ggplot relationships



# --- X. SR ~ LANDSCAPE HABITAT FEATURES --- #
# Linear ggplot relationships




# ============================================================================ # 
# TESTS
# ============================================================================ # 


###### TESTS FOR NORMALITY

###### TESTS FOR COLLINEARITY




