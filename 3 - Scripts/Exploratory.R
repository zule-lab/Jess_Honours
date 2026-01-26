# 18-Jan-2026                          
# ============================================================================ # 
#                          EXPLORATION DATA ANALYSIS
# ============================================================================ # 

# DESCRIPTION: 
# Exploration of data and trends in data.

# PACKAGES USED:
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)



# ============================================================================ # 
# 1. SPECIES RICHNESS ACROSS SEASONS
# ============================================================================ # 

# --- 1.1 TOTAL NUMBER OF OBSERVATIONS --- #

# Function to count observations in data frame
num_observations <- function(cleaned_df){
  nrow(cleaned_df)
}

# 1.11 Total
cleaned_yard_data <- read_csv("2 - Cleaned/cleaned_yard_data.csv")
n_total = num_observations(cleaned_yard_data)

# 1.12 Migration
cleaned_migratory_data_2024 <- read_csv("2 - Cleaned/cleaned_migratory_data_2024.csv")
cleaned_migratory_data_2025 <- read_csv("2 - Cleaned/cleaned_migratory_data_2025.csv")
cleaned_migratory_data <- read_csv("2 - Cleaned/cleaned_migratory_data.csv")
n_mig24 = num_observations(cleaned_migratory_data_2024)
n_mig25 = num_observations(cleaned_migratory_data_2025)
n_mig = num_observations(cleaned_migratory_data)

# 1.13 Breeding
cleaned_breeding_data_2024 <- read_csv("2 - Cleaned/cleaned_breeding_data_2024.csv")
cleaned_breeding_data_2025 <- read_csv("2 - Cleaned/cleaned_breeding_data_2025.csv")
cleaned_breeding_data <- read_csv("2 - Cleaned/cleaned_breeding_data.csv")
n_bre24 = num_observations(cleaned_breeding_data_2024)
n_bre25 = num_observations(cleaned_breeding_data_2025)
n_bre = num_observations(cleaned_breeding_data)


# --- 1.2 OVERALL SPECIES RICHNESS --- #

# Function to find SR from presence-absence matrices
SR <- function(yard_wide){
  yard_wide <- select(yard_wide,-any_of(c("Code","NA", "Unknown"))
  )
  return(ncol(yard_wide))
}



# 1.21 Total
total_yard_wide <- read_csv("4 - Outputs/SR matrices/total_yard_wide.csv")
SR_total = SR(total_yard_wide)


# 1.22 Migration
m24_yard_wide <- read_csv("4 - Outputs/SR matrices/m24_visit_wide.csv")
m25_yard_wide <- read_csv("4 - Outputs/SR matrices/m25_visit_wide.csv")
m_yard_wide <- read_csv("4 - Outputs/SR matrices/m_visit_wide.csv")
SR_mig24 = SR(m24_yard_wide)
SR_mig25 = SR(m25_yard_wide)
SR_mig = SR(m_yard_wide)

# 1.23 Breeding
b24_yard_wide <- read_csv("4 - Outputs/SR matrices/b24_visit_wide.csv")
b25_yard_wide <- read_csv("4 - Outputs/SR matrices/b25_visit_wide.csv")
b_yard_wide <- read_csv("4 - Outputs/SR matrices/b_visit_wide.csv")
SR_bre24 = SR(b24_yard_wide)
SR_bre25 = SR(b25_yard_wide)
SR_bre = SR(b_yard_wide)



# --- 1.3 AVERAGE SPECIES RICHNESS PER YARD --- #

# Function to find average species richness per yard
avgSR_yard <- function(long_df,season){
  season_long = long_df[long_df$dataset==season,]
  return(mean(season_long$richness))
}

SR_long <- read_csv("4 - Outputs/SR_long.csv")

# 1.31 Total
avgSR_yard_total = avgSR_yard(SR_long, season="total")

# 1.32 Migration
avgSR_yard_m24 = avgSR_yard(SR_long, season="mig_2024")
avgSR_yard_m25 = avgSR_yard(SR_long, season="mig_2025")
avgSR_yard_m = avgSR_yard(SR_long, season="mig_total")

# 1.33 Breeding
avgSR_yard_bre24 = avgSR_yard(SR_long, season="breed_2024")
avgSR_yard_bre25 = avgSR_yard(SR_long, season="breed_2025")
avgSR_yard_bre = avgSR_yard(SR_long, season="breed_total")



# --- 1.4 AVERAGE SPECIES RICHNESS PER VISIT --- #

# Function to find average number of species seen in yard survey
avgSR_visit <- function(visit_wide) {
  visit_wide = select(visit_wide,-any_of(c(Code,Date,`NA`,Unknown))
  )
  richness = rowSums(visit_wide)
  return(mean(richness))
}

# 1.41 Total
total_visit_wide <- read_csv("4 - Outputs/SR matrices/total_visit_wide.csv")
avgSR_visit_total = avgSR_visit(total_visit_wide)


# 1.42 Migration
m24_visit_wide <- read_csv("4 - Outputs/SR matrices/m24_visit_wide.csv")
m25_visit_wide <- read_csv("4 - Outputs/SR matrices/m25_visit_wide.csv")
m_visit_wide <- read_csv("4 - Outputs/SR matrices/m_visit_wide.csv")
avgSR_visit_mig24 = avgSR_visit(m24_visit_wide)
avgSR_visit_mig25 = avgSR_visit(m25_visit_wide)
avgSR_visit_mig = avgSR_visit(m_visit_wide)

# 1.43 Breeding
b24_visit_wide <- read_csv("4 - Outputs/SR matrices/b24_visit_wide.csv")
b25_visit_wide <- read_csv("4 - Outputs/SR matrices/b25_visit_wide.csv")
b_visit_wide <- read_csv("4 - Outputs/SR matrices/b_visit_wide.csv")
avgSR_visit_bre24 = avgSR_visit(b24_visit_wide)
avgSR_visit_bre25 = avgSR_visit(b25_visit_wide)
avgSR_visit_bre = avgSR_visit(bre_visit_wide)




# ============================================================================ # 
# 2. YARD HABITAT FEATURES  
# ============================================================================ # 

# --- 187. YARD HABITAT FEATURES --- #

# 1.1 Find mean, range, and SD of the following:
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



# 1.2 Find abundance of plant species in yards

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



# --- 5. SPECIES-SPECIFIC DATA --- #

# 5.1. Species presence in each season


# 5.2 Frequency of presence (number of visits OR yards in which it was present)


# 5.3 Number of yards in which it was present





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




