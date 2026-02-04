# 03-Feb-2026                          
# ============================================================================ # 
#                          EXPLORATORY DATA ANALYSIS
# ============================================================================ # 

# DESCRIPTION: 
# Exploration and visualization of distributions and trends of variables

# PACKAGES USED:
library(readr)
library(ggplot2)
library(dplyr)


# ============================================================================ # 
# DISTRIBUTION OF VARIABLES  
# ============================================================================ # 

# --- 1. SPECIES RICHNESS ACROSS YARDS --- #

yard_characteristics <- read_csv("4 - Outputs/yard_characteristics.csv")
SR_long <- read_csv("4 - Outputs/SR_long.csv")

ggplot(data = SR_long) + 
  geom_boxplot(mapping = aes(x = dataset, y=richness, fill = dataset)) 

# 1.1 Migration
# 2024
ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_mig_2024), binwidth = 0.60)
       
# 2025

# Both


# 1.2 Breeding
# 2024

# 2025

# Both


# --- 2. YARD CHARACTERISTICS --- #
yard_characteristics <- read_csv("yard_characteristics.csv")
# 2.1 Area


# 2.2 Number of trees


# 2.3 Number of shrubs


# 2.4 Density


# 2.5 Average DBH


# 2.6 Number of fruiting plants


# 2.7 Number of native plants



# --- 3. LANDSCAPE CHARACTERISTICS --- #

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




