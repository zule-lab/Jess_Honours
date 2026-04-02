# 04-Feb-2026                          
# ============================================================================ # 
#                             COLLINEARITY MATRICES
# ============================================================================ # 

# DESCRIPTION: 
# Creating a correlation matrix to asses the collinearity of my yard-level and 
# landscape-level habitat features

# PACKAGES USED:
library(readr)
library(dplyr)
library(corrplot)


# ============================================================================ # 
# 1. YARD HABITAT FEATURES  
# ============================================================================ # 

# --- 1.1 CORRELATION MATRIX --- #

# 1.11 Prepare yard_characteristics to include only yard habitat features
# Import data
yard_characteristics <- read_csv("2 - Cleaned/yard_characteristics.csv")

# Remove yard ID, centroid data, and SR columns
yard_features_only <- select(yard_characteristics, 
                                c(area,tree,shrub,density,mean_dbh,
                                  proportion_fruit,proportion_native))


# 1.12 Create correlation matrices
# Numerical correlation matrix
cor_matrix_yard <- cor(yard_features_only) # numerical correlation matrix
cor_matrix_yard
corrplot(cor_matrix_yard) # visual correlation matrix

# Conclusions: should remove density due to high correlation with trees and shrubs



# ============================================================================ # 
# 2. LANDSCAPE HABITAT FEATURES  
# ============================================================================ # 

# --- 2.1 CORRELATION MATRIX --- #

# 2.11 Prepare landscape_characteristics to include only yard habitat features
# Import data
landscape_characteristics <- read_csv("2 - Cleaned/landscape_characteristics.csv")

# Remove yard ID, centroid data, and SR columns
landscape_features_only <- landscape_characteristics %>% 
  select(-Yard.Code, -SR_total, -SR_mig, -SR_mig_2024, -SR_mig_2025, -SR_breed, -SR_breed_2024, -SR_breed_2025)


# 2.12 Create correlation matrices
# Numerical correlation matrix
cor_matrix_landscape <- cor(landscape_features_only) # numerical correlation matrix
cor_matrix_landscape
corrplot(cor_matrix_landscape) # visual correlation matrix

# Conclusions:
# - low canopy has high collinearity with everything
# - high canopy also does
# - the only acceptable variable is high_canopy_25m
# This is probably because the buffers have a lot of spatial overlap with one another








