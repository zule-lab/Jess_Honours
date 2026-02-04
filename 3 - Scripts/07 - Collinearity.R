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
                                c(area,back_area_ha,tree,shrub,density,mean_dbh,
                                  n_fruiting_plants,n_native_plants))


# 1.12 Create correlation matrices
# Numerical correlation matrix
cor_matrix_yard <- cor(yard_features_only) # numerical correlation matrix
cor_matrix_yard

corrplot(cor_matrix_yard) # visual correlation matrix



# ============================================================================ # 
# 2. LANDSCAPE HABITAT FEATURES  
# ============================================================================ # 

# --- 2.1 CORRELATION MATRIX --- #

#cor_matrix_landscape