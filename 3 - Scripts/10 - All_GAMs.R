# 2-Apr-2026                          
# ============================================================================ # 
#                              FINAL MODEL SELECTION
# ============================================================================ # 

# DESCRIPTION: 
# Creating Generalized Additive Models of SR as a function of landscape-level 
# canopy cover and yard-level habitat features across seasons to determine the 
# best model and scale for predicting avian species richness.

# PACKAGES USED:
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(itsadug)
library(mgcv)
library(gratia)
library(patchwork)




# ============================================================================ # 
# 1. PREPARATION FOR GENERALIZED ADDITIVE MODELLING
# ============================================================================ # 

# --- 1.1 YARD & LANDSCAPE COMBINED DATA FRAME --- #

# Selected scale for landscape-level: 100 m

landscape_characteristics <- read_csv("2 - Cleaned/landscape_characteristics.csv")

# Creating big data frame with yard-level and landscape-level data
big_GAM_df <- landscape_characteristics %>%
  # select relevant variables
  select(Yard.Code, high_canopy_100m) %>%
  # convert columns to correct class
  mutate(Yard.Code = as.factor(Yard.Code),
         high_canopy_100m = as.numeric(high_canopy_100m)) %>%
  # scale all continuous variables between 2 and -2
  mutate_if(is.numeric, .funs = list(scale = ~as.numeric(scale(.x)))) %>%
  # rename column
  rename(high_canopy_100m_scale = scale) %>%
  # join to scaled yard characteristics df
  right_join(yard_GAM_df, by = c("Yard.Code" = "Yard.Code"))




# ============================================================================ # 
# 2. GENERALIZED ADDITIVE MODELLING
# ============================================================================ # 

# --- 2.1 QUESTION & HYPOTHESIS --- #

# 2.11 Research Question
# a. 

# 2.12 Hypothesis & Predictions
# B: 
#   a. Species richness in yards differs between migration and breeding
#         Because of presence of migratory birds in yards during migration
#   b. Different yard habitat features are important for species richness in 
#      different seasons
#         Because birds use yard habitat features differently between seasons



# --- 2.2 GAM PREPARATION --- #

# 2.21 Model Components
#   Response Variable: richness
#   Fixed Effects (Predictors): all yard habitat features (continuous), season (factor)
#   Interactions: yard habitat features by season
#   Random Effects: none 
#     Because yards don't need to be random effects, and year has only 2 levels)

# 2.22 Notes about model
#   Method = REML (Bayesian) (due to reasoning in Woods, 2011)
#   Factor-smooth interaction = "by" variable smooth
#     Because: entirely different smooth function for each level of 'season'
#   k = 5 
#     Because: k cannot be more than 21, so this is just a first guess





# --- 2.1 YARD HABITAT FEATURE GAM --- #

yard_global_GAM <- gam(richness ~ season + 
                         s(area_scale, by = season, k = 5) + 
                         s(tree_scale, by = season, k = 5) + 
                         s(shrub_scale, by = season, k = 5) + 
                         s(mean_dbh_scale, by = season, k = 5),
                       data = big_GAM_df, method = "REML")
summary(yard_global_GAM)




# --- 2.2 LANDSCAPE HABITAT FEATURE GAM --- #
# Selected scale for landscape-level: 100 m

landscape_GAM <- gam(richness ~
                       s(high_canopy_100m_scale, by = season, k = 5),
                     data = big_GAM_df, method = "REML")
summary(landscape_GAM)


# --- 2.3 COMBINED HABITAT FEATURE GAM --- #
combined_GAM <- gam(richness ~ season + 
                      s(area_scale, by = season, k = 5) + 
                      s(tree_scale, by = season, k = 5) + 
                      s(shrub_scale, by = season, k = 5) + 
                      s(mean_dbh_scale, by = season, k = 5) + 
                      s(high_canopy_100m_scale, by = season, k = 5),
                    data = big_GAM_df, method = "REML")
summary(combined_GAM)            


# --- 2.4 NULL GAM --- #
null_GAM <- gam(richness ~ 1, 
                data = big_GAM_df, method = "REML")


# ============================================================================ # 
# 3. AIC MODEL SELECTION
# ============================================================================ # \

AIC_big_GAM <- AIC(yard_global_GAM, 
                   landscape_GAM, 
                   combined_GAM, 
                   null_GAM)
AIC_big_GAM


