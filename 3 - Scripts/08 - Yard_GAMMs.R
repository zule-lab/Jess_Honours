# 06-Feb-2026                          
# ============================================================================ # 
#                                 YARD ~ SR GAMMS
# ============================================================================ # 

# DESCRIPTION: 
# Creating Generalized Additive Mixed Models for yard-level habitat features and
# species richness across seasons.

# PACKAGES USED:
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(itsadug)
library(mgcv)


# ============================================================================ # 
# 1. PREPARATION FOR MODELLING
# ============================================================================ # 

# --- 1.1 RESEARCH QUESTION & PREDICTIONS --- #

# 1.11 Question: 
# - What is the effect of various yard habitat features on avian species richness 
#         AND 
# - does this effect vary during migration versus breeding?

# 1.12 Predictions:
# - non-linear relationships between habitat features and SR
# - effect of season on habitat features


# --- 1.2 DATA FRAME PREPARATION --- #

# 1.21 Variables of GAMM data frame and their class
#   Yard.Code: factor (22 yards)
#   year: factor (2024, 2025)
#   season: factor (migration, breeding)
#   richness: integer
#   area: numeric
#   tree: numeric
#   shrub: numeric
#   mean_dbh: numeric
#   n_fruiting: numeric
#   n_native: numeric

# 1.22 Long data frame from yard_characteristics_long.csv (splitting year and season)

yard_characteristics_long <- read_csv("2 - Cleaned/yard_characteristics_long.csv")

yard_GAMM_df <- yard_characteristics_long %>%
  
  # remove correlated variables (density, back_area_ha)
  select(-c(back_area_ha, density)) %>%
  
  # remove values of "SR_total", "SR_mig", "SR_breed"
  filter(season %in% c("SR_mig_2024", "SR_mig_2025", 
                       "SR_breed_2024", "SR_breed_2025")) %>%
  
  # split "season" column into 2 columns: season and year
  mutate(year = str_extract(season, "\\d{4}"),
         season = case_when(
           str_detect(season,"mig") ~ "migration",
           str_detect(season, "breed") ~ "breeding"),
         # convert columns to correct class
         season = as.factor(season),
         year = as.factor(year),
         Yard.Code = as.factor(Yard.Code), 
         richness = as.integer(richness)
  )

# Export yard_GAMM_df data frame
write.csv(yard_GAMM_df, file="2 - Cleaned/yard_GAMM_df.csv", row.names=FALSE)



# ============================================================================ # 
# 2. GENERALIZED ADDITIVE MIXED EFFECT MODELLING
# ============================================================================ # 

# --- 2.1 GAMM PREPARATION --- #

# 2.11 Components of model
# response variable: richness in yard i, season j, year k
# fixed effects (predictors): yard habitat features (continuous), season (factor)
# interactions: every yard habitat features should interact with season
# random effects: Yard.Code, year

# 2.12 Notes about model
# method = REML (Bayesian) (due to reasoning in Woods, 2011)
# factor-smooth interaction = "by" variable smooth
#   BC: entirely different smooth function for each level of 'season'
# k = 5 
#   BC: k cannot be more than 21, so this is just a first guess
# random effects: intercepts
#   BUT: should they be smooths? I don't know


#note: dropped fruiting plants due to low number of variables 
#   note: maybe i need to change k for each predictor?

# --- 2.2 GAMM MODEL --- #
yard_GAMM_model <- gam(richness ~ 
                          season + # Fixed effects
                          s(area, by = season, k = 5) + 
                          s(tree, by = season, k = 5) + 
                          s(shrub, by = season, k = 5) + 
                          s(mean_dbh, by = season, k = 5) +
                          s(n_native_plants, by = season, k = 5) + 
                          
                          # Random effects (intercepts)
                          s(year, bs = "re") + 
                          s(Yard.Code, bs = "re"),
                          
                        data = yard_GAMM_df, method = "REML")

summary(yard_GAMM_model)$s.table
plot(yard_GAMM_model)

# Conclusions/thoughts from this first model:
#   - shrubs (linear?) & mean_dbh (non-linear) are significant in migration
#   - native plants seem to be important (threshold) in both seasons
#   - some stuff seems linear...?





# MODEL VALIDATION .... for later :)

