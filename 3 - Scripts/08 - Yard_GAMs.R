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
# 1. PREPARATION FOR GENERALIZED ADDITIVE MODELLING
# ============================================================================ # 

# --- 1.1 DATA FRAME PREPARATION --- #

# 1.11 Variables of GAMM data frame and their class
#   Yard.Code: factor (22 yards)
#   year: factor (2024, 2025)
#   season: factor (migration, breeding)
#   richness: integer
#   area: numeric
#   tree: numeric
#   shrub: numeric
#   mean_dbh: numeric
#   n_fruiting: numeric
#   proportion_native: numeric

# 1.12 Long data frame from yard_characteristics_long.csv (splitting year and season)

yard_characteristics_long <- read_csv("2 - Cleaned/yard_characteristics_long.csv")

yard_GAMM_df <- yard_characteristics_long %>%
  # remove correlated variables (density)
  select(-c(density)) %>%
  
  # remove values of "SR_total", "SR_mig", "SR_breed"
  filter(season %in% c("SR_mig_2024", "SR_mig_2025", 
                       "SR_breed_2024", "SR_breed_2025")) %>%
  
  # split "season" column into 2 columns: season and year
  mutate(year = str_extract(season, "\\d{4}"),
         season = case_when(str_detect(season,"mig") ~ "migration",
                            str_detect(season, "breed") ~ "breeding"),
         season = as.factor(season)) %>%
  
  mutate(year = as.factor(year),  # convert columns to correct class
         Yard.Code = as.factor(Yard.Code), 
         richness = as.integer(richness),
         area = as.numeric(area),
         tree = as.numeric(tree),
         shrub = as.numeric(shrub),
         mean_dbh = as.numeric(mean_dbh),
         proportion_fruit = as.numeric(proportion_fruit),
         proportion_native = as.numeric(proportion_native)) %>%
  
  # scale all continuous variables between 2 and -2
  mutate_if(is.numeric, .funs = list(mut = ~as.numeric(scale(.x))))

# Export yard_GAMM_df data frame
write.csv(yard_GAMM_df, file="2 - Cleaned/yard_GAMM_df.csv", row.names=FALSE)



# ============================================================================ # 
# 2. GLOBAL GAM
# ============================================================================ # 

# --- 2.1 GAMM PREPARATION --- #

# 2.11 Components of model
# response variable: species richness
# fixed effects (predictors): yard habitat features (continuous), season (factor)
# interactions: yard habitat features by season
# random effects: none (because yards don't need to be random effects, and year
# has only 2 levels)

# 2.12 Notes about model
# method = REML (Bayesian) (due to reasoning in Woods, 2011)
# factor-smooth interaction = "by" variable smooth
#   Because: entirely different smooth function for each level of 'season'
# k = 5 
#   Because: k cannot be more than 21, so this is just a first guess


#note: dropped fruiting plants due to low number of variables 


# 2.13 GENERAL RESEARCH QUESTION & PREDICTIONS

# Question: 
# - What is the effect of various yard habitat features on avian species richness 
#         AND 
# - does this effect vary during migration versus breeding?

# Predictions:
# - non-linear relationships between habitat features and SR
# - effect of season on habitat features



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

summary(yard_GAMM_model)

plot(yard_GAMM_model)

concurvity(yard_GAMM_model, full = TRUE)


yard_GAMM_test <- gam(richness ~ 
                         s(area, k = 5) + 
                         s(tree, k = 5) + 
                         s(shrub, k = 5) + 
                         s(mean_dbh, k = 5) +
                         s(n_native_plants, k = 5),
                       
                       data = yard_GAMM_df, method = "REML")


concurvity(yard_GAMM_test)
# concurvity with mean DBH


# Conclusions/thoughts from this first model:
#   - shrubs (linear?) & mean_dbh (non-linear) are significant in migration
#   - native plants seem to be important (threshold) in both seasons
#   - some stuff seems linear...? why.


# MODEL VALIDATION .... for later :')



# ============================================================================ # 
# 3. SEASON + AREA GAM
# ============================================================================ # 





# ============================================================================ # 
# 4. SEASON ONLY GAM
# ============================================================================ # 





# ============================================================================ # 
# 5. NULL GAM
# ============================================================================ # 










