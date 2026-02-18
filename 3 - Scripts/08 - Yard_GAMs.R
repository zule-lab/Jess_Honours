# 11-Feb-2026                          
# ============================================================================ # 
#                            SR ~ YARD FEATURES GAMS
# ============================================================================ # 

# DESCRIPTION: 
# Creating Generalized Additive Models for yard-level habitat features and
# species richness across seasons.

# PACKAGES USED:
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(itsadug)
library(mgcv)
library(gratia)



# ============================================================================ # 
# 1. PREPARATION FOR GENERALIZED ADDITIVE MODELLING
# ============================================================================ # 

# --- 1.1 DATA FRAME PREPARATION --- #

# 1.11 All possible variables for GAM data frame and their desired class
#   Yard.Code: factor (22 yards)
#   year: factor (2024, 2025)
#   season: factor (migration, breeding)
#   richness: integer
#   area: numeric
#   tree: numeric
#   shrub: numeric
#   mean_dbh: numeric
#   proportion_fruit: numeric
#   proportion_native: numeric
#### NOTE: each numeric variable has a scaled version ("variable_scale")

# 1.12 Prepare long data frame from yard_characteristics_long.csv
yard_characteristics_long <- read_csv("2 - Cleaned/yard_characteristics_long.csv")

# Create data frame
yard_GAM_df <- yard_characteristics_long %>%
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
  # convert columns to correct class
  mutate(year = as.factor(year),
         Yard.Code = as.factor(Yard.Code), 
         richness = as.integer(richness),
         area = as.numeric(area),
         tree = as.numeric(tree),
         shrub = as.numeric(shrub),
         mean_dbh = as.numeric(mean_dbh),
         proportion_fruit = as.numeric(proportion_fruit),
         proportion_native = as.numeric(proportion_native)) %>%
  # scale all continuous variables between 2 and -2
  mutate_if(is.numeric, .funs = list(scale = ~as.numeric(scale(.x))))

# Export yard_GAMM_df data frame
write.csv(yard_GAM_df, file="2 - Cleaned/yard_GAM_df.csv", row.names=FALSE)






# ============================================================================ # 
# 2. GLOBAL GAM
# ============================================================================ # 

# --- 2.1 QUESTION & HYPOTHESIS --- #

# 2.11 Research Question
# a. How does species richness in yards differ by season, AND 
# b. how do yard-level habitat features affect species richness differently by season?

# 2.12 Hypothesis & Predictions
# Birds use yards (and green spaces) differently during migration than during 
# the breeding season, thus: 
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



# --- 2.3 GLOBAL GAM --- #

# 2.31 Variables of GAM data frame and their class
#   season: factor (migration, breeding)
#   richness: integer
#   area_scale: numeric
#   tree_scale: numeric
#   shrub_scale: numeric
#   mean_dbh_scale: numeric
#   proportion_fruit_scale: numeric
#   proportion_native_scale: numeric

# 2.32 Global GAM
yard_global_GAM <- gam(richness ~ season + 
                         s(area_scale, by = season, k = 5) + 
                         s(tree_scale, by = season, k = 5) + 
                         s(shrub_scale, by = season, k = 5) + 
                         s(mean_dbh_scale, by = season, k = 5),
                         #s(proportion_fruit_scale, by = season, k = 5) + 
                         #s(proportion_native_scale, by = season, k = 5), 
                       data = yard_GAM_df, method = "REML")
    # note: after standardizing fruiting plants for proportion, the model could 
    #       run with fruiting plants included

summary(yard_global_GAM)

# Visualize
par(mfrow = c(1,1))
vis.gam(yard_global_GAM, view = c("shrub_scale","season"), theta = 1, n.grid = 50, lwd = 0.4)
vis.gam(yard_global_GAM, view = c("mean_dbh_scale","season"), theta = 60, n.grid = 50, lwd = 0.4)
par(mfrow = c(2,6))
plot(yard_global_GAM)

draw(yard_global_GAM)

# --- 2.4 MODEL CHECKING --- #

# 2.41 Concurvity testing
concurvity(yard_global_GAM) # problems: all variables ~0.9 :(

# 2.42 Gam.check
par(mfrow = c(2,2))
gam.check(yard_global_GAM) # problem: heteroscedasticity, I fear

# 2.43 Gratia model check
par(mfrow = c(2,2))
appraise(yard_global_GAM) 




#### Stuff with Barbara - concurvity
yard_GAMM_test <- gam(richness ~ 
                         s(area, k = 5) + 
                         s(tree, k = 5) + 
                         s(shrub, k = 5) + 
                         s(mean_dbh, k = 5) +
                         s(n_native_plants, k = 5),
                       data = yard_GAM_df, method = "REML")
concurvity(yard_GAMM_test)
# note potential concurvity with mean DBH - be careful!






# ============================================================================ # 
# 3. SEASON + AREA GAM
# ============================================================================ # 

# --- 3.1 QUESTION & HYPOTHESIS --- #

# 3.11 Research Question
# a. How does species richness in yards differ by season, AND 
# b. how does area affect species richness differently by season?

# 3.12 Hypothesis & Predictions
# Birds use yards (and green spaces) differently during migration than during 
# the breeding season, thus: 
# Bird species richness is largely driven by season and area where:
#   a. Species richness in yards differs between migration and breeding
#         Because of presence of migratory birds in yards during migration
#   b. Area is differently important for species richness across seasons
#         Theory of species-area relationships
#         ***NOTE: I actually don't know why area should interact with season



# --- 3.2 GAM PREPARATION --- #

# 3.21 Model Components
#   Response Variable: richness
#   Fixed Effects (Predictors): area (numeric), season (factor)
#   Interactions: area by season
#   Random Effects: none

# 3.22 Notes about model
#   Method = REML (Bayesian) (due to reasoning in Woods, 2011)
#   Factor-smooth interaction = "by" variable smooth
#     Because: entirely different smooth function for each level of 'season'
#   k = 5 
#     Because: k cannot be more than 21, so this is just a first guess



# --- 3.3 SEASON-AREA GAM --- #

# 3.31 Variables of GAM data frame and their class
#   season: factor (migration, breeding)
#   richness: integer
#   area_scale: numeric

# 3.32 Season-Area GAM
yard_season_area_GAM <- gam(richness ~ season + 
                              s(area, by = season, k = 5),
                            data = yard_GAM_df, method = "REML")

summary(yard_season_area_GAM)

par(mfrow = c(1,2))
plot(yard_season_area_GAM) 
draw(yard_season_area_GAM)


# --- 3.4 MODEL CHECKING --- #

# 3.41 Concurvity testing
concurvity(yard_season_area_GAM) # no problems!

# 3.42 Gam.check
par(mfrow = c(2,2))
gam.check(yard_season_area_GAM) # problem: low p-values







# ============================================================================ # 
# 4. SEASON ONLY GAM
# ============================================================================ # 

# --- 4.1 QUESTION & HYPOTHESIS --- #

# 4.11 Research Question
# How does species richness in yards differ by season?

# 4.12 Hypothesis & Predictions
# More species of birds pass through cities during migration than breeding season,
# thus:
# Bird species richness is driven by season
#   Prediction: Species richness in yards differs between migration and breeding
#         Because of presence of migratory birds in yards during migration


# --- 4.2 GAM PREPARATION --- #

# 4.21 Model Components
#   Response Variable: richness
#   Fixed Effects (Predictors): season (factor)
#   Interactions: none
#   Random Effects: none

# 4.22 Notes about model
#   Method = REML (Bayesian) (due to reasoning in Woods, 2011)
#   k = 5 
#     Because: k cannot be more than 21, so this is just a first guess



# --- 4.3 SEASON GAM --- #

# 4.31 Variables of GAM data frame and their class
#   season: factor (migration, breeding)
#   richness: integer

# 4.32 Season GAM
yard_season_GAM <- gam(richness ~ season,
                            data = yard_GAM_df, method = "REML")

summary(yard_season_GAM)
# not possible to plot ("apparently")



# --- 4.4 MODEL CHECKING --- #

# 4.41 Concurvity testing
concurvity(yard_season_GAM)
# "nothing to do for this model" according to R

# 4.42 Gam.check
par(mfrow = c(2,2))
gam.check(yard_season_GAM)






# ============================================================================ # 
# 5. NULL GAM
# ============================================================================ # 

# --- 5.1 QUESTION & HYPOTHESIS --- #

# 5.11 Research Question
# Null

# 5.12 Hypothesis & Predictions
# Bird species richness in yards is driven by random chance
#   Prediction: Species richness in yards is the same for migration and breeding


# --- 5.2 GAM PREPARATION --- #

# 5.21 Model Components
#   Response Variable: richness
#   Fixed Effects (Predictors): 1
#   Interactions: none
#   Random Effects: none

# 5.22 Notes about model
#   Method = REML (Bayesian) (due to reasoning in Woods, 2011)
#   k = 5 
#     Because: k cannot be more than 21, so this is just a first guess



# --- 5.3 NULL GAM --- #

# 5.31 Variables of GAM data frame and their class
#   season: factor (migration, breeding)
#   richness: integer

# 5.32 Null GAM
yard_null_GAM <- gam(richness ~ 1,
                       data = yard_GAM_df, method = "REML")

summary(yard_null_GAM)
plot(yard_null_GAM) # not possible to plot ("apparently")



# --- 5.4 MODEL CHECKING --- #

# 5.41 Concurvity testing
concurvity(yard_null_GAM)
# "nothing to do for this model" according to R

# 5.42 Gam.check
par(mfrow = c(2,2))
gam.check(yard_null_GAM)







# ============================================================================ # 
# 6. AIC MODEL SELECTION
# ============================================================================ # 

# --- 6.1 GAM SELECTION --- #

# Model selection according to AIC
AIC(yard_global_GAM, yard_season_area_GAM, yard_season_GAM, yard_null_GAM)
# result: yard_global_GAM has the lowest AIC value (hell yeah - i think)



