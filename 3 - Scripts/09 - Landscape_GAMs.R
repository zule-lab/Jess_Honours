# 18-Mar-2026                          
# ============================================================================ # 
#                              SCALE OF EFFECT ANALYSIS
# ============================================================================ # 

# DESCRIPTION: 
# Creating Generalized Additive Models of SR as a function of landscape-level 
# canopy cover to determine scale effect.

# I want to test if scale of effect is different between seasons, and if scale 
# of effect is more or less important than local yard features for SR

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
library(tidyr)


# ============================================================================ # 
# 1. PREPARATION FOR GENERALIZED ADDITIVE MODELLING
# ============================================================================ # 

# --- 1.1 LANDSCAPE CHARACTERISTICS DATA FRAME PREPARATION --- #

landscape_characteristics_split <- read_csv("2 - Cleaned/landscape_characteristics_split.csv")

# 1.11 Create data frame for GAM model
canopy_GAM_df <- landscape_characteristics_split %>%
  # convert columns to correct class
  mutate(Yard.Code = as.factor(Yard.Code), 
         season = as.factor(season),
         richness = as.integer(richness),
         low_canopy_25m = as.numeric(low_canopy_25m),
         low_canopy_50m = as.numeric(low_canopy_50m),
         low_canopy_100m = as.numeric(low_canopy_100m),
         low_canopy_200m = as.numeric(low_canopy_200m),
         low_canopy_400m = as.numeric(low_canopy_400m),
         high_canopy_25m = as.numeric(high_canopy_25m),
         high_canopy_50m = as.numeric(high_canopy_50m),
         high_canopy_100m = as.numeric(high_canopy_100m),
         high_canopy_200m = as.numeric(high_canopy_200m),
         high_canopy_400m = as.numeric(high_canopy_400m))
  # scale all continuous variables between 2 and -2
  # mutate_if(is.numeric, .funs = list(scale = ~as.numeric(scale(.x))))


write.csv(canopy_GAM_df, file="2 - Cleaned/canopy_GAM_df.csv", row.names=FALSE)

# 1.12 Create data frames for each season (migration, breeding)
canopy_migration_GAM_df <- canopy_GAM_df %>%
  filter(season %in% c("migration")) # select migration data

canopy_breeding_GAM_df <- canopy_GAM_df %>%
  filter(season %in% c("breeding")) # select breeding data



# ============================================================================ # 
# 2. MIGRATION GAMS AT EACH SCALE
# ============================================================================ # 

# --- 2.1 QUESTION & HYPOTHESIS --- #

# 2.11 Research Question
# a. What is the scale of effect for bird species richness during migration?
# b. Is scale of effect different in migration than breeding?
  #  (not answered in this model only - needs the next one too)

# 2.12 Hypothesis & Predictions
# Birds use yards (and green spaces) differently during migration than during 
# the breeding season, thus: 
#   a. Scale of effect will be small during migration
#         Because of previous studies in lab and need for foraging (?)
#   b. Scale of effect will be different during migration than breeding
#         Because birds have different needs across seasons



# --- 2.2 GAM PREPARATION --- #

# 2.21 Model Components
#   Response Variable: richness
#   Fixed Effects (Predictors): low canopy and high canopy
#   Interactions: none
#   Random Effects: none 

# I will create a GAM at each spatial scale: 25 m, 50 m, 100 m, 200 m, 400 m

# Variables of GAM data frame and their class
#   season: factor (migration, breeding)
#   high_canopy_scaleX: integer
#   low_canopy_scaleX: numeric


# 2.22 Notes about model
#   Method = REML (Bayesian) (due to reasoning in Woods, 2011)
#   Factor-smooth interaction = "by" variable smooth
#     Because: entirely different smooth function for each level of 'season'
#   k = 5 
#     Because: k cannot be more than 21, so this is just a first guess



# --- 2.3 MIGRATION GAM: 25 m SCALE --- #
# All veg layers
mig_25m_veg_GAM <- gam(richness ~
                         s(low_canopy_25m, k = 5) + 
                         s(high_canopy_25m, k = 5),
                       data = canopy_migration_GAM_df, method = "REML")

summary(mig_25m_veg_GAM)
concurvity(mig_25m_veg_GAM)

# Just canopy layer
mig_25m_canopy_GAM <- gam(richness ~ 
                            s(high_canopy_25m, k = 5),
                          data = canopy_migration_GAM_df, method = "REML")

summary(mig_25m_canopy_GAM)
concurvity(mig_25m_canopy_GAM)



# --- 2.4 MIGRATION GAM: 50 m SCALE --- #
# All veg
mig_50m_veg_GAM <- gam(richness ~
                            s(low_canopy_50m, k = 5) + 
                            s(high_canopy_50m, k = 5),
                          data = canopy_migration_GAM_df, method = "REML")

summary(mig_50m_veg_GAM)
concurvity(mig_50m_veg_GAM) #highly concurve

# Just canopy 
mig_50m_canopy_GAM <- gam(richness ~
                            s(high_canopy_50m, k = 5),
                          data = canopy_migration_GAM_df, method = "REML")

summary(mig_50m_canopy_GAM)
concurvity(mig_50m_canopy_GAM)



# --- 2.5 MIGRATION GAM: 100 m SCALE --- #
# All veg
mig_100m_veg_GAM <- gam(richness ~
                            s(low_canopy_100m, k = 5) + 
                            s(high_canopy_100m, k = 5),
                          data = canopy_migration_GAM_df, method = "REML")

summary(mig_100m_veg_GAM)
concurvity(mig_100m_veg_GAM)

# Just canopy
mig_100m_canopy_GAM <- gam(richness ~
                             s(high_canopy_100m, k = 5),
                           data = canopy_migration_GAM_df, method = "REML")

summary(mig_100m_canopy_GAM)
concurvity(mig_100m_canopy_GAM)



# --- 2.6 MIGRATION GAM: 200 m SCALE --- #
# All veg
mig_200m_veg_GAM <- gam(richness ~
                             s(low_canopy_200m, k = 5) + 
                             s(high_canopy_200m, k = 5),
                           data = canopy_migration_GAM_df, method = "REML")

summary(mig_200m_veg_GAM)
concurvity(mig_200m_veg_GAM) #highly concurve

# Just canopy
mig_200m_canopy_GAM <- gam(richness ~
                             s(high_canopy_200m, k = 5),
                           data = canopy_migration_GAM_df, method = "REML")

summary(mig_200m_canopy_GAM)
concurvity(mig_200m_canopy_GAM) 


# --- 2.7 MIGRATION GAM: 400 m SCALE --- #
# All veg
mig_400m_veg_GAM <- gam(richness ~
                             s(low_canopy_400m, k = 5) + 
                             s(high_canopy_400m, k = 5),
                           data = canopy_migration_GAM_df, method = "REML")

summary(mig_400m_veg_GAM)
concurvity(mig_400m_veg_GAM) # highly concurve

# Just canopy
mig_400m_canopy_GAM <- gam(richness ~
                             s(high_canopy_400m, k = 5),
                           data = canopy_migration_GAM_df, method = "REML")

summary(mig_400m_canopy_GAM)
concurvity(mig_400m_canopy_GAM)




# --- 2.8 AIC MODEL SELECTION --- #
AIC_mig_veg_GAM <- AIC(mig_25m_veg_GAM, 
                       mig_50m_veg_GAM, 
                       mig_100m_veg_GAM, 
                       mig_200m_veg_GAM, 
                       mig_400m_veg_GAM)
AIC_mig_veg_GAM

AIC_mig_canopy_GAM <- AIC(mig_25m_canopy_GAM, 
                          mig_50m_canopy_GAM, 
                          mig_100m_canopy_GAM, 
                          mig_200m_canopy_GAM, 
                          mig_400m_canopy_GAM)
AIC_mig_canopy_GAM






# ============================================================================ # 
# 3. BREEDING GAMS AT EACH SCALE
# ============================================================================ # 
# --- 3.3 BREEDING GAM: 25 m SCALE --- #
# All veg
bre_25m_veg_GAM <- gam(richness ~
                            s(low_canopy_25m, k = 5) + 
                            s(high_canopy_25m, k = 5),
                          data = canopy_breeding_GAM_df, method = "REML")

summary(bre_25m_veg_GAM)
concurvity(bre_25m_veg_GAM)

# Just high canopy
bre_25m_canopy_GAM <- gam(richness ~
                            s(high_canopy_25m, k = 5),
                          data = canopy_breeding_GAM_df, method = "REML")

summary(bre_25m_canopy_GAM)
concurvity(bre_25m_canopy_GAM)


# --- 3.4 BREEDING GAM: 50 m SCALE --- #
# All veg
bre_50m_veg_GAM <- gam(richness ~
                            s(low_canopy_50m, k = 5) + 
                            s(high_canopy_50m, k = 5),
                          data = canopy_breeding_GAM_df, method = "REML")

summary(bre_50m_veg_GAM)
concurvity(mig_50m_veg_GAM) #highly concurve

# Just high canopy
bre_50m_canopy_GAM <- gam(richness ~
                            s(high_canopy_50m, k = 5),
                          data = canopy_breeding_GAM_df, method = "REML")

summary(bre_50m_canopy_GAM)
concurvity(mig_50m_canopy_GAM) 



# --- 3.5 BREEDING GAM: 100 m SCALE --- #
# All veg
bre_100m_veg_GAM <- gam(richness ~
                             s(low_canopy_100m, k = 5) + 
                             s(high_canopy_100m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_100m_veg_GAM)
concurvity(bre_100m_veg_GAM) # highly concurve

# Just high canopy
bre_100m_canopy_GAM <- gam(richness ~
                             s(high_canopy_100m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_100m_canopy_GAM)
concurvity(bre_100m_canopy_GAM)




# --- 3.6 BREEDING GAM: 200 m SCALE --- #
# All veg
bre_200m_veg_GAM <- gam(richness ~
                             s(low_canopy_200m, k = 5) + 
                             s(high_canopy_200m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_200m_veg_GAM)
concurvity(bre_200m_veg_GAM) #highly concurve

# Just high canopy
bre_200m_canopy_GAM <- gam(richness ~
                             s(high_canopy_200m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_200m_canopy_GAM)
concurvity(bre_200m_canopy_GAM)


# --- 3.7 BREEDING GAM: 400 m SCALE --- #
# All veg
bre_400m_veg_GAM <- gam(richness ~
                             s(low_canopy_400m, k = 5) + 
                             s(high_canopy_400m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_400m_veg_GAM)
concurvity(bre_400m_veg_GAM) # highly concurve

# Just high canopy
bre_400m_canopy_GAM <- gam(richness ~
                             s(high_canopy_400m, k = 5),
                           data = canopy_breeding_GAM_df, method = "REML")

summary(bre_400m_canopy_GAM)
concurvity(bre_400m_canopy_GAM) # highly concurve



# --- 3.8 AIC MODEL SELECTION --- #

AIC_bre_veg_GAM <- AIC(bre_25m_veg_GAM, 
                       bre_50m_veg_GAM, 
                       bre_100m_veg_GAM, 
                       bre_200m_veg_GAM, 
                       bre_400m_veg_GAM)
AIC_bre_veg_GAM

AIC_bre_canopy_GAM <- AIC(bre_25m_canopy_GAM, 
                          bre_50m_canopy_GAM,
                          bre_100m_canopy_GAM, 
                          bre_200m_canopy_GAM, 
                          bre_400m_canopy_GAM)
AIC_bre_canopy_GAM


# Conclusions: no true scale of effect in breeding or migration, and results are
# slightly contrary to our hypotheses


# ============================================================================ # 
# 4. PLOT OF AIC VALUES ACROSS SCALES (FIGURE 3)
# ============================================================================ # 

# --- 4.1 BUILD LONG ∆AIC DATA FRAME --- #

# Define spatial scales and seasons (repeated due to long data frame that we're buiding)
scale <- c(25, 50, 100, 200, 400, 25, 50, 100, 200, 400)
scale <- as.factor(scale)
season <- c("migration","migration","migration","migration","migration", 
            "breeding","breeding","breeding","breeding","breeding")

# Calculate ∆AIC for migration season
AIC_mig <- AIC_mig_canopy_GAM$AIC
deltaAIC_mig <- AIC_mig_canopy_GAM$AIC - 140.0439
deltaAIC_mig <- c(1.297754,1.349002,1.678672,0.9670479,0)

# Calculate ∆AIC for breeding season
AIC_bre <- AIC_bre_canopy_GAM$AIC
deltaAIC_bre <- AIC_bre_canopy_GAM$AIC - 96.50497
deltaAIC_bre <- c(0,1.939411,2.073233,1.906869,1.843430)

# Stack ∆AIC values
AIC <- c(AIC_mig, AIC_bre)
deltaAIC <- c(deltaAIC_mig, deltaAIC_bre)

# Bind all data together to build long data frame
deltaAIC_long <- data.frame(season, scale, AIC, deltaAIC)
deltaAIC_long_mig <- deltaAIC_long[deltaAIC_long$season == "migration",]
deltaAIC_long_bre <- deltaAIC_long[deltaAIC_long$season == "breeding",]


# --- 4.2 PLOT AIC VALUES  --- #
season_cols <- c(
  migration = "#62B751",
  breeding = "#FF4A79")


# Plotting ∆AIC values across scales
ggplot(deltaAIC_long, aes(x = scale, y = deltaAIC, group = season, color = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_color_manual(values = c(
    "breeding" = "#FF4A79",
    "migration" = "#62B751")) +
  labs(
    x = "Spatial scale (m)",
    y = expression(Delta*AIC[c]),
    color = "Season"
  ) +
  theme_bw()


# Plotting AIC values across scales
ggplot(deltaAIC_long, aes(x = scale, y = AIC, group = season, color = season)) +
  
  # shaded intervals above and below 2
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = 96.50497, ymax = 98.50497,
           alpha = 0.2, fill = "#FF4A79") +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = 140.0439, ymax = 142.0439,
           alpha = 0.2, fill = "#62B751") +
  
  # lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # horizontal reference lines
  scale_color_manual(values = c(
    "breeding" = "#FF4A79",
    "migration" = "#62B751")) +
  #geom_hline(yintercept = 96.50497, linetype = "dashed", color = "#FF4A79") +
  #geom_hline(yintercept = 140.0439, linetype = "dashed", color = "#62B751") +
  
  labs(
    x = "Spatial scale (m)",
    y = expression(AIC[c]),
    color = "Season"
  ) +
  theme_bw()



# Plotting AIC values across scales in panels
ggplot(deltaAIC_long, aes(x = scale, y = AIC, group = season, color = season)) +
  
  # season-specific shaded bands
  geom_rect(
    data = data.frame(
      season = c("breeding", "migration"),
      xmin = -Inf, xmax = Inf,
      ymin = c(96.50497, 140.0439),
      ymax = c(96.50497, 140.0439) + 2
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = season),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  
  # lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # horizontal reference lines
  #geom_hline(yintercept = 96.50497, linetype = "dashed", color = "#FF4A79") +
  #geom_hline(yintercept = 140.0439, linetype = "dashed", color = "#62B751") +
  
  # colours
  scale_color_manual(values = c(
    "breeding" = "#FF4A79",
    "migration" = "#62B751"
  )) +
  scale_fill_manual(values = c(
    "breeding" = "#FF4A79",
    "migration" = "#62B751"
  )) +
  
  # split into panels
  facet_wrap(~ season) +
  
  labs(
    x = "Spatial scale (m)",
    y = expression(AIC[c]),
    color = "Season",
    fill = "Season"
  ) +
  theme_bw()





