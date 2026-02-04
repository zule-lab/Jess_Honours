# 04-Feb-2026                          
# ============================================================================ # 
#                          EXPLORATORY DATA ANALYSIS
# ============================================================================ # 

# DESCRIPTION: 
# Exploration and visualization of distributions and trends of variables

# PACKAGES USED:
library(readr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyverse)


# ============================================================================ # 
# 1. DISTRIBUTION OF VARIABLES  
# ============================================================================ # 

# --- 1.1 SPECIES RICHNESS ACROSS YARDS --- #

yard_characteristics <- read_csv("4 - Outputs/yard_characteristics.csv")
SR_long <- read_csv("4 - Outputs/SR_long.csv")


# 1.11 All SR data
# boxplot of SR for each season
ggplot(data = SR_long) + 
  geom_boxplot(mapping = aes(x = dataset, y=richness, fill = dataset)) 

# histogram of SR for each season
ggplot(data = SR_long) + geom_histogram(mapping = aes(x=richness)) + 
  theme_bw() +
  facet_wrap(~dataset, nrow=2) + 
  labs(x = "Species richness", y = "Frequency")

# histogram for total SR
hist_total = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_total), bins = 20) + 
  theme_bw() +
  labs(title = "Total", x = "Richness", y = "Frequency")
hist_total


# 1.12 Migration
# 2024
hist_m24 = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_mig_2024), fill="lightgreen", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Migration 2024", x = "Species Richness", y = "Frequency")
hist_m24
       
# 2025
hist_m25 = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_mig_2025), fill="forestgreen", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Migration 2025", x = "Species Richness", y = "Frequency")
hist_m25

# Both
hist_m = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_mig), fill="darkgreen", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Migration", x = "Species Richness", y = "Frequency")
hist_m


# 1.13 Breeding
# 2024
hist_b24 = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_breed_2024), fill="lightyellow", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Breeding 2024", x = "Species Richness", y = "Frequency")
hist_b24

# 2025
hist_b25 = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_breed_2025), fill="yellow", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Breeding 2025", x = "Species Richness", y = "Frequency")
hist_b25

# Both
hist_b = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = SR_breed), fill="yellow3", color = "black", bins = 20) + 
  theme_bw() +
  labs(title = "Breeding", x = "Species Richness", y = "Frequency")
hist_b


# 1.14 Display all histograms on one panel
multi_hist_SR <- ((hist_m24|hist_m25|hist_m)/(hist_b24|hist_b25|hist_b)) + 
  plot_annotation(tag_levels = 'A')
multi_hist_SR



# --- 1.2 YARD HABITAT FEATURES --- #
# DEF: Make histograms for each yard habitat feature

# 1.21 Area
# Histograms
hist_area = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = area), fill="purple", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Area (ha)", y = "Frequency")

hist_back_area = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = back_area_ha), fill="purple4", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Backyard area (ha)", y = "Frequency")


# 1.22 Number of trees
hist_tree = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = tree), fill="darkgreen", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Number of trees", y = "Frequency")


# 1.23 Number of shrubs
hist_shrub = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = shrub), fill="lightgreen", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Number of shrubs", y = "Frequency")


# 1.24 Density
hist_density = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = density), fill="orange", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Density of vegetation (stems/ha", y = "Frequency")


# 1.25 Mean DBH
hist_DBH = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = mean_dbh), fill="tan4", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Mean DBH", y = "Frequency")


# 1.26 Number of fruiting plants
hist_fruit = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = n_fruiting_plants), fill="red", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Number of fruiting plants", y = "Frequency")


# 1.27 Number of native plants
hist_native = ggplot(data = yard_characteristics) + 
  geom_histogram(mapping = aes(x = n_native_plants), fill="blue4", color = "black", bins = 20) + 
  theme_bw() +
  labs(x = "Number of native plants", y = "Frequency")


# 1.28 Display all histograms together
multi_hist_yard_characteristics <- 
  ((hist_area|hist_tree|hist_density|hist_fruit)/(hist_back_area|hist_shrub|hist_DBH|hist_native)) + 
  plot_annotation(tag_levels = 'A')
multi_hist_yard_characteristics




# --- 1.3 LANDSCAPE CHARACTERISTICS --- #

# INSERT



# ============================================================================ # 
# 2. RELATIONSHIPS TO SPECIES RICHNESS
# ============================================================================ # 

# --- 2.1 SR ~ YARD HABITAT FEATURES --- #
# DEF: Scatter plots of yard habitat features with species richness for each season

# 2.11 Make long data frame for each variable with each season's SR
yard_characteristics_long <- yard_characteristics %>%
  select(-c(back_perimeter_m,diagonal_1_m,diagonal_2_m,short_radius_m,
            long_radius_m,lat,long,utm_zone,utm_easting,utm_northing)) %>%
  pivot_longer(
    cols = starts_with("SR_"), # select columns starting with SR_
    names_to = "season", # name of new column to store old column names
    values_to = "richness" # name of new column to store values
  )

# Export yard_characteristics_long data frame
write.csv(yard_characteristics_long, file="yard_characteristics_long.csv", row.names=FALSE)
  # Moved to "4 - Outputs" directory


# 2.12 SR ~ Area
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = area, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Area", y = "Species richness")

# backyard area
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = back_area_ha, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Backyard area", y = "Species richness")


# 2.13 SR ~ Number of trees
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = tree, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Number of trees", y = "Species richness")


# 2.14 SR ~ Number of shrubs
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = shrub, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Number of shrubs", y = "Species richness")


# 2.15 SR ~ Density of vegetation
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = density, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Density of vegetation (stem/ha)", y = "Species richness")


# 2.16 SR ~ Mean DBH
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = mean_dbh, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Mean DBH", y = "Species richness")


# 2.17 SR ~ Number of fruiting plants
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = n_fruiting_plants, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Number of fruiting plants", y = "Species richness")
# Note: no plant fruited in spring, so this would only really be important in breeding


# 2.18 SR ~ Number of native plants
ggplot(data = yard_characteristics_long) + 
  geom_point(mapping = aes(x = n_native_plants, y = richness)) + 
  theme_bw() +
  facet_wrap(~season, nrow=2) + 
  labs(x = "Number of native plants", y = "Species richness")




# --- 2.2. SR ~ LANDSCAPE HABITAT FEATURES --- #
# Linear ggplot relationships



# --- 2.3. SR ~ LANDSCAPE HABITAT FEATURES --- #
# Linear ggplot relationships




