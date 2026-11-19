# 18-Nov-2026                          
# ============================================================================ # 
#                              OUTLIER REMOVAL EXPERIMENTATION
# ============================================================================ # 

# DESCRIPTION: 
# Removing outlier DBH value and seeing how the relationship changes in GAMs

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
# 1. REMOVING OUTLIER VARIABLE FROM DATA
# ============================================================================ # 

# --- 1.1 REMOVE OUTLIER FROM YARD.CHARACTERISTICS --- #

# Load data


