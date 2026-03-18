# 03-Dec-2025
# ============================================================================ # 
#                            CLEANING THE DATASETS
# ============================================================================ # 

# DESCRIPTION: 
# Splitting of cleaned observational data (essentially my raw data)
# into functional data frames in the context of my project (i.e., splitting them
# by season and year).

# PACKAGES USED:
library(readr)


# ============================================================================ # 
# 1. EXTRACTING YARDS FROM LARGE DATASET
# ============================================================================ # 
# DEF: Importing data and extracting only observations from yards, not from streets.

# --- 1.1 YARD DATA 2024 --- #
# Import
data_2024 <- read_csv("1 - Input/ndg_cleaneddata_2024.csv")
# Extract yards
yard_data_2024 <- data_2024[data_2024$Landtype=="yard", ]

# --- 1.2 YARD DATA 2025 --- #
# Import
data_2025 <- read_csv("1 - Input/ndg_cleaneddata_2025.csv")
# Extract yards
yard_data_2025 <- data_2025[data_2025$Landtype=="yard",]

# --- 1.3 NEW DATA FRAME --- #
# Export a new data frame with just yard data from all years
all_yard_data <- rbind(yard_data_2024, yard_data_2025)
write.csv(all_yard_data, file="2 - Cleaned/cleaned_yard_data.csv", row.names=FALSE)


# ============================================================================ # 
# 2. MIGRATORY PERIOD
# ============================================================================ # 
# DEF: Defining the migratory period as start of surveys to June 1st, and 
# exporting new data frames for each season of each year.

# --- 2.1 MIGRATION 2024 --- #
migratory_data_2024 <- yard_data_2024[yard_data_2024$Date <= "2024-06-01",]
write.csv(migratory_data_2024, file = "2 - Cleaned/cleaned_migratory_data_2024.csv", row.names = FALSE)

# --- 2.2 MIGRATION 2025 --- #
migratory_data_2025 <- yard_data_2025[yard_data_2025$Date <= "2025-06-01",]
write.csv(migratory_data_2025, file = "2 - Cleaned/cleaned_migratory_data_2025.csv", row.names = FALSE)


# ============================================================================ # 
# 3. BREEDING PERIOD
# ============================================================================ # 
# DEF: Defining breeding period as June 2nd to end of surveys, and exporting new 
# data frames for each season of each year. 

# --- 3.1 BREEDING 2024 --- #
breeding_data_2024 <- yard_data_2024[yard_data_2024$Date > "2024-06-01",]
write.csv(breeding_data_2024, file = "2 - Cleaned/cleaned_breeding_data_2024.csv", row.names = FALSE)

# --- 3.2 BREEDING 2025 --- #
breeding_data_2025 <- yard_data_2025[yard_data_2025$Date > "2025-06-01",]
write.csv(breeding_data_2025, file = "2 - Cleaned/cleaned_breeding_data_2025.csv", row.names = FALSE)



# ============================================================================ # 
# 4. COMBINED SEASONS
# ============================================================================ # 
# DEF: Stacking the data frames together, and export them as new data frames.

# --- 4.1 MIGRATORY DATA --- #
migratory_data <- rbind(migratory_data_2024, migratory_data_2025)
write.csv(migratory_data, file = "2 - Cleaned/cleaned_migratory_data.csv", row.names = FALSE)

# --- 4.2 BREEDING DATA --- #
breeding_data <- rbind(breeding_data_2024, breeding_data_2025)
write.csv(breeding_data, file = "2 - Cleaned/cleaned_breeding_data.csv", row.names = FALSE)




