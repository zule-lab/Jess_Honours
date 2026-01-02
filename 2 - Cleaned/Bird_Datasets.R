# 03-Dec-2025                CLEANING THE DATASETS


############################### EXTRACTING YARDS ###############################
# Importing data and extracting only observations from yards

# 2024 yard data
data_2024 <- 
  read_csv("~/Desktop/Jess_Honours/1 - Input/ndg_cleaneddata_2024.csv")

yard_data_2024 <- data_2024[data_2024$Landtype=="yard", ]

# 2025 yard data
data_2025 <- 
  read_csv("~/Desktop/Jess_Honours/1 - Input/ndg_cleaneddata_2025.csv")

yard_data_2025 <- data_2025[data_2025$Landtype=="yard",]

# Export a new dataset with just yard data from all years
all_yard_data <- rbind(yard_data_2024, yard_data_2025)
write.csv(all_yard_data, file="cleaned_yard_data.csv", row.names=FALSE)



############################### MIGRATORY PERIOD ###############################
# Defining the migratory period as start of surveys to June 1st, and exporting the data each year
# 2024
migratory_data_2024 <- yard_data_2024[yard_data_2024$Date <= "2024-06-01",]
write.csv(migratory_data_2024, file = "cleaned_migratory_data_2024.csv", row.names = FALSE)

# 2025
migratory_data_2025 <- yard_data_2025[yard_data_2025$Date <= "2024-06-01",]
write.csv(migratory_data_2025, file = "cleaned_migratory_data_2025.csv", row.names = FALSE)



################################ BREEDING PERIOD ###############################
# Defining breeding period as June 2nd to end of surveys
# 2024
breeding_data_2024 <- yard_data_2024[yard_data_2024$Date > "2024-06-01",]
write.csv(breeding_data_2024, file = "cleaned_breeding_data_2024.csv", row.names = FALSE)

# 2025
breeding_data_2025 <- yard_data_2025[yard_data_2025$Date > "2024-06-01",]
write.csv(breeding_data_2025, file = "cleaned_breeding_data_2025.csv", row.names = FALSE)


################################# NEW DATASETS #################################
# Stack the datasets together, and export them as new datasets
# Migratory data
migratory_data <- rbind(migratory_data_2024, migratory_data_2025)
write.csv(migratory_data, file = "cleaned_migratory_data.csv", row.names = FALSE)

# Breeding data
breeding_data <- rbind(breeding_data_2024, breeding_data_2025)
write.csv(breeding_data, file = "cleaned_breeding_data.csv", row.names = FALSE)




