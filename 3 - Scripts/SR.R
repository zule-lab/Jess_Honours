# 02-Jan-2026              CREATING SPECIES RICHNESS DATA FRAMES

# DESCRIPTION: 
# Extraction of species richness (SR) data from cleaned observation data
# (cleaned_yard_data.csv, cleaned_migratory_data.csv, cleaned_breeding_data.csv)
# for each visit in each yard in each season (breeding, migratory) and year (2024, 2025). 
# Extracted data will be transformed into 2 data frames for future analysis: 
  # 1) wide and 2) long.

# PACKAGES USED:
library(readr)
library(tidyverse)

# Import cleaned observation data frames:
total <- read_csv("cleaned_yard_data.csv")
migratory_2024 <- read_csv("cleaned_migratory_data_2024.csv")
migratory_2025 <- read_csv("cleaned_migratory_data_2025.csv")
migratory <- read_csv("cleaned_migratory_data.csv")
breeding_2024 <- read_csv("cleaned_breeding_data_2024.csv")
breeding_2025 <- read_csv("cleaned_breeding_data_2025.csv")
breeding <- read_csv("cleaned_breeding_data.csv")


########################### 1. WIDE SR DATA FRAME ##############################
# DEF: Presence-absence of each bird species in each visit/yard for each season and year.
# Two wide data frames will be made from each of the following:
  # a. total
  # b. migratory_2024
  # c. migratory_2025
  # d. migratory
  # e. breeding_2024
  # f. breeding_2025
  # g. breeding
# One wide data frame will have rows that represent 'visits' to a yard (i.e., 
# yard survey i.e., a unique yard-date combination). 
# The second wide data frame will have rows that represent individual yards.
# All wide data frames will have the following columns: yard, date, species_1, 
# species_2...etc. 

##### a. SR total wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
total_visit_pa <- total %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
  # every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
total_visit_wide <- total_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
total_yard_wide <- total %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(total_visit_wide, file="total_visit_wide.csv", row.names=FALSE)
write.csv(total_yard_wide, file="total_yard_wide.csv", row.names=FALSE)
  # moved to ~/Desktop/Jess_Honours/3 - Extraction



##### b. SR migratory_2024 wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
m24_visit_pa <- migratory_2024 %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
m24_visit_wide <- m24_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
m24_yard_wide <- migratory_2024 %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(m24_visit_wide, file="m24_visit_wide.csv", row.names=FALSE)
write.csv(m24_yard_wide, file="m24_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction

##### c. SR migratory_2025 wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
m25_visit_pa <- migratory_2025 %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
m25_visit_wide <- m25_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
m25_yard_wide <- migratory_2025 %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(m25_visit_wide, file="m25_visit_wide.csv", row.names=FALSE)
write.csv(m25_yard_wide, file="m25_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction

##### d. SR migratory wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
m_visit_pa <- migratory %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
m_visit_wide <- m_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
m_yard_wide <- migratory %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(m_visit_wide, file="m_visit_wide.csv", row.names=FALSE)
write.csv(m_yard_wide, file="m_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction



##### e. SR breeding_2024 wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
b24_visit_pa <- breeding_2024 %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
b24_visit_wide <- b24_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
b24_yard_wide <- breeding_2024 %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(b24_visit_wide, file="b24_visit_wide.csv", row.names=FALSE)
write.csv(b24_yard_wide, file="b24_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction

# Export Wide SR Presence-Absence Matrices
write.csv(m24_visit_wide, file="m24_visit_wide", row.names=FALSE)
write.csv(m24_yard_wide, file="m24_yard_wide", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction

##### f. SR breeding_2025 wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
b25_visit_pa <- breeding_2025 %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
b25_visit_wide <- b25_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
b25_yard_wide <- breeding_2025 %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(b25_visit_wide, file="b25_visit_wide.csv", row.names=FALSE)
write.csv(b25_yard_wide, file="b25_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction






##### g. SR breeding wide #####
# DEF: Creation of two wide data frames: one with visits as rows and the other 
# with yards as rows.

# 1. VISITS
# Create a yard visit ID and reduce to data frame to presence.
b_visit_pa <- breeding %>%  # pa for presence-absence
  mutate(Date = as.Date(Date)) %>%  # ensure Date is Date class
  distinct(Code, Date, Bird.code) %>% # removes repeated detections of same species within a visit
  mutate(present = 1)
# every remaining row means “species X was present during survey Y”

# Make wide presence-absence matrix
b_visit_wide <- b_visit_pa %>%
  pivot_wider(
    id_cols = c(Code, Date), # rows are visits
    names_from = Bird.code, # columns are bird alpha codes
    values_from = present, # presence = 1, absence = 0
    values_fill = 0
  )

# 2. YARDS
# Collapse all visits together so that there is one row per yard. 
b_yard_wide <- breeding %>%
  distinct(Code, Bird.code) %>%  # species ever seen in each yard
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = Code,
    names_from = Bird.code,
    values_from = present,
    values_fill = 0
  )

# Export Wide SR Presence-Absence Matrices
write.csv(b_visit_wide, file="b_visit_wide.csv", row.names=FALSE)
write.csv(b_yard_wide, file="b_yard_wide.csv", row.names=FALSE)
# moved to ~/Desktop/Jess_Honours/3 - Extraction







############################ 2. LONG SR DATA FRAME #############################
# DEF: Creating one STACKED long data frame containing the species richness for 
# each yard.
# Where each row is a visit/yard and the columns are SR and wide data frame 
# identifier. The data frame identifiers will be the following:
  # a. migration_2024
  # b. migration_2025
  # c. migration
  # d. breeding_2024
  # e. breeding_2025
  # f. breeding
  # g. total

# Create function to calculate yard species richness from wide dataframe
yard_richness <- function(yard_wide) {
  yard_wide %>%
    mutate(richness = rowSums(across(-Code))) %>% # -Unknown, -`NA`
    select(Code, richness)
}

# Cleaning the wide data frames of NA and Unknown before SR calculation
total_yard_wide_clean <- subset(total_yard_wide, select = -c(`NA`, Unknown))
m24_yard_wide_clean <- subset(m24_yard_wide, select = -c(`NA`))
m25_yard_wide_clean <- subset(m25_yard_wide, select = -c(`NA`,Unknown))
m_yard_wide_clean <- subset(m_yard_wide, select = -c(`NA`, Unknown))
b24_yard_wide_clean <- subset(b24_yard_wide, select = -c(`NA`))
b25_yard_wide_clean <- subset(b25_yard_wide, select = -c(`NA`, Unknown))
b_yard_wide_clean <- subset(b_yard_wide, select = -c(`NA`, Unknown))

# Create stacked long data frame of species richness in yards
SR_long <- bind_rows(
  total         = yard_richness(total_yard_wide_clean),
  mig_2024      = yard_richness(m24_yard_wide_clean),
  mig_2025      = yard_richness(m25_yard_wide_clean),
  mig_total     = yard_richness(m_yard_wide_clean),
  breed_2024    = yard_richness(b24_yard_wide_clean),
  breed_2025    = yard_richness(b25_yard_wide_clean),
  breed_total   = yard_richness(b_yard_wide_clean),
  .id = "dataset"
)

# Export Wide SR Presence-Absence Matrices
write.csv(SR_long, file="SR_long.csv", row.names=FALSE)

