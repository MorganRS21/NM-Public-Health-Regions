library(tidycensus)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)


# Download the shapefile for New Mexico counties
nm_counties <- get_acs(state = "NM", geography = 'county', 
                       var = 'B01001_001', geometry = TRUE, output = 'wide')

# Define the public health regions
northwest_counties <- c("San Juan", "McKinley", "Cibola", "Sandoval", "Bernalillo", "Torrance", "Valencia")
northeast_counties <- c("Rio Arriba", "Los Alamos", "Taos", "Colfax", "Mora", "Harding", "Guadalupe", "Union", "San Miguel", "Santa Fe")
southeast_counties <- c("Lincoln", "Chaves", "De Baca", "Roosevelt", "Lea", "Curry", "Quay", "Eddy")
southwest_counties <- c("Catron", "Socorro", "Grant", "Sierra", "Doña Ana", "Otero", "Luna", "Hidalgo")

#Separate NAME into county 
nm_counties <- separate(nm_counties, col = NAME, into = c("County", "State"), sep = ",\\s", remove = FALSE)

# Remove the word 'County' from the 'County' column
nm_counties$County <- gsub(" County", "", nm_counties$County)


# Create a new variable indicating the public health region for each county
nm_counties <- nm_counties %>%
  mutate(PH_Region = case_when(
    County %in% northwest_counties ~ "Northwest",
    County %in% northeast_counties ~ "Northeast",
    County %in% southeast_counties ~ "Southeast",
    County %in% southwest_counties ~ "Southwest",
    TRUE ~ "Other"
  ))

# Filter the counties for each public health region
northwest <- nm_counties %>% filter(PH_Region == "Northwest")
northeast <- nm_counties %>% filter(PH_Region == "Northeast")
southeast <- nm_counties %>% filter(PH_Region == "Southeast")
southwest <- nm_counties %>% filter(PH_Region == "Southwest")

# Plot the boundaries of the public health regions
plot(nm_counties$geometry)
plot(northwest$geometry, col = "red", add = TRUE)
plot(northeast$geometry, col = "blue", add = TRUE)
plot(southeast$geometry, col = "green", add = TRUE)
plot(southwest$geometry, col = "purple", add = TRUE)

# Add legend
legend("topright", legend = c("Northwest", "Northeast", "Southeast", "Southwest"), fill = c("red", "blue", "green", "purple"))


# Export shapefile since it looks correct

nm_counties <- nm_counties %>% 
  mutate(Total_pop = B01001_001E) %>% 
  select(GEOID, NAME, County, State, Total_pop, PH_Region, geometry)

st_write(nm_counties, 'results/NM_PH_Regions.shp')


