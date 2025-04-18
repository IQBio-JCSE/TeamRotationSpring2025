# main

# Load parameters
source("parameters.R")

# Load climate data
source("climate_data.R")
climate_data <- load_climate_data("climate_data.csv")

# Load functions
# source("functions.R")

# Initialize variables
thermal_time_accumulated <- 0
biomass <- 0
root_depth <- 0
LAI <- 0

# Loop through climate data
for (day in 1:nrow(climate_data)) {
  Tm <- (climate_data$TemperatureAirMin[day] + climate_data$TemperatureAirMax[day]) / 2
  PAR <- climate_data$Radiation[day] * 0.48
  
  # Calculate thermal time
  thermal_time_accumulated <- thermal_time_accumulated + thermal_time(Tm, Tb)
  
  # Calculate RIE
  RIE <- radiation_interception(LAI, k)
  
  # Calculate RUE
  RUE <- radiation_use_efficiency(thermal_time_accumulated, TDF1, TDM0, TDM3, r0, rmax, rd, rmin, a, b)
  
  # Update biomass
  biomass <- crop_biomass(PAR, RIE, RUE, biomass)
  
  # Update root depth
  root_depth <- root_growth(Tm, 0.7, RootDepthLimit, root_depth)
  
  # Update LAI (example: assume constant plant leaf area)
  LAI <- leaf_area_index(7, 0.1)
}

# Print final results
cat("Final Biomass:", biomass, "g/m^2\n")
cat("Final Root Depth:", root_depth, "mm\n")