# Jenna Stanislaw

# Script to run re-coded version of Sunflo model
# When referring to "docs", see: rsunflo documentation at https://github.com/picasa/rsunflo/blob/master/inst/doc/documentation_model.pdf 
# April 2025

# Import functions defined in other files
source("functions_Jenna.R")
source("functions_SE.R")
source("climate_data.R")

# TODO: make this more easily modified
climate_data <- read.table(
  "truncated_AUZ_2014.txt", # Path to test file 
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)


# Variables which are predefined or passed in :

Rainfall <- climate_data$Pluie #from the climate data file
PET <- climate_data$ETP #from the climate data file as ETP
T_m <- (climate_data$Tmax + climate_data$Tmin)/2 #from climate file
Radiation <- climate_data$RAD #from climate file

harvest <- nrow(climate_data)# Length of harvest, predefined and calculated based on the climate file. 
  
# Initialize a vector and set specific dates with nitrogen or water doses
#fert_days = c()
#fert_values = c()
Fertilization <- numeric(harvest+1) # predefined values 
#Fertilization[c(fert_days)] <- c(fert_values)

water_days <- c(95,105) # estimating number of days until flowering for H. annuus
water_values <- c(60,60)
Irrigation <- numeric(harvest+1) #from the climate data file MAYBE? no, from ote
Irrigation[c(water_days)] <- c(water_values)

RootGrowthRate <- 0.7 # As defined in docs, p 11
  
SowingDensity <- 7.0 # As defined in docs, p 6

StoneContent <- soil_parameters$StoneContent
SoilDensity <- soil_parameters$SoilDensity_0_30 # Same for all depths for now
RootDepthLimit <- soil_parameters$RootDepthLimit 

ThermalTimeVegetative <- phenology_parameters$ThermalTimeVegetative
ThermalTimeFlowering <- phenology_parameters$ThermalTimeFlowering
ThermalTimeMaturity <- phenology_parameters$ThermalTimeMaturity
ThermalTimeSenescence <- phenology_parameters$ThermalTimeSenescence

PotentialOilContent <- cultivar_parameters$PotentialOilContent
PotentialMineralizationRate <- soil_parameters$PotentialMineralizationRate

PotentialLeafSize <- leaf_area_parameters$PotentialLeafSize
PotentialLeafNumber <- leaf_area_parameters$PotentialLeafNumber
PotentialLeafProfile <- leaf_area_parameters$PotentialLeafProfile

x <- 0 #days since last water input

# Leaf initiation parameters - all pre-defined
#n_possible_leaves=500
LeafInitiationTime <- initialize_leaf_generation(PotentialLeafNumber)
LeafNumber <- 0 # starts at 0 and increases. used to search LeafInitiationTimes vector
LeafExpansionTime <- init_leaf_expansion_time(LeafInitiationTime)
LeafExpansionDuration <- init_leaf_expansion_duration(PotentialLeafNumber, PotentialLeafSize,
                                         PotentialLeafProfile)
LeafSenescenceTime <- LeafExpansionTime + LeafExpansionDuration   
PotentialLeafArea <- init_potential_leaf_area(LeafNumber, PotentialLeafSize, PotentialLeafProfile) 

SoilWaterCapacity_wp <- soil_parameters$SoilWaterCapacity_Wilting_30_Root # wilting point, same for all depths
SoilWaterCapacity_fc <- soil_parameters$SoilWaterCapacity_30_Root #Field capacity, same for all depths for default val
SoilWaterCapacity_available_water <- SoilWaterCapacity_fc -SoilWaterCapacity_wp 


# Initialize vectors for things that have a value for each timepoints
# May be clearer to put this in a dataframe? 
CropBiomass <- numeric(harvest+1) #TODO: check length adjustment is correct
RIE <- numeric(harvest+1)
LAI <- numeric(harvest+1)
PlantLeafArea <- numeric(harvest+1)

TotalLeafArea <- matrix(ncol=harvest+1, nrow = 0)
SenescentLeafArea <-  matrix(ncol = harvest+1, nrow = 0)
#mat <- matrix(v, nrow = 1)

#LeafExpansionRate <- numeric(harvest+1)
#LeafSenescenceRate <- numeric(harvest+1)

WaterStress <- numeric(harvest+1)
WaterStressExpansion <- numeric(harvest+1)
Evaporation <- numeric(harvest+1)
Transpiration <- numeric(harvest+1)
Drainage <- numeric(harvest+1)
WaterTotal <- numeric(harvest+1)
WaterDemand <- numeric(harvest+1)
WaterStressConductance <- numeric(harvest+1)
TranspirationRate <- numeric(harvest+1)
Leaching <- numeric(harvest+1)
Denitrification <- numeric(harvest+1)
WaterStressMineralization <- numeric(harvest+1)
WaterStressRUE<- numeric(harvest+1)

RootDepth <- numeric(harvest+1)
  
NitrogenStressExpansion <- numeric(harvest+1)
NitrogenSupply <- numeric(harvest+1)
NitrogenSupplyRate  <- numeric(harvest+1)
NitrogenDemand <- numeric(harvest+1)
CropNitrogenConcentrationCritical <- numeric(harvest+1)
NitrogenUptake <- numeric(harvest+1)
NNI <- numeric(harvest+1) # not sure if this is necessary to save? 
Mineralization <- numeric(harvest+1) 
MineralizationRate <- numeric(harvest+1) 
SoilNitrogenConcentration <- numeric(harvest+1)
NitrogenStressRUE <- numeric(harvest+1)

PAR <- numeric(harvest+1)
RUE <- numeric(harvest+1)

WaterStressPhenology <- numeric(harvest+1)
ThermalTime <- numeric(harvest+1)
ThermalStressRUE <- numeric(harvest + 1)
ThermalStressMineralization <-numeric(harvest+1)
RelativeWaterContent <- numeric(harvest + 1)

# Seed nitrogen and water with initial values at index 1
SoilNitrogenContent <- numeric(harvest+1)
SoilNitrogenContent[1] <- 20 # As defined in docs, p 14. Using the larger value for now because treating everything as one layer
WaterContentTheta <- numeric(harvest + 1) 
WaterContentTheta[1] <- SoilWaterCapacity_fc # according to paper, this is usually equal to field capactiy
WaterAvailable <- numeric(harvest+1)
WaterAvailable[1] <- (WaterContentTheta[1]/100) * 30 * SoilDensity # assuming no roots, just consider the surface layer of soil (first 30 mm)

### Define other functions to process inputs
days_since_water_input <- function(rainfall, irrigation, current_x) {
  curr_water_input <- rainfall+irrigation
  if (curr_water_input <= 4) {
    return (current_x + 1)
  } else {
    return (0)
  }
}
### end functions

# Connecting functions, will need to import most functions from sarah elizabeth
for (t in 2:100) {
  #T = t +1

  # Water stress
  x <- days_since_water_input(Rainfall[t], Irrigation[t],x) # should be number of days since last water input
  WaterStressEvaporation <- water_stress_evaporation(x) 
  Evaporation[t] <- evaporation(RIE[t-1], PET[t], WaterStressEvaporation) #RIE defined below, may have to move this up
  WaterDemand[t] <- water_demand(RIE[t-1], PET[t])
  WaterStressConductance[t] <- water_stress_conductance(WaterStress[t-1]) #NDY
  Transpiration[t] <- transpiration(WaterDemand[t], WaterStressConductance[t]) # NDY
  WaterAvailable_beforedrainage <- water_available_before_drain(WaterAvailable[t-1], Rainfall[t],Irrigation[t],Evaporation[t], # NDY . don't actually need drainage in this function. BUT, add if statment, cannot exceed SoilWaterCapcity_fc
                                       Transpiration[t]) 
  
  RootDepth[t] <- root_depth(RootGrowthRate, T_m[t], RootDepthLimit)
  Drainage[t] <- drainage(WaterAvailable_beforedrainage, RootDepth[t], SoilWaterCapacity_fc) # NDY , did not see this defined in the documentation
  WaterAvailable[t] <- WaterAvailable_beforedrainage - Drainage[t]
  
  WaterContentTheta[t] <- water_content_theta(WaterAvailable[t], RootDepth[t]) # NDY
  RelativeWaterContent[t] <- relative_water_content(WaterContentTheta[t], SoilWaterCapacity_wp, SoilWaterCapacity_fc) # NDY
  
  WaterTotal[t] <- water_total(RootDepth[t], SoilWaterCapacity_available_water, SoilDensity, StoneContent) # NDY
  WaterStress[t] <- water_stress(WaterAvailable[t], WaterTotal[t]) # NDY
  WaterStressExpansion[t] <- water_stress_expansion(WaterStress[t]) # NDY
  
  WaterStressPhenology[t] <- water_stress_phenology(WaterStressConductance[t]) # NDY
  ThermalTime[t] <- thermal_time_discrete(ThermalTime[t-1], T_m[t], WaterStressPhenology[t])
 
  TranspirationRate <- Transpiration[t] # using same logic as other places
  Leaching[t] <- Drainage[t] * SoilNitrogenConcentration[t] #not defined in documentation, depends on drainage (also not defined)
  DenitrificationRate <- denitrification_rate(T_m[t]) # NDY
  Denitrification[t] <- DenitrificationRate * 1
  
  # Mineralization
  WaterStressMineralization[t] <- water_stress_mineralization(RelativeWaterContent[t]) # NDY
  ThermalStressMineralization[t] <- thermal_stress_mineralization(T_m[t]) # NDY
  MineralizationRate[t] <- mineralization_rate(PotentialMineralizationRate, # NDY
                                               WaterStressMineralization[t],
                                               ThermalStressMineralization[t]) 
  Mineralization[t] <- MineralizationRate[t] * 1 # check with other rate things 
  
  # Nitrogen stress
  SoilNitrogenContent[t] <- soil_nitrogen_content(SoilNitrogenContent[t-1], Fertilization[t], # NDY
                              Mineralization[t],Leaching[t], Denitrification[t],
                              NitrogenUptake[t]) 

  SoilNitrogenConcentration[t] <- soil_nitrogen_concentration(SoilNitrogenContent[t],RootDepth[t],
                                                              WaterContentTheta[t], SoilDensity) # NDY , see jenna_notes
  
  NitrogenSupplyRate[t] <- nitrogen_uptake_rate(TranspirationRate, SoilNitrogenConcentration[t]) #NDY , same thing as nitrogen update rate
 
  # Same as NitrogenUptake, no functions for this in the documentation
  # Given because we look at one day at a time, the rate per day can
  # be multiplied by the number of days we are looking at (1) to get the 
  # nitrogen supply/uptake for that day
  NitrogenSupply[t] <- NitrogenSupplyRate[t] * 1 # this is the same thing as nitrogen uptake
  
  # Values for a and b below are based on critical N concentration
  CropNitrogenConcentrationCritical[t] <- crop_nitrogen_concentration(CropBiomass[t-1], a=4.53, b=0.42) # NDY, assuming crop nitrogen conc calcs all by one function
  NitrogenDemand[t] <- nitrogen_demand(CropNitrogenConcentrationCritical[t], CropBiomass[t-1]) #NDY
  NNI[t] <- nitrogen_nutrition_index(NitrogenSupply[t], NitrogenDemand[t]) # NDY
  NitrogenStressExpansion[t] <- nitrogen_stress_expansion(NNI[t]) # NDY
  
  # Leaf growth
  # Check: will next leaf grow?
  
  if ( LeafNumber < PotentialLeafNumber && ThermalTime[t] >= LeafInitiationTime[LeafNumber + 1]) {
    LeafNumber <- LeafNumber + 1
    SenescentLeafArea <- rbind(SenescentLeafArea, numeric(harvest+1))
    TotalLeafArea  <- rbind(TotalLeafArea, numeric(harvest+1))
  }
  
  if (LeafNumber > 1) {
    # Calculate these parameters for each existing leaf
    # I think total leaf area and senescent leaf area can be re-calc each time
    leaf_data <- data.frame(
      LeafSenescenceRate <- numeric(LeafNumber),
      LeafExpansionRate <- numeric(LeafNumber)
    )
    
    # For each leaf, update its total leaf area and senescent leaf area
    for (i in 1:LeafNumber) {
      leaf_data$LeafSenescenceRate[i] <- leaf_senescence_rate(T_m[t], PotentialLeafArea[i], ThermalTime[t], LeafSenescenceTime[i]) # NDY, fill in later
      leaf_data$LeafExpansionRate[i] <- leaf_expansion_rate(T_m[t], PotentialLeafArea[i], # NDY
                                          ThermalTime[t], LeafExpansionTime[i]) 
      SenescentLeafArea[i,t] <- senescent_leaf_area_discrete(SenescentLeafArea[i,t-1],LeafSenescenceRate[i]) 
      TotalLeafArea[i,t] <- total_leaf_area_discrete(TotalLeafArea[i,t-1], LeafExpansionRate[i], WaterStressExpansion[t], NitrogenStressExpansion[t]) 
    }
  
    PlantLeafArea[t] <- plant_leaf_area(TotalLeafArea[,t], SenescentLeafArea[,t]) # review existing function
  }
  
  LAI[t] <- leaf_area_index(SowingDensity, PlantLeafArea[t])
  
  # Radiation stress
  RIE[t] <- radiation_interception(LAI[t])
  
  # Calc PAR
  PAR[t] <- photosynthetically_active_radiation(Radiation[t]) # rename this from PAR in SE's funtions
  
  # Calc RUE
  ThermalStressRUE[t] <- thermal_stress_rue(T_m[t]) # NDY
  
  # NDY , same formulas for water stress expansion, water stress RUE, water stress
  # leaf expansion, and water stress transpiration, according to SUNFLO pub.
  WaterStressRUE[t] <- water_stress_expansion(WaterStress[t]) # NDY
  
  # Below is not defined, but if we assume that the nitrogen demand 
  NitrogenDemandRate <- NitrogenDemand[t] 
  NitrogenStressRUE[t] <- nitrogen_stress_rue(NitrogenSupplyRate[t], NitrogenDemandRate) # NDY Not well defined. ratio of daily update rate to daily demand 
  
  # Some of the below thermal time values should be constants I think, 
  # May need to modify function definition
  RUE[t] <- radiation_use_efficiency(ThermalTime[t], ThermalStressRUE[t], 
                                     WaterStressRUE[t], NitrogenStressRUE[t])
  
  # Final output, crop yield for entire harvest
  CropBiomass[t] <- crop_biomass(RIE[t], PAR[t], RUE[t], CropBiomass[t-1])
  #CropYield[t] <- crop_yeild(CropBiomass[t],HarvestIndex[t])
}

HarvestIndex <- cultivar_parameters$PotentialHarvestIndex
CropYield_harvest <- crop_yeild(CropBiomass[harvest], HarvestIndex) #note: fix spelling


# Calc additional outputs 
# Other parameters may be set as constants ? - may need to rewrite function 
# Also requires nitrogen absorbed
NitrogenAbsorbed <- sum(NitrogenSupply) # I think this is correct?
#OilContent <- oil_content(PotentialOilContent, NNI, T_m, RUE, LAI)

# Save into output dataframe
# output <- data.frame(
#   TemperatureAirMin = climate_data$Tmin,
#   TemperatureAirMax = climate_data$Tmax,
#   TemperatureAirMean = T_m,
#   Radiation = Radiation,
#   PET = PET,
#   Rainfall = Rainfall,
#   ThermalTime = ThermalTime,
#   #PhenoStage
#   WaterStress = WaterStress,
#   WaterStressConductance = WaterStressConductance,
#   WaterStressRUE = WaterStressRUE, # note this is also WaterStressConductance from photosynthesis according to docs
#   WaterSupplyDemandRatio <- WaterAvailable/WaterDemand, # Not 100% sure this is correct formula
#   ThermalStressRUE <- ThermalStressRUE, 
#   NitrogenAbsorbed <- NitrogenAbsorbed,
#   NitrogenNutritionIndex <- NNI,
#   NitrogenStressRUE <- NitrogenStressRUE,
#   LAI = LAI,
#   RIE = RIE,
#   RUE = RUE,
#   CropBiomass = CropBiomass,
#   CropYield = CropYield
#   #OilContent = OilContent
# )









