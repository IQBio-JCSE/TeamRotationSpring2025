# Jenna Stanislaw

# Script to run re-coded version of Sunflo model
# When referring to "docs", see: rsunflo documentation at https://github.com/picasa/rsunflo/blob/master/inst/doc/documentation_model.pdf 
# April 2025

run_sunflo <- function(climate_data, pest_resistance_genetic, working_directory) {
  # Import functions defined in other files
  source(file.path(working_directory, "functions_Jenna.R"))
  source(file.path(working_directory, "functions_SE.R"))
  
  # TODO:modify below depending on how climate data is passed in from SE's R model.
  # Note that column names below may need to be updated as well
  
  # climate_data <- read.table(
  #   "truncated_AUZ_2014.txt", # Path to test file 
  #   header = TRUE,
  #   sep = "\t",
  #   stringsAsFactors = FALSE
  # )

  # Environment variables which are predefined or passed in :
  
  Rainfall <- climate_data$Pluie #from the climate data file
  PET <- climate_data$ETP #from the climate data file as ETP
  T_m <- (climate_data$Tmax + climate_data$Tmin)/2 #from climate file
  Radiation <- climate_data$RAD #from climate file
  # TODO: add pest climate info
  
  harvest <- nrow(climate_data)# Length of harvest, predefined and calculated based on the climate file. 
  
  ## TODO: Intro a variable genetic parameters
  # a_water_stress = runif(1, min=-15.6, max=-2.3)
  # a_water_stress=-8.95
  ##
  
  # Initialize a vector and set specific dates with nitrogen or water doses
  fert_days = c(30,60,90,120)
  fert_values = c(20,20,20,20)
  Fertilization <- numeric(harvest) # predefined values 
  Fertilization[c(fert_days)] <- c(fert_values)
  
  water_days <- c(70,80) # estimating number of days until flowering for H. annuus
  water_values <- c(60,60)
  Irrigation <- numeric(harvest) #from the climate data file MAYBE? no, from ote
  Irrigation[c(water_days)] <- c(water_values)
  
  RootGrowthRate <- 0.7 # As defined in docs, p 11
    
  SowingDensity <- 7.0 # As defined in docs, p 6
  
  StoneContent <- soil_parameters$StoneContent
  SoilDensity <- soil_parameters$SoilDensity_0_30 # Same for all depths for now
  RootDepthLimit <- soil_parameters$RootDepthLimit # Not sure if it should be this or RootDepthLimit (?) 
  
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
  LeafExpansionDuration <- init_leaf_expansion_duration(PotentialLeafNumber, PotentialLeafProfile)
  LeafSenescenceTime <- LeafExpansionTime + LeafExpansionDuration   
  PotentialLeafArea <- init_potential_leaf_area(PotentialLeafNumber, PotentialLeafSize, PotentialLeafProfile) 
  
  SoilWaterCapacity_wp <- soil_parameters$SoilWaterCapacity_Wilting_30_Root # wilting point, same for all depths
  SoilWaterCapacity_fc <- soil_parameters$SoilWaterCapacity_30_Root #Field capacity, same for all depths for default val
  SoilWaterCapacity_available_water <- SoilWaterCapacity_fc -SoilWaterCapacity_wp 
  
  
  # Initialize vectors for things that have a value for each timepoints
  # May be clearer to put this in a dataframe? 
  CropBiomass <- numeric(harvest) #TODO: check length adjustment is correct
  RIE <- numeric(harvest)
  LAI <- numeric(harvest)
  PlantLeafArea <- numeric(harvest)
  
  TotalLeafArea <- matrix(ncol=harvest, nrow = 0)
  SenescentLeafArea <-  matrix(ncol = harvest, nrow = 0)
  #mat <- matrix(v, nrow = 1)
  
  #LeafExpansionRate <- numeric(harvest)
  #LeafSenescenceRate <- numeric(harvest)
  
  WaterStress <- rep(1,harvest)
  WaterStressExpansion <- numeric(harvest)
  Evaporation <- numeric(harvest)
  Transpiration <- numeric(harvest)
  Drainage <- numeric(harvest)
  WaterTotal <- numeric(harvest)
  WaterDemand <- numeric(harvest)
  WaterStressConductance <- numeric(harvest)
  TranspirationRate <- numeric(harvest)
  Leaching <- numeric(harvest)
  Denitrification <- numeric(harvest)
  WaterStressMineralization <- numeric(harvest)
  WaterStressRUE<-  rep(1,harvest) #numeric(harvest)
  
  RootDepth <- numeric(harvest)
    
  NitrogenStressExpansion <- numeric(harvest)
  NitrogenSupply <- numeric(harvest)
  NitrogenSupplyRate  <- numeric(harvest)
  NitrogenDemand <- numeric(harvest)
  NitrogenDemandRate <-  numeric(harvest)
  CropNitrogenConcentrationCritical <- numeric(harvest)
  NitrogenUptake <- numeric(harvest)
  NNI <- numeric(harvest) # not sure if this is necessary to save? 
  Mineralization <- numeric(harvest) 
  MineralizationRate <- numeric(harvest) 
  SoilNitrogenConcentration <- numeric(harvest)
  NitrogenStressRUE <-  rep(1,harvest) #numeric(harvest)
  
  PAR <- numeric(harvest)
  RUE <- numeric(harvest)
  
  WaterStressPhenology <- numeric(harvest)
  ThermalTime <- numeric(harvest)
  ThermalStressRUE <-  rep(1,harvest) #numeric(harvest + 1)
  ThermalStressMineralization <-numeric(harvest)
  RelativeWaterContent <- numeric(harvest)
  
  # Seed nitrogen and water with initial values at index 1
  SoilNitrogenContent <- numeric(harvest)
  SoilNitrogenContent[1] <- 20 # As defined in docs, p 14. Using the larger value for now because treating everything as one layer
  WaterContentTheta <- numeric(harvest) 
  WaterContentTheta[1] <- SoilWaterCapacity_fc # according to paper, this is usually equal to field capactiy
  WaterAvailable <- numeric(harvest)
  WaterAvailable[1] <- convert_gravimetric_to_mm_water(WaterContentTheta[1]) # assuming no roots, just consider the surface layer of soil (first 30 mm)
  SoilNitrogenConcentration[1] <- soil_nitrogen_concentration(SoilNitrogenContent[1],
                                                   WaterAvailable[1])
  # SoilNitrogenConcentration[1] <- soil_nitrogen_concentration(SoilNitrogenContent[1],
  #                                                             300, WaterContentTheta[1])
  
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
  WaterAvailable_beforedrainage <- numeric(harvest)
  WaterStressEvaporation<- numeric(harvest)
  
  ## NEW : terpene/terpenoid synthesis pathway stress
  # We expect this will negatively impact the increase in biomass
  TerpeneStressRUE <- numeric(harvest)
  
  # Connecting functions, will need to import most functions from sarah elizabeth
  for (t in 2:harvest) {
    #T = t +1
  
    # Water stress
    x <- days_since_water_input(Rainfall[t], Irrigation[t],x) # should be number of days since last water input
    WaterStressEvaporation[t] <- water_stress_evaporation(x) 
    Evaporation[t] <- evaporation(RIE[t-1], PET[t], WaterStressEvaporation[t]) #RIE defined below, may have to move this up
    WaterDemand[t] <- water_demand(RIE[t-1], PET[t])
    WaterStressConductance[t] <- water_stress_conductance(WaterStress[t-1]) #NDY
    Transpiration[t] <- transpiration(WaterDemand[t], WaterStressConductance[t]) # NDY
    # NDY . don't actually need drainage in this function. BUT, add if statment, cannot exceed SoilWaterCapcity_fc
    WaterAvailable_beforedrainage[t] <- water_available_before_drain(WaterAvailable[t-1], Rainfall[t],Irrigation[t],Evaporation[t], Transpiration[t]) 
    
    RootDepth[t] <- root_depth(RootDepth[t-1], T_m[t], RootGrowthRate, RootDepthLimit)
    Drainage[t] <- drainage(WaterAvailable_beforedrainage[t], RootDepth[t], SoilWaterCapacity_fc) # NDY , did not see this defined in the documentation
    WaterAvailable[t] <- WaterAvailable_beforedrainage[t] - Drainage[t]
    
    WaterContentTheta[t] <- water_content_theta(WaterAvailable[t], RootDepth[t]) # NDY
    RelativeWaterContent[t] <- relative_water_content(WaterContentTheta[t], SoilWaterCapacity_wp, SoilWaterCapacity_fc) # NDY
    
    WaterTotal[t] <- water_total(RootDepth[t], SoilWaterCapacity_available_water, SoilDensity, StoneContent) # NDY
    WaterStress[t] <- water_stress(WaterAvailable[t], WaterTotal[t]) # NDY
    WaterStressExpansion[t] <- water_stress_expansion(WaterStress[t]) # NDY
    
    WaterStressPhenology[t] <- water_stress_phenology(WaterStressConductance[t]) # NDY
    ThermalTime[t] <- thermal_time_discrete(ThermalTime[t-1], T_m[t], WaterStressPhenology[t])
   
    TranspirationRate <- Transpiration[t] # using same logic as other places
    Leaching[t] <- Drainage[t] * SoilNitrogenConcentration[t-1] #not defined in documentation, depends on drainage (also not defined)
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
                                NitrogenSupply[t-1]) 
  
    SoilNitrogenConcentration[t] <- soil_nitrogen_concentration(SoilNitrogenContent[t], WaterAvailable[t])
    # SoilNitrogenConcentration[t] <- soil_nitrogen_concentration(SoilNitrogenContent[t-1],RootDepth[t],
    #                                                             WaterContentTheta[t], SoilDensity) # NDY , see jenna_notes
    
    NitrogenSupplyRate[t] <- nitrogen_uptake_rate(Transpiration[t], SoilNitrogenConcentration[t]) #NDY , same thing as nitrogen update rate
   
    # Same as NitrogenUptake, no functions for this in the documentation
    # Given because we look at one day at a time, the rate per day can
    # be multiplied by the number of days we are looking at (1) to get the 
    # nitrogen supply/uptake for that day
    NitrogenSupply[t] <- NitrogenSupplyRate[t] * 1 # this is the same thing as nitrogen uptake
    
    # Values for a and b below are based on critical N concentration
    CropNitrogenConcentrationCritical[t] <- crop_nitrogen_concentration(CropBiomass[t-1], a=4.53, b=0.42) # NDY, assuming crop nitrogen conc calcs all by one function
    NitrogenDemand[t] <- nitrogen_demand(CropNitrogenConcentrationCritical[t], CropBiomass[t-1]) #NDY
    NNI[t] <- nitrogen_nutrition_index(NitrogenSupply, NitrogenDemand[t]) # NDY
    NitrogenStressExpansion[t] <- nitrogen_stress_expansion(NNI[t]) # NDY
    
    # Leaf growth
    # Check: will next leaf grow?
    
    if ( LeafNumber < PotentialLeafNumber && ThermalTime[t] >= LeafInitiationTime[LeafNumber + 1]) {
      LeafNumber <- LeafNumber + 1
      SenescentLeafArea <- rbind(SenescentLeafArea, numeric(harvest))
      TotalLeafArea  <- rbind(TotalLeafArea, numeric(harvest))
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
        leaf_data$LeafExpansionRate[i] <- leaf_expansion_rate(T_m[t], PotentialLeafArea[i], # NDY
                                            ThermalTime[t], LeafExpansionTime[i]) 
        
        TotalLeafArea[i,t] <- total_leaf_area_discrete(TotalLeafArea[i,t-1], leaf_data$LeafExpansionRate[i], WaterStressExpansion[t], NitrogenStressExpansion[t])
        
        leaf_data$LeafSenescenceRate[i] <- leaf_senescence_rate(T_m[t], TotalLeafArea[i,t], ThermalTime[t], LeafSenescenceTime[i]) # NDY, fill in later
        SenescentLeafArea[i,t] <- senescent_leaf_area_discrete(SenescentLeafArea[i,t-1],leaf_data$LeafSenescenceRate[i]) 
      }
    
      PlantLeafArea[t] <- plant_leaf_area(TotalLeafArea[,t], SenescentLeafArea[,t]) # review existing function
    }  
    
    
    
    LAI[t] <- leaf_area_index(SowingDensity, PlantLeafArea[t])
    
    # Radiation stress
    RIE[t] <- radiation_interception(LAI[t])
    
    # Calc PAR
    PAR[t] <- photosynthetically_active_radiation(Radiation[t]) # rename this from PAR in SE's functions
    
    # Calc RUE
    ThermalStressRUE[t] <- thermal_stress_rue(T_m[t]) # NDY
    
    # NDY , same formulas for water stress expansion, water stress RUE, water stress
    # leaf expansion, and water stress transpiration, according to SUNFLO pub.
    WaterStressRUE[t] <- water_stress_expansion(WaterStress[t]) # NDY
    
    # Below is not defined, but if we assume that the nitrogen demand 
    NitrogenDemandRate[t] <- NitrogenDemand[t] 
    NitrogenStressRUE[t] <- nitrogen_stress_rue(NitrogenSupplyRate[t], NitrogenDemandRate[t]) # NDY Not well defined. ratio of daily update rate to daily demand 
    
    
    # RUE[t] <- radiation_use_efficiency(ThermalTime[t], ThermalStressRUE[t], 
    #                                    WaterStressRUE[t], NitrogenStressRUE[t])
    
    TerpeneStressRUE[t] <- terp_expression_stress_rue(T_m[t], pest_resistance_genetic)
    # Adding in effects of expressing terpenoid genes to RUE
    RUE[t] <- radiation_use_efficiency(ThermalTime[t], ThermalStressRUE[t], 
                                       WaterStressRUE[t], NitrogenStressRUE[t],
                                       TerpeneStressRUE[t])
    
    # Final output, crop yield for entire harvest
    CropBiomass[t] <- crop_biomass(PAR[t], RIE[t],RUE[t], CropBiomass[t-1])
  }
  CropBiomass <- CropBiomass*(10 ** 6/ 10 ** 4) # Covert from t/ha to g/m^2
  HarvestIndex <- cultivar_parameters$PotentialHarvestIndex
  CropYield <- crop_yield(CropBiomass[1:harvest], HarvestIndex) #note: fix spelling
  
  # Calc additional outputs 
  # Other parameters may be set as constants ? - may need to rewrite function 
  # Also requires nitrogen absorbed
  NitrogenAbsorbed <- cumsum(NitrogenSupply) # I think this is correct?
  # OilContent <- oil_content(PotentialOilContent, NNI, T_m, RUE, LAI)
  
  # Save into output dataframe
  output <- data.frame(
    TemperatureAirMin = climate_data$Tmin,
    TemperatureAirMax = climate_data$Tmax,
    TemperatureAirMean = T_m,
    Radiation = Radiation,
    PET = PET,
    Rainfall = Rainfall,
    ThermalTime = ThermalTime,
    #PhenoStage
    WaterStress = WaterStress,
    WaterStressConductance = WaterStressConductance,
    WaterStressRUE = WaterStressRUE, # note this is also WaterStressConductance from photosynthesis according to docs
    WaterSupplyDemandRatio <- WaterDemand/WaterAvailable, # Not 100% sure this is correct formula
    ThermalStressRUE <- ThermalStressRUE,
    NitrogenAbsorbed <- NitrogenAbsorbed,
    NitrogenNutritionIndex <- NNI,
    NitrogenStressRUE <- NitrogenStressRUE,
    LAI = LAI,
    RIE = RIE,
    RUE = RUE,
    CropBiomass = CropBiomass,
    CropYield = CropYield_harvest,
    TerpeneStressRUE = TerpeneStressRUE
    # a_water_stress = a_water_stress
    #OilContent = OilContent
  )
  
  #outpath = "sample_out.csv"
  # outpath = paste0("output/sample_out_", sprintf("%.2f", a_water_stress),".csv")
  # print(outpath)
  # write.csv(output, outpath)
  
  return (output)
}







