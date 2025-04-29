# Jenna Functions

#### FROM THE RSUNFLO REPO #####
# summarise timed output variables 
#' @export indicate
indicate <- function(x, integration="crop", Tb=4.8) {
  
  # Définition des périodes d'intégration
  # semis - levee
  SE <- x$PhenoStage == 1
  # levée - fin maturité
  EH <- (x$PhenoStage > 1 & x$PhenoStage < 6)
  # levée - debut maturité
  EM <- (x$PhenoStage > 1 & x$PhenoStage < 5)
  # levée - floraison
  EF <- (x$PhenoStage == 2 | x$PhenoStage == 3)
  # initiation florale - début maturité
  FIM <- (x$PhenoStage == 3 | x$PhenoStage == 4)
  # floraison - début maturité
  FM <- x$PhenoStage == 4
  # floraison - fin maturité
  FH <- (x$PhenoStage == 4 | x$PhenoStage == 5)
  # début maturité - fin maturité
  MH <- x$PhenoStage == 5
  
  
  switch(integration,
    
    crop = {
      # Calcul des indicateurs
      o <- data.frame(

        # Phenologie
        D_SE = sum(SE),
        D_EF = sum(EF),
        D_FM = sum(FM),
        D_MH = sum(MH),
        TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
        
        # Ressources environnementales
        SGR = sum(x$GR[EH] * 0.48), # PAR
        SRR = sum(x$RR[EH]),
        SPET = sum(x$PET[EH]),
        SCWD = sum(x$RR[EH] - x$PET[EH]),
        
        # Contraintes hydriques
        ## basés sur FTSW
        SFTSW = sum(1 - x$FTSW[EH]),
        NET = sum(x$ETPET[EH] < 0.6),
        SFHTR = sum(1 - x$FHTR[EH]),
        SFHRUE = sum(1 - x$FHRUE[EH]), 
        
        # Contraintes azotées
        # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
        SNNI = sum(1 - x$NNI[EH & x$NNI <1]), #  déficit d'azote 
        SNAB = last(x$NAB[EH]),  # quantité totale d'azote absorbé 
        SFNRUE = sum(1 - x$FNRUE[EH]),
        
        # Contraintes thermiques
        SFTRUE = sum(1 - x$FTRUE[EH]),
        NHT = sum(x$TM[EH] > 28),
        NLT = sum(x$TM[EH] < 20),
        SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
        SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
        
        # Évolution de la surface foliaire
        LAI = max(x$LAI[EH]),
        LAD = sum(x$LAI[EH]), 
        
        # Rayonnement intercepté (PAR)
        SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
        
        # Photosynthèse
        MRUE = mean(x$RUE[EH]),
        
        # Biomasse accumulée
        STDM = max(x$TDM[EH]),
        
        # Performances
        GY = max(x$GY),
        OC = max(x$OC)
      ) 
    },
         
    phase = {       
       # Calcul des indicateurs
       o <- data.frame(
         
         # Phenologie
         D_SE = sum(SE),
         D_EF = sum(EF),
         D_FM = sum(FM),
         D_MH = sum(MH),
         
         # Ressources environnementales
         # Somme de rayonnement
         SGR = sum(x$GR[EH]),
         SGR_EF = sum(x$GR[EF]),
         SGR_FM = sum(x$GR[FM]),
         SGR_MH = sum(x$GR[MH]),
         
         # Cumul de précipitations
         SRR = sum(x$RR[EH]),
         SRR_EF = sum(x$RR[EF]),
         SRR_FM = sum(x$RR[FM]),
         SRR_MH = sum(x$RR[MH]),
         
         # Cumul d'évapotranspiration potentielle
         SPET = sum(x$PET[EH]),
         SPET_EF = sum(x$PET[EF]),
         SPET_FM = sum(x$PET[FM]),
         SPET_MH = sum(x$PET[MH]),
         
         # Déficit hydrique climatique : sum(P-PET)
         SCWD = sum((x$RR-x$PET)[EH]),
         SCWD_EF = sum((x$RR-x$PET)[EF]),
         SCWD_FM = sum((x$RR-x$PET)[FM]),
         SCWD_MH = sum((x$RR-x$PET)[MH]),
         
         # Déficit hydrique édaphique : mean(ET/PET)
         MET = mean(x$ETPET[EH]),
         MET_EF = mean(x$ETPET[EF]),
         MET_FM = mean(x$ETPET[FM]),
         MET_MH = mean(x$ETPET[MH]),
         
         # Déficit hydrique édaphique qualitatif : sum(ET/PET < 0.6)
         NET = sum(x$ETPET[EH] < 0.6),
         NET_EF = sum(x$ETPET[EF] < 0.6), 
         NET_FM = sum(x$ETPET[FM] < 0.6),
         NET_MH = sum(x$ETPET[MH] < 0.6),
         
         # Déficit hydrique édaphique quantitatif : sum(1-FTSW)
         SFTSW = sum(1 - x$FTSW[EH]),
         SFTSW_EF = sum(1 - x$FTSW[EF]), 
         SFTSW_FM = sum(1 - x$FTSW[FM]),
         SFTSW_MH = sum(1 - x$FTSW[MH]),
         
         # Effet de la contrainte hydrique sur la photosynthèse : sum(1-FHRUE)
         SFHRUE = sum(1 - x$FHRUE[EH]),
         SFHRUE_EF = sum(1 - x$FHRUE[EF]), 
         SFHRUE_FM = sum(1 - x$FHRUE[FM]),
         SFHRUE_MH = sum(1 - x$FHRUE[MH]),
         
         # Effet de la contrainte hydrique sur la transpiration : sum(1-FHTR)
         SFHTR = sum(1 - x$FHTR[EH]),
         SFHTR_EF = sum(1 - x$FHTR[EF]), 
         SFHTR_FM = sum(1 - x$FHTR[FM]),
         SFHTR_MH = sum(1 - x$FHTR[MH]),
         
         # Somme de température 
         TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
         TT_SE = sum((x$TM[SE] - Tb)[(x$TM[SE] - Tb) > 0]),
         TT_EF = sum((x$TM[EF] - Tb)[(x$TM[EF] - Tb) > 0]),
         TT_FM = sum((x$TM[FM] - Tb)[(x$TM[FM] - Tb) > 0]),
         TT_MH = sum((x$TM[MH] - Tb)[(x$TM[MH] - Tb) > 0]),       
         
         # Contraintes thermiques
         SFTRUE = sum(1 - x$FTRUE[EH]),
         SFTRUE_EF = sum(1 - x$FTRUE[EF]), 
         SFTRUE_FM = sum(1 - x$FTRUE[FM]),
         SFTRUE_MH = sum(1 - x$FTRUE[MH]),
         
         # heat stress
         NHT = sum(x$TM[EH] > 28),
         NHT_EF = sum(x$TM[EF] > 28),
         NHT_FM = sum(x$TM[FM] > 28),
         NHT_MH = sum(x$TM[MH] > 28),
         SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
         SHT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="high")),
         SHT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="high")),
         SHT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="high")),
         
         # cold stress
         NLT = sum(x$TM[EH] < 20),
         NLT_EF = sum(x$TM[EF] < 20),
         NLT_FM = sum(x$TM[FM] < 20),
         NLT_MH = sum(x$TM[MH] < 20),
         SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
         SLT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="low")),
         SLT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="low")),
         SLT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="low")),
         
         # nitrogen stress 
         # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
         # absorbed nitrogen
         SNAB = max(x$NAB[EH]),
         SNAB_EF = max(x$NAB[EF]),
         SNAB_FM = max(x$NAB[FM]),
         SNAB_EM = max(x$NAB[EM]),
         SNAB_MH = max(x$NAB[MH]),
         
         # nitrogen nutrition index 
         SNNI = sum(1 - x$NNI[EH & x$NNI <1]),
         SNNI_EF = sum(1 - x$NNI[EF & x$NNI <1]),
         SNNI_FM = sum(1 - x$NNI[FM & x$NNI <1]),
         SNNI_MH = sum(1 - x$NNI[MH & x$NNI <1]),
         
         # nitrogen impact on phytosynthesis
         SFNRUE = sum(1 - x$FNRUE[EH]),
         SFNRUE_EF = sum(1 - x$FNRUE[EF]),
         SFNRUE_FM = sum(1 - x$FNRUE[FM]),
         SFNRUE_MH = sum(1 - x$FNRUE[MH]),
      
         # NNI at flowering
         NNI_F = x$NNI[FM][1],
         
         # Nombre de jours INN < 0.8 jusqu'à M0
         NNNID_EM = sum(x$NNI[EM] < 0.8),
         NNNIE_EM = sum((x$NNI[EM] > 1.2) & (x$NNI[EM] < 2)),
         
         # Indice foliaire maximum
         LAI = max(x$LAI),
         
         # Durée de surface foliaire : sum(x$LAI)
         LAD = sum(x$LAI[EH]),
         
         # Rayonnement intercepté (PAR)
         SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
         SIR_EF = sum(x$RIE[EF] * x$GR[EF] * 0.48),
         SIR_FM = sum(x$RIE[FM] * x$GR[FM] * 0.48),
         SIR_MH = sum(x$RIE[MH] * x$GR[MH] * 0.48),
         
         # Photosynthèse
         MRUE = mean(x$RUE[EH]),
         
         # Biomasse
         STDM = max(x$TDM[EH]),
         STDM_F = x$TDM[FM][1],
         
         # Performance
         GY = max(x$GY),
         OC = max(x$OC)
       ) 
     }         
  )
  return(o)
}

###### END: FROM SUNFLO REPO ##############

# Variables which are passed in, list:
'''
SowingDensity <- crop_density from default params
Rainfall <- #from the climate data file
Irrigation <- #from the climate data file MAYBE?
PET <- #from the climate data file as ETP
StoneContent <- think this is pre=defined and constant
SoilDensity <- predefined from default params
T_m <- from climate file
RootGrowthRate <- predefined?
RootDepthLimit <- predefined ; but how is it difference from RootDepthMax
Radiation <- from climate file
ThermalTimeVegetative <- predefined, TDE1
ThermalTimeFlowering <- predefined, TDF1 
ThermalTimeMaturity <- predefined, TDM3
ThermalTimeSenescence <- predefined, TDM0
harvest <- # Date of harvest, predefined and calculated based on the climate file
x <- 0 #days since last water input
PotentialOilContent <- # predefined value
Fertilization <- # prefdefined values 
RelativeWaterContent <- # Should be a constant based on theta values???
PotentialMineralizationRate <- # Contstant
PotentialLeafSize <- # from default params
PotentialLeafNumber <- # from default params 
LeafInitiationTime <- initialize_leaf_generation(n_possible_leaves, PotentialLeafNumber)
LeafNumber <- 0 # starts at 0 and increases. used to search LeafInitiationTimes vector
LeafExpansionTime <- init_leaf_expansion_time(LeafInitiationTime)
LeafExpansionDuration <- init_leaf_expansion_duration(PotentialLeafNumber, PotentialLeafSize,
                                         PotentialLeafProfile)
LeafSenscenseTime <- LeafExpansionTime + LeafExpansionDuration
PotentialLeafArea <- init_potential_leaf_area(LeafNumber, PotentialLeafSize, PotentialLeafProfile) # NDY , see .rmd file for a and b values

SoilWaterCapacity_wp <- # wilting point, same for all depths
SoilWaterCapacity_fc <- #Field capcity, same for all depths for default val
SoilWaterCapacity_available_water <- SoilWaterCapacity_fc -SoilWaterCapacity_wp # NDY , calculated from predefined values

area <- size of plot?? will need to define this somewhere
'''

initialize_leaf_generation <- function(n_possible_leaves = 500, Phyllotherm_1=76.43,Phyllotherm_7=16.34) {
  #default n_possible_leaves picked an arbitrary value for this... could adjust later if needed
  init_leaf_tt <- numeric(n_possible_leaves)
  for (i in 1:n_possible_leaves) {
    if (i <= 6) {
      init_leaf_tt[i] <- (i * Phyllotherm_1)
    } else {
      init_leaf_tt[i] <- ((i-5) * Phyllotherm_7) + 6 * Phyllotherm_1
    }
  }
  return (init_leaf_tt)
}

init_leaf_expansion_time <- function(LeafInitiationTime, a = 0.01379) {
  return (LeafInitiationTime + (1/a))
}

init_potential_leaf_area <- function(PotentialLeafNumber, PotentialLeafSize, PotentialLeafProfile,
                                     a=2.05, b=0.049) {
  enumerated_leaf_numbers <- seq(1,PotentialLeafNumber)
  leaf_profile_exp <- (enumerated_leaf_numbers-PotentialLeafProfile)/(PotentialLeafProfile -1)
  exp_a <- a * leaf_profile_exp**2
  exp_b <- b * leaf_profile_exp**3
  return (PotentialLeafSize * exp(exp_a + exp_b))
}

init_leaf_expansion_duration <- function(PotentialLeafNumber, PotentialLeafSize,
                                         PotentialLeafProfile,
                                          a=153, b=851.3, c=0.78) {
  enumerated_leaf_numbers <- seq(1,PotentialLeafNumber)
  exponent=(-1*(enumerated_leaf_numbers - PotentialLeafProfile)**2)/((c*PotentialLeafNumber)**2)
  return (a+b*exp(exponent))
}

# Initialize vectors for things that have a value for each timepoints
# May be clearer to put this in a dataframe? 
CropBiomass <- numeric(harvest+1) #TODO: check length adjustment is correct
RIE <- numeric(harvest+1)
LAI <- numeric(harvest+1)
PlantLeafArea <- numeric(harvest+1)

TotalLeafArea <- numeric(harvest+1)
SenescentLeafArea <- numeric(harvest+1)

LeafExpansionRate <- numeric(harvest+1)
LeafSenescenceRate <- numeric(harvest+1)

WaterStress <- numeric(harvest+1)
WaterStressExpansion <- numeric(harvest+1)
WaterAvailable <- numeric(harvest+1)
Evaporation <- numeric(harvest+1)
Transpiration <- numeric(harvest+1)
Drainage <- numeric(harvest+1)
WaterTotal <- numeric(harvest+1)
WaterDemand <- numeric(harvest+1)
WaterStressConductance <- numeric(harvest+1)
  
NitrogenStressExpansion <- numeric(harvest+1)
NitrogenSupply <- numeric(harvest+1)
NitrogenDemand <- numeric(harvest+1)
CropNitrogenConcentraionCritical <- numeric(harvest+1)
NitrogenUptake <- numeric(harvest+1)
NNI <- numeric(harvest+1) # not sure if this is necessary to save? 
SoilNitrogenConcentration <- numeric(harvest+1)

PAR <- numeric(harvest+1)
RUE <- numeric(harvest+1)

WaterStressPhenology <- numeric(harvest+1)
ThermalTime <- numeric(harvest+1)
ThermalStressRUE <- numeric(harvest + 1)
RelativeWaterContent <- numeric(harvest + 1)
WaterContentTheta <- numeric(harvest + 1)

CropYield <- numeric(harvest+1)

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
# harvest = final_day + 1
for (t in 2:harvest) {
  
  # Calc RIE
  # Water stress
  x <- days_since_water_input(Rainfall[t], Irrigation[t],x) # should be number of days since last water input
  WaterStressEvaporation <- water_stress_evaporation(x, Rainfall[t], Irrigation[t]) #NDY some function to calc this, will need to look more later
  Evaporation[t] <- evaporation(RIE[t], PET[t], WaterStressEvaporation) #RIE defined below, may have to move this up
  WaterDemand[t] <- water_demand(RIE[t], PET[t]) # NDY
  WaterStressConductance[t] <- water_stress_conductance(WaterStress[t]) #NDY
  Transpiration[t] <- transpiration(WaterDemand[t], WaterStressConductance[t]) # NDY
  WaterAvailable[t] <- water_available(Rainfall[t],Irrigation[t],Evaporation[t],
                                       Transpiration[t]) # NDY . don't actually need drainage in this function. BUT, add if statment, cannot exceed SoilWaterCapcity_fc
  # Add something here to calcualte the gravimetric water content at the time
  area <- ()
  WaterContentTheta[t] <- water_content_theta(WaterAvailable[t], area)
  RelativeWaterContent[t] <- relative_water_content(WaterContentTheta[t]) # NDY
  
  Drainage[t] <- drainage(WaterAvailable[t], SoilWaterCapacity_fc) # did not see this defined in the documentation
  RootDepth[t] <- root_depth(RootGrowthRate, T_m[t], RootDepthLimit)
  WaterTotal[t] <- water_total(RootDepth[t], SoilWaterCapacity_available_water, SoilDensity, StoneContent) # NDY
  WaterStress[t] <- water_stress(WaterAvailable[t], WaterTotal[t]) # NDY
  WaterStressExpansion[t] <- water_stress_expansion(WaterStress[t]) # NDY
  
 

  TranspirationRate[t] <- Transpiration[t] # using same logic as other places
  Leaching[t] <- Drainage[t] * SoilNitrogenConcentration[t] #not defined in documentation, depends on drainage (also not defined)
  DenitrificationRate <- denitrification_rate(Tm) # NDY
  Denitrification[t] <- DenitrificationRate[t] * 1
  
  # Mineralization
  WaterStressMineralization[t] <- water_stress_mineralization(RelativeWaterContent[t])
  ThermalStressMineralization[t] <- thermal_stress_mineralization(Tm) 
  MineralizationRate[t] <- mineralization_rate(PotentialMineralizationRate,
                                               WaterStressMineralization,
                                               ThermalStressMineralization[t]) # NDY
  Mineralization[t] <- MineralizationRate[t] * 1 # check with other rate things 
  
  # Nitrogen stress
  SoilNitrogenContent[t] <- soil_nitrogen_content(Fertilization[t],
                              Mineralization[t],Leaching[t], Denitrification[t],
                              NitrogenUptake[t]) # NDY

  SoilNitrogenConcentration[t] <- soil_nitrogen_concentration(SoilNitrogenContent[t],RootDepth[t],
                                                              WaterContentTheta[t], SoilDensity) # NDY , see jenna_notes
  
  NitrogenSupplyRate[t] <- nitrogen_uptake_rate(TranspirationRate[t], SoilNitrogenConcentration[t]) #NDY , same thing as nitrogen update rate
 
  # Same as NitrogenUptake, no functions for this in the documentation
  # Given because we look at one day at a time, the rate per day can
  # be multiplied by the number of days we are looking at (1) to get the 
  # nitrogen supply/uptake for that day
  NitrogenSupply[t] <- NitrogenSupplyRate[t] * 1 # this is the same thing as nitrogen uptake
  
  CropNitrogenConcentrationCritical[t] <- crop_nitrogen_concentration(CropBiomass[t], a=4.53, b=0.42) #NDY, assuming crop nitrogen conc calcs all by one function
  NitrogenDemand[t] <- nitrogen_demand(CropNitrogenConcentrationCritical[t], CropBiomass[t]) #NDY
  NNI <- nitrogen_nutrition_index(NitrogenSupply[t], NitrogenDemand[t]) # NDY
  NitrogenStressExpansion[t] <- nitrogen_stress_expansion(NNI) # NDY
  
  # Leaf stuff
  # Check: will next leaf grow?
  if (ThermalTime[t] >= LeafInitiationTime[LeafNumber + 1]) {
    LeafNumber += 1
  }
  # Calculate these parameters for each existing leaf
  # I think total leaf area and senescent leaf area can be re-calc each time
  leaf_data <- data.frame(
    LeafSenescenceRate <- numeric(LeafNumber),
    SenescentLeafArea <- numeric(LeafNumber),
    
    LeafExpansionRate <- numeric(LeafNumber),
    LeafExpansionTime <- numeric(LeafNumber),
    
    TotalLeafArea <- numeric(LeafNumber)
  )
  
  for (i in 1:LeafNumber) {
    leaf_data$LeafSenescenceRate[i] <- leaf_senescence_rate(ThermalTime, LeafSenescenceTime[i]) # fill in later
    leaf_data$LeafExpansionRate[i] <- leaf_expansion_rate(Tm, PotentialLeafArea[i],
                                        ThermalTime[t], LeafExpansionTime[i]) # NDY
    leaf_data$SenescentLeafArea[i] <- senescent_leaf_area(LeafSenescenceRate[t])
    leaf_data$TotalLeafArea[i] <- total_leaf_area(LeafExpansionRate[t], WaterStressExpansion, NitrogenStressExpansion, t) # review function for this, should just be the sum?
  }
  
  # Do we need to pass in i, number of leaves? or is this deduced in the function?
  PlantLeafArea[t] <- plant_leaf_area(leaf_data$TotalLeafArea, leaf_data$SenescentLeafArea)
  LAI <- leaf_area_index(SowingDensity, PlantLeafArea[t])
  
  # Radiation stress
  RIE[t] <- radiation_interception(LAI[t])
  
  ### End RIE for now
  
  # Calc PAR
  PAR[t] <- PAR(Radiation[t]) # rename one of these to not have same name
  
  # Calc RUE
  WaterStressPhenology[t] <- water_stress_phenology(WaterStressConductance[t])
  ThermalTime[t] <- thermal_time(T_m[t], WaterStressPhenology[t])
  
  ThermalStressRUE[t] <- thermal_stress_rue(Tm[t]) # NDY
  
  # NDY , same formulas for water stress expansion, water stress RUE, water stress
  # leaf expansion, and water stress transpiration, according to SUNFLO pub.
  WaterStressRUE[t] <- water_stess_expansion(WaterStress[t]) 
  
  # Below is not defined, but if we assume that the nitrogen demand 
  NitrogenDemandRate[t] <- NitrogenDemand[t] 
  NitrogenStressRUE[t] <- nitrogen_stress_rue(NitrogenSupplyRate[t], NitrogenDemandRate[t]) # NDY Not well defined. ratio of daily update rate to daily demand 
  
  # Some of the below thermal time values should be constants I think, 
  # May need to modify function definition
  RUE[t] <- radiation_use_efficiency(ThermalTime[t], ThermalStressRUE[t], 
                                     WaterStressRUE[t], NitrogenStressRUE[t])
  
  # Final output, crop yield for entire harvest
  CropBiomass[t+1] <- crop_biomass(RIE[t], PAR[t], RUE[t], CropBiomass[t])
  CropYield[t] <- crop_yeild(CropBiomass[t],HarvestIndex[t])
}

CropYield_harvest <- crop_yeild(CropBiomass[harvest], HarvestIndex[harvest]) #note: fix spelling

# Expected outputs according to parameters_default sheet:
# ! before things that we do not have yet
# X before things we have
# ? for things I am not sure about how to calculate
'''
Calc from the climate file, as vectors: 
! TemperatureAirMin
! TemperatureAirMax
! TemperatureAirMean
! Radiation
! PET
! Rainfall

Below are all from the model:
X ThermalTime
! PhenoStage - not sure about this one, can it be calculated in post or is it necessary for steps of the model??? 
X WaterStress
? WaterStressConductance (transpiration)
? WaterStressConductance (photosynthesis) - different from above?
! WaterSupplyDemandRatio - should be easy to calc: water stress divided by water demand? 
! ThermalStressRUE - may need to include as part of RUE calculation
! NitrogenAbsorbed - might just be the sum of NitrogenSupply
X NitrogenNutritionIndex (this is the NNI)
! NitrogenStressRUE - may need to include as part of RUE calculation 
X LAI
X RIE
X RUE
X CropBiomass

Calc at end (?): 
X CropYield 
X OilContent - below, check because missing some vars in SE-defined function
'''

# Calc additional outputs 
# Other parameters may be set as constants ? - may need to rewrite function 
# Also requires nitrogen absorbed
NitrogenAbsorbed <- sum(NitrogenSupply) # I think this is correct?
OilContent <- oil_content(PotentialOilContent, NNI, TemperatureAirMean, RUE, LAI)

# Save into output dataframe
output <- data.frame(
  TemperatureAirMin = TemperatureAirMin,
  TemperatureAirMax = TemperatureAirMax,
  TemperatureAirMean = Tm,
  Radiation = Radiation,
  PET = PET,
  Rainfall = Rainfall,
  ThermalTime = ThermalTime,
  #PhenoStage
  WaterStress = WaterStress,
  WaterStressConductance = WaterStressConductance,
  WaterStressRUE = WaterStressRUE,
  WaterSupplyDemandRatio <- WaterAvailable/WaterDemand, # Not 100% sure this is correct formula
  ThermalStressRUE <- ThermalStressRUE, 
  NitrogenAbsorbed <- NitrogenAbsorbed,
  NitrogenNutritionIndex <- NNI,
  NitrogenStressRUE <- NitrogenStressRUE,
  LAI = LAI,
  RIE = RIE,
  RUE = RUE,
  CropBiomass = CropBiomass,
  CropYield = CropYield,
  OilContent = OilContent
)









