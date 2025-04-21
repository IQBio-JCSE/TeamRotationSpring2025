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
SoilDensity 
T_m <- from climate file
RootGrowthRate <- predefined?
RootDepthLimit <- predefined ; but how is it difference from RootDepthMax
Radiation <- from climate file
ThermalTimeFlowering <- predefined 
ThermalTimeMaturity <- predefined
ThermalTimeSenescence <- predefined
harvest <- # Date of harvest, predefined and calcualted based on the climate file
'''


# Initialize vectors for things that have a value for each timepoints
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

PAR <- numeric(harvest+1)
RUE <- numeric(harvest+1)

PET <- numeric(harvest+1)

WaterStressPhenology <- numeric(harvest+1)
ThermalTime <- numeric(harvest+1)


# Connecting functions, will need to import most functions from sarah elizabeth
# harvest = final_day + 1
for (t in 2:harvest) {
  
  # Calc RIE
  # Water stress
  x <- () # should be number of days since last water input
  WaterStressEvaporation <- water_stress_evaporation(x, Rainfall[t], Irrigation[t]) #NDY some function to calc this, will need to look more later
  Evaporation[t] <- evaporation(RIE[t], PET[t], WaterStressEvaporation) #RIE defined below, may have to move this up
  WaterDemand[t] <- water_demand(RIE[t], PET[t]) # NDY
  WaterStressConductance[t] <- water_stress_conductance(WaterStress[t]) #NDY
  Transpiration[t] <- transpiration(WaterDemand[t], WaterStressConductance[t]) # NDY
  Drainage[t] <- drainage() # did not see this defined in the documentation
  WaterAvailable[t] <- water_available(Rainfall[t],Irrigation[t],Evaporation[t],
                                       Transpiration[t],Drainage[t]) # NDY
  RootDepth[t] <- root_depth(RootGrowthRate, T_m[t], RootDepthLimit)
  SoilWaterCapacity <- soil_water_capacity() # NDY , looks like this does not have to be vector? may  be predefined
  WaterTotal[t] <- water_total(RootDepth[t], SoilWaterCapacity, SoilDensity, StoneContent) # NDY
  WaterStress[t] <- water_stress(WaterAvailable[t], WaterTotal[t]) # NDY
  WaterStressxpansion[t] <- water_stress_expansion(WaterStress[t]) # NDY
  
  # Nitrogen stress
  NitrogenUptake[t] <- nitrogen_uptake() # did not see this defined in the documentation
  NitrogenSupply[t] <- nitrogen_supply(NitrogenUptake[t]) #NDY
  CropNitrogenConcentraionCritical[t] <- crop_nitrogen_concentration(CropBiomass[t], a=4.53, b=0.42) #NDY, assuming crop nitrogen conc calcs all by one function
  NitrogenDemand[t] <- nitrogen_demand(CropNitrogenConcentraionCritical[t], CropBiomass[t]) #NDY
  NNI <- nitrogen_nutrition_index(NitrogenSupply[t], NitrogenDemand[t]) # NDY
  NitrogenStressExpansion[t] <- nitrogen_stress_expansion(NNI) # NDY
  
  # Probably have to do something here for each leaf?
  LeafNumber <- () #NDY , don't see this in the paper
  for (i in 1:LeafNumber) {
    LeafSenescenceRate[t][i] <- leaf_senescence_rate(ThermalTime, LeafSenescenceTime[i]) # fill in later
    LeafExpansionRate[t][i] <- ()
    SenescentLeafArea[t][i] <- senescent_leaf_area(LeafSenescenceRate[t])
    TotalLeafArea[t][i] <- total_leaf_area(LeafExpansionRate[t], WaterStressExpansion[t], NitrogenStressExpansion[t]) # review function for this, should just be the sum?
  }
  
  PlantLeafArea[t] <- plant_leaf_area(TotalLeafArea[t], SenescentLeafArea[t]) # Do we need to pass in i, number of leaves? or is this deduced in the function?
  LAI <- leaf_area_index(SowingDensity, PlantLeafArea[t])
  
  # Radiation stress
  RIE[t] <- radiation_interception(LAI[t])
  
  ### End RIE for now
  
  # Calc PAR
  PAR[t] <- PAR(Radiation[t]) # rename one of these to not have same name
  
  # Calc RUE
  WaterStressPhenology[t] <- water_stress_phenology(WaterStressConductance[t])
  ThermalTime[t] <- thermal_time(T_m[t], WaterStressPhenology[t])
  RUE[t] <- radiation_use_efficiency(ThermalTime[t], ThermalTimeFlowering, ThermalTimeMaturity, ThermalTimeSenescence)
  
  # Final output, crop yield for entire harvest
  CropBiomass[t+1] <- crop_biomass(RIE[t], PAR[t], RUE[t], CropBiomass[t])

}

CropYield_harvest <- crop_yeild(CropBiomass[harvest], HarvestIndex[harvest]) #note: fix spelling

















