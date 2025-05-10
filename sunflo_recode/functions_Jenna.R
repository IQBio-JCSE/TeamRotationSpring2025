# Jenna Stanislaw 

# Additional functions that have not already been defined in functions_SE.R
thermal_time_discrete <- function(prev_ThermalTime, T_m, WaterStressPhenology, T_b=4.8) {
  curr_ThermalTime <- 0 
  if (T_m > T_b ) {
    curr_ThermalTime <- (T_m - T_b) * (1 + WaterStressPhenology)
  }
  return (prev_ThermalTime + curr_ThermalTime)
}

### ROOTS ###
# Root growth is a linear function of temperature and stops at estimated maximum 
# soil rooting depth.
# RootDepth = with:
#  (RootGrowthRate × Tm, if RootDepth < RootDepthLimit RootDepthLimit, else
#    • RootGrowthRate = 0.7, root elongation rate (mm °Cd-1)
#    • RootDepthMax = 1800, maximum root depth (mm)
root_depth <- function(prev_RootDepth, Tm, RootGrowthRate = 0.7, RootDepthLimit = 1800) {
  growth <- RootGrowthRate * Tm
  curr_depth <- prev_RootDepth + growth
  if (curr_depth > RootDepthLimit) { # no further growth, max reached
    return (RootDepthLimit)
  } else {
    return (curr_depth)
  }
}

### WATER ###

# The water balance model treats the soil as a reservoir with three dynamic layers:
# surface layer (0-30 cm), root layer (30-rooting front), and soil layer (rooting front - soil depth) (Sarr et al., 2004)
# Rainfall, irrigation and evaporation only impacts the balance of the surface layer. 
# Water movement in the soil is assumed to be only vertical, with runoff and lateral 
# flow being ignored. Drainage occurs when the water content of a layer exceeds 
# its water retention capacity (defined by the SoilWaterCapacity parameter).
# WaterAvailablet = Rainfallt + Irrigationt −Evaporationt −Transpirationt −Drainaget
# Jenna note: Because drainage is determined based on how much the water of the surface
# layer exceeds the field capacity, it is not actually needed to calculate the
# amount of water available in the surface layer of soil
water_available_before_drain <- function(prev_WaterAvailable, Rainfall,Irrigation,Evaporation, Transpiration) {
  raw_calc <- (prev_WaterAvailable + Rainfall + Irrigation - Evaporation - Transpiration)
  if  (raw_calc < 0) {
    return (0)
  } else {
    return (raw_calc)
  }
}


# Units for water assume to be %. Units for density g/cm^3
# depth units assumed to be mm
convert_gravimetric_to_mm_water <- function(water_theta, soil_depth=300, soil_density=1.3) {
  depth <- max(300,soil_depth)
  return((water_theta/100)*(depth)*soil_density)
}

# Jenna note: check that this function is accurately calculating the %
# WaterAvailable units assumed to be mm
# Root or soil depth units assumed to be mm
# * 100 at end to convert back to a percent
water_content_theta <- function(WaterAvailable, RootDepth, SoilDepth_surface_layer=300,
                                SoilDensity=1.3) {
  depth <- max(300,RootDepth)
  return (WaterAvailable/(depth *SoilDensity) * 100)
}

# Jenna note: this is not defined explicity in the paper or the documentation
drainage <- function(WaterAvailable, RootDepth, SoilWaterCapacity_fc) {
  depth <- max(300,RootDepth)
  max_water <- convert_gravimetric_to_mm_water(SoilWaterCapacity_fc, depth)
  #percent_water <- water_content_theta(WaterAvailable,RootDepth) # Convert water to cm
  difference <- (WaterAvailable - max_water)
  # print(depth)
  # print(max_water)
  # print(WaterAvailable)
  if (difference <= 0) {
    return (0)
  } else {
    return (difference)
  }
}

# Soil evaporation is modeled with the same approach as crop transpiration.
# Evaporationt = (1 − RIE) × PET × WaterStressEvaporation
evaporation <- function(RIE, PET, WaterStressEvaporation) {
 return ((1 - RIE) * PET * WaterStressEvaporation) 
}

# From the paper:
# Water loss due to plant transpiration [Eqs. (15) and (16)] is a function of 
# potential transpiration rate [Eq. (13)], water effect on transpiration [Eq. (19)] 
# and root distribution over the two first layers (fR). This distribution is 
# made proportional to the thickness of each soil layer [Eq. (14)].
# dPTR=Kc×ETP×RIE , where Kc=1.2 [eq 13] -
# fR = zC1/(zC1 + zC2) [eq 14]
# dTRC1=fR×dPTR×WaterStressConductanceₜ if(zR>zC1) else dPTR×W.TR [eq. 15] - Layer 1 transpiration
# dTRC2=(1−fR)×dPTR×W.TR if(zR>zC1) else0 (16) - Layer 2 transpiration
# transpiration <- function(RIE, PET, ) {
#   
# }

# WaterStressEvaporation = square_root(x + 1) − square_root(x)
# The relative soil evaporation is based on Ritchie (1981) two-stage model, 
# where soil evaporation is reduced as a function (WaterStressEvaporation) of
# the number of days since last water input (x)
water_stress_evaporation <- function(x) {
  return (sqrt(x+1)-sqrt(x))
}

# Crop transpiration rate correspond to the water demand scaled by the reduction
# of transpiration under water deficit (control of stomatal conductance).
# Transpirationt = WaterDemandt × WaterStressConductancet
transpiration <- function(WaterDemand, WaterStressConductance) {
  return (WaterDemand * WaterStressConductance)
}

# Water demand is a function of crop light interception and potential evapotranspiration.
# WaterDemandt = RIEt × PETt × Kc with Kc = 1.2, crop coefficient
water_demand <- function(RIE, PET, K_c = 1.2) {
  return (RIE * PET * K_c)
}

# The fraction of transpirable soil water (FTSW, Sinclair, 2005) accounts for the
# amount of soil water available to the plant within the root zone. FTSW is used 
# to drive function representing various physiological responses to water deficit 
# in the model.
# WaterStresst = FTSWt = WaterAvailablet/WaterTotalt
water_stress <- function(WaterAvailable, WaterTotal){
  if (WaterTotal == 0) {
    return (0)
  }
  return(WaterAvailable/WaterTotal)
}

# Total water available for the crop depends on rooting depth and soil texture and density.
# WaterTotalt = RootDeptht × SoilWaterCapacity × SoilDensity × (1 − StoneContent) 
# with SoilWaterCapacity = θfc −θwp, the difference between the gravimetric water
# content at field capacity and at wilting point.
# Jenna note: here, I use SoilWaterCapacity_available_water, beacuse the SoilWaterCapacity
# is defined in different ways in different parts of the documentation. This is precalculated
# before being passed into the function based on the definition given above
water_total <- function(RootDepth, SoilWaterCapacity_available_water, 
                         SoilDensity, StoneContent){
  return(RootDepth*SoilWaterCapacity_available_water*SoilDensity*(1-StoneContent))
}         

# Leaf expansion and plant transpiration rates are exponentially reduced with in-
# creased water deficit. The same response curve is used for transpiration (WaterStressConductance)
# and photosynthesis (WaterStressRUE).
# Depemds on a ∈ [−15.6; −2.3], genotype-dependent response parameter
water_stress_expansion <- function(WaterStress, a = -8.95) {
  return ( -1 + (2/(1+exp(a * WaterStress))))
}

water_stress_conductance <- function(WaterStress, a = -8.95) {
  return (1/(1+exp(4.5 * a * WaterStress)))
}

# Accelerated crop developement under water deficit is modeled as a function 
# plant sensitivity to water deficit.
# WaterStressPhenologyt = a × (1 − WaterStressConductancet)
# with a = 0.1, scaling parameter for water-stress plant heating
water_stress_phenology <- function(WaterStressConductance, a = 0.1){
  return (a * (1 - WaterStressConductance))
}

# The effect of soil water content on net mineral nitrogen mineralization is
# described by a linear function (Mary et al., 1999; Valé, 2006).
# WaterStressMineralization = (1 − y0) × RelativeWaterContent + y0
# with:
# • y0 = 0.2, relative nitrogen mineralization rate at wilting point
# • RelativeWaterContent = (θ−θwp)/(θfc−θwp) , relative water content in surface layer. 
# Jenna note: Here, fp and wp theta value represents various version of the 
# SoilWaterCapacity, for field capacity and wilting point respectively
# The current theta values is represented as WaterContentTheta in the main script
water_stress_mineralization <- function(RelativeWaterContent, y0 = 0.2) {
  return ((1-y0) * RelativeWaterContent * y0)
}


relative_water_content <- function(theta, theta_wp, theta_fc) {
  if (theta < theta_wp) {
    return (0)
  } else {
    return ((theta - theta_wp)/(theta_fc - theta_wp))
  }
}

### NITROGEN ###

# The mineral nitrogen content of the soil layers (kg ha-1) depends on nitrogen 
# fertilization, mineralization, leaching, denitrification, and plant uptake. 
# The amount of nitrogen added to the surface layer from fertilization depends 
# on a threshold of water input (5 mm) for solubilization and nitrogen use efficiency, 
# which is modeled as a linear function of crop growth rate (g m -2 °Cd-1) (Limaux et al., 1999).
# Leaching is the product of drained water (Drainage) and the nitrogen concentration from the soil layer concerned.
# SoilNitrogenContentt = Fertilizationt + Mineralizationt − Leachingt − Denitrificationt − NitrogenUptaket
soil_nitrogen_content <- function(prev_SoilNitrogenContent, Fertilization, Mineralization,Leaching, Denitrification, NitrogenUptake) {
  cur_content <- (prev_SoilNitrogenContent + Fertilization + Mineralization - Leaching - Denitrification - NitrogenUptake)
  if (cur_content < 0) {
    return (0)
  } else {
    return (cur_content)
  }
} 

# Nitrogen mineralization takes place in surface layer and is impacted by relative 
# soil water content and temperature.
# MineralizationRatet = PotentialMineralizationRate × WaterStressMineralizationt × ThermalStressMineralizationt
mineralization_rate <- function(PotentialMineralizationRate,  WaterStressMineralization, 
                                ThermalStressMineralization) {
  return (PotentialMineralizationRate * WaterStressMineralization * ThermalStressMineralization)
}

# Denitrification occurs when the surface soil layer is water saturated and is a 
# function of air temperature (Sinclair and Muchow, 1995).
# DenitrificationRatet =6×exp(a×Tm−b)
# with:
#  • Tm, daily mean air temperature (°C);
#  • a = 0.07738 and b = 6.593 (Sinclair and Muchow, 1995)
denitrification_rate <- function(Tm, a = 0.07738, b = 6.59) {
  return (6 * exp(a * Tm -b))
}

# Soil nitrogen is absorbed in the transpirational stream (mass flow).
# NitrogenSupplyt = NitrogenUptaket
# NitrogenUptakeRatet = TranspirationRatet × SoilNitrogenConcentrationt
nitrogen_uptake_rate <- function(TranspirationRate, SoilNitrogenConcentration) {
  return (TranspirationRate * SoilNitrogenConcentration)
}

# Crop nitrogen demand is driven by the nitrogen dilution in the biomass produced.
# Two thresholds (critical and maximal) for plant nitrogen concentration 
# (% dry matter) were thus experimentally defined by monitoring nitrogen accumulation
# in relation to crop biomass for various fertilization levels (0–160 kg ha-1) 
# in field (Debaeke et al., 2012).
# CropNitrogenConcentration = min(a, a × CropBiomass−b)
# with:
#  • CropBiomass, daily shoot biomass (t ha-1);
#  • CropNitrogenConcentrationCritical is defined with a = 4.53 and b = 0.42;
#  • CropNitrogenConcentrationMaximum is defined with a = 6.49 and b = 0.44;
crop_nitrogen_concentration <- function(CropBiomass, a, b) {
  if (CropBiomass == 0) {
    crop_dependent_N <- 0
  } else {
    crop_dependent_N <- a * (CropBiomass)**(-b)
  }
  min_N <- min(a,crop_dependent_N)
  return ( min_N )
}   

# The critical crop nitrogen uptake is defined as the minimum nitrogen uptake necessary to achieve maximum biomass accumulation.
# NitrogenDemandt = CropNitrogenConcentrationCriticalt × CropBiomasst 
nitrogen_demand <- function(CropNitrogenConcentrationCritical, CropBiomass) {
  return (CropNitrogenConcentrationCritical * CropBiomass)
}

# Nitrogen stress index (Nitrogen Nutrition Index, NNI, see Lemaire and Meynard, 1997),
# is based on the ratio of actually absorbed nitrogen (NitrogenSupply, kg ha-1) 
# to the critical nitrogen amount needed to satisfy the demand (NNitrogenDemand, kg ha-1 ).
# NitrogenStresst = NitrogenSupplyt/NitrogenDemandt = NNI 
# Jenna note: according to the paper, this is the cumulative N absorbed. There is 
# a second term called INNI, which is the instantaneous NNI
nitrogen_nutrition_index <- function(NitrogenSupply_all, NitrogenDemand) {
  # Here, NitrogenSupply is a vector for ALL timepoints
  if (NitrogenDemand == 0) {
    return (0)
  }
  return (sum(NitrogenSupply_all)/NitrogenDemand)
}

I_nitrogen_nutrition_index <- function(NitrogenSupply, NitrogenDemand) {
  if (NitrogenDemand == 0) {
    return (0)
  }
  return (NitrogenSupply/NitrogenDemand)
}

# The impact of nitrogen deficit on leaf expansion is a linear function of 
# nitrogen stress index (Brisson et al., 2009).
# NitrogenStressExpansiont = (1.75 × NNI − 0.75), if NNI > 0.6 ; else, 0.3
nitrogen_stress_expansion <- function(NNI) {
  if (NNI > 0.6) {
    return (1.75 * NNI - 0.75)
  } else {
    return (0.3)
  }
}

# The impact of nitrogen deficit on photosynthesis (RUE) is the ratio of daily nitrogen
# uptake rate to the daily critical nitrogen amount neeaded to satisfy the demand.
# NitrogenStressRUEt = NitrogenSupplyRatet/NitrogenDemandRatet
nitrogen_stress_rue <- function(NitrogenSupplyRate, NitrogenDemandRate) {
  if (NitrogenDemandRate == 0) {
    return (1) #if there is no demand, there is no stress effect from nitrogen
  } else if (NitrogenSupplyRate == 0) {
    return(0) #no nitrogen available
  } else {
    return (NitrogenSupplyRate/NitrogenDemandRate)
  }
}

# Jenna note: I made this function up. It could be wrong!
# Equation:
# SoilNitrogenConcentration = (SoilNitrogen Content [kg N/ha soil])/(% water in soil * SoilDensity * Soil Depth* (10^8 cm2/ha) * (1 ha/10^10 mm2))
# Final units should be kg Nitrogen / mm water
# soil_nitrogen_concentration <- function(SoilNitrogenContent, LayerDepth,
#                                         WaterContentTheta, SoilDensity = 1.3){
#   depth <- max(300, LayerDepth)
#   SoilWater <- (WaterContentTheta * SoilDensity * LayerDepth)
#   SoilWater_converted <- SoilWater * (10**8/10**10)
#   if (SoilWater_converted == 0 ) {
#     return (0)
#   }
#   return (SoilNitrogenContent/SoilWater_converted)
# }

# soil_nitrogen_concentration <- function(SoilNitrogenContent, LayerDepth,
#                                         WaterContentTheta, SoilDensity = 1.3){
#   depth <- max(300, LayerDepth)
#   # SoilWater <- (WaterContentTheta * SoilDensity * LayerDepth)
#   # SoilWater_converted <- SoilWater * (10**8/10**10)
#   # if (SoilWater_converted == 0 ) {
#   #   return (0)
#   # }
#   return (SoilNitrogenContent * (1/SoilDensity) * (1/WaterContentTheta/100) * (1/depth))
# }

# kg of Nitrogen per mm of water. Assumes the amount of nitrogen is 
# distributed evenly across 1 ha, and that the water is also distributed evenly
soil_nitrogen_concentration <- function(SoilNitrogenContent, WaterAvailable) {
  # water available should never be 0
  if (WaterAvailable == 0) { #if there is no water, the effective concentration is 0
    return (0)
  } else {
    return (SoilNitrogenContent/WaterAvailable)
  }

}


### LEAF ###x

# Potential expansion or senescence rate of leaf i is a function of thermal time
# and potential area of the leaf. 
# LeafExpansionRate = (Tm − Tb ) × PotentialLeafArea_i × a × (exp(−a(ThermalTime−LeafExpansionTime_i)) /(1+exp(−a(ThermalTime−LeafExpansionTime_i))^2
# LeafSenescenceRatei = (Tm−Tb)×LeafArea_i×a × (exp(−a(ThermalTime−LeafSenescenceTime_i)))/ (1+exp(−a(ThermalTime−LeafSenescenceTime_i)))^2
#  with:
#    • Tm = 25, mean air temperature (°C)
#    • Tb = 4.8, base temperature (°C)
#    • a = 0.01379
leaf_expansion_rate <- function(Tm, PotentialLeafArea, ThermalTime, 
                                LeafExpansionTime, Tb = 4.8, a = 0.01379){
  temp_diff <- Tm - Tb
  numerator <- exp(-a * (ThermalTime - LeafExpansionTime))
  denom <-  (1 + exp(-a * (ThermalTime - LeafExpansionTime)))**2
  return (temp_diff * PotentialLeafArea * a * (numerator/denom) )
}

leaf_senescence_rate <- function(Tm, LeafArea, ThermalTime, LeafSenescenceTime, 
                                 Tb = 4.8, a = 0.01379) {
  temp_diff <- Tm - Tb
  numerator <- exp(-a * (ThermalTime - LeafSenescenceTime))
  denom <-  (1 + (exp(-a * (ThermalTime - LeafSenescenceTime))))**2
  return (temp_diff * LeafArea * a * (numerator/denom) )
}

# Jenna note: below are redefined from SE's functions
# Individual leaf expansion is impacted by water and nitrogen stress during leaf
# longevity. Leaf senescence is only function of temperature. Active leaf area is the
# difference between total and senescent leaf area.
# Total Leaf Area
# TotalLeafArea_it = ∫(LeafExpansionRate_it × WaterStressExpansion_t × NitrogenStressExpansion_t) dt
total_leaf_area <- function(LeafExpansionRate, WaterStressExpansion, NitrogenStressExpansion) {
  # All inputs are vectors of length t, where t is current time (timestep)
  sum_prods <- LeafExpansionRate * WaterStressExpansion * NitrogenStressExpansion
  return (sum(sum_prods))
}

total_leaf_area_discrete <- function(prev_TotalLeafArea, LeafExpansionRate, WaterStressExpansion, NitrogenStressExpansion) {
  # Assumes TotalLeafArea is value at the previous timestep
  # and all other variables are just their value at the current timestep
  area_t <- LeafExpansionRate * WaterStressExpansion * NitrogenStressExpansion
  return (prev_TotalLeafArea + area_t)
}

# Senescent Leaf Area
# SenescentLeafArea_it = ∫(LeafSenescenceRate_it) dt
senescent_leaf_area <- function(LeafSenescenceRate) {
  # Assumes the input is vector of length t
  return(sum(LeafSenescenceRate))
}

senescent_leaf_area_discrete <- function(prev_SenescentLeafArea, LeafSenescenceRate) {
  # Assumes the SenescentLeafArea at the previous timestep
  # and LeafSenescenceRate is the current rate for the current timestep
  return(prev_SenescentLeafArea + LeafSenescenceRate)
}

# Plant Leaf Area
# PlantLeafArea_t = Σ(TotalLeafArea_it - SenescentLeafArea_it) for i = 1 to LeafNumber
plant_leaf_area <- function(TotalLeafArea, SenescentLeafArea) {
  # Assumes inputs are vectors of length i, where i is the total number of leaves
  # diffs <- TotalLeafArea - SenescentLeafArea
  # non_zero_diffs <- diffs[diffs >= 0]
  # return (sum(non_zero_diffs))
  return (sum( TotalLeafArea - SenescentLeafArea))
}

### Leaf initiation parameters ### 
initialize_leaf_generation <- function(n_possible_leaves = 500, Phyllotherm_1=76.43,Phyllotherm_7=16.34) {
  init_leaf_tt <- numeric(n_possible_leaves) # Placeholder matrix, one spot for each leaf
  for (i in 1:n_possible_leaves) {
    if (i <= 6) {
      init_leaf_tt[i] <- (i * Phyllotherm_1)
    } else {
      init_leaf_tt[i] <- (i-5) * Phyllotherm_7 + 6 * Phyllotherm_1
    }
  }
  return (init_leaf_tt)
}

init_leaf_expansion_time <- function(LeafInitiationTime, a = 0.01379) {
  return (LeafInitiationTime + (1/a))
}

init_potential_leaf_area <- function(PotentialLeafNumber, PotentialLeafSize, PotentialLeafProfile,
                                     a=-2.05, b=0.049) {
  enumerated_leaf_numbers <- seq(1,PotentialLeafNumber)
  leaf_profile_exp <- (enumerated_leaf_numbers-PotentialLeafProfile)/(PotentialLeafProfile -1)
  exp_a <- a * leaf_profile_exp**2
  exp_b <- b * leaf_profile_exp**3
  return (PotentialLeafSize * exp(exp_a + exp_b))
}

init_leaf_expansion_duration <- function(PotentialLeafNumber, PotentialLeafProfile,
                                         a=153, b=851.3, c=0.78) {
  enumerated_leaf_numbers <- seq(1,PotentialLeafNumber)
  exponent=(-1*(enumerated_leaf_numbers - PotentialLeafProfile)**2)/((c*PotentialLeafNumber)**2)
  return (a+b*exp(exponent))
}

# Pest Stress
# PestStressRUE - effects of expressing pest-resistance-related genes
terp_expression_stress_rue <- function(Tm, pest_resistance_genetic) {
  T_b = 30 # in this case, "base" temp is 30 C
  constant = 0.02 #Based on calculations from https://pmc.ncbi.nlm.nih.gov/articles/PMC10445015 and 
  # https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/epdf/10.1002/bit.20237?saml_referrer
  base_terpene_expression <- 0.004
  
  # based on expriments from https://www.sciencedirect.com/science/article/pii/S0168192323000928
  terpene_expr_increase_per_C <- (0.22/(43-30)) 
  if (Tm > T_b) {
    terpene_expression <- pest_resistance_genetic + (Tm - T_b) * terpene_expr_increase_per_C * pest_resistance_genetic
  } else {
    terpene_expression  <- pest_resistance_genetic
  }
  norm_terpene_expression <- terpene_expression / base_terpene_expression # normalize terpenoid expression
  TerpeneStressRUE <- 1 - (constant * norm_terpene_expression)
  
  # TODO: should this have a lower limit of 1? Lower expression should not result
  # in a faster growth rate... 
  if (TerpeneStressRUE > 1) {
    return (1)
  } else if (TerpeneStressRUE < 0) {
    return (0)
  } else {
    return (TerpeneStressRUE)
  }
}

# Optional TODO: add effect of pest presence on biomass... 



