# Sarah Elizabeth's Functions
# vector for testing
test_vector <- c(1, 2, 3, 4, 5)

# adding functions from https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fpce.13001&file=pce13001-sup-0001-Data_S1.pdf

# Crop Yield ------------------------------------------------------------------
# At harvest time, crop yield is computed as proportion of total aerial biomass
# allocated to seeds (i.e. crop yield not defined before harvest).
# CropYieldharvest = CropBiomassharvest ×HarvestIndexharvest
 crop_yeild <- function(crop_biomass, harvest_index) {
  return(crop_biomass * harvest_index)
}

# Oil Content -----------------------------------------------------------------
# symbol    description                           unit            formula
# OC        Potential seed oil content            % dry
# SFTSW_FM  Edaphic water deficit (continuous)                    sum(1−FTSW)
# SFTSW_MH  Edaphic water deficit (continuous)                    sum(1−FTSW)
# SNAB_MH   Absorbed nitrogen                     kg.ha−1         max(NAB)
# NNNIE_EM  Nitrogen excess (discrete)            d               sum(NNI > 1.2)
# NHT_MH    Thermal stress, heat (discrete)       d               sum(TM > 34)
# MRUE_MH   Photosynthesis                        g.MJ−1 .m−2     mean(RUE)
# LAD_MH    Leaf area duration                                    sum(LAI)
# DENS      Plant density

# predict oil content at harvest time based on following coefficients (Andriana-solo et al., 2014)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) -18.702220 3.898791 -4.797 2.26e-06 ***
# OC 0.996473 0.059631 16.711 < 2e-16 ***
# SFTSW_FM 0.111097 0.026317 4.221 2.99e-05 ***
# SFTSW_MH 0.126438 0.041208 3.068 0.002297 **
# NNNIE_EM -0.068492 0.015455 -4.432 1.20e-05 ***
# SNAB_MH -0.035815 0.010669 -3.357 0.000862 ***
# NHT_MH -0.235708 0.049564 -4.756 2.75e-06 ***
# LAD_MH 0.007082 0.009191 0.771 0.441441
# MRUE_MH 21.052693 2.900957 7.257 2.01e-12 ***
# DENS 0.831619 0.172779 4.813 2.10e-06 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.516 on 408 degrees of freedom
# Multiple R-squared: 0.5022, Adjusted R-squared: 0.4913
# F-statistic: 45.74 on 9 and 408 DF, p-value: < 2.2e-16

oil_content <- function(OC, SFTSW_FM, SFTSW_MH, NNNIE_EM, SNAB_MH, NHT_MH, LAD_MH, MRUE_MH, DENS) {
  return(-18.702220 + 
         0.996473 * OC + 
         0.111097 * SFTSW_FM + 
         0.126438 * SFTSW_MH - 
         0.068492 * NNNIE_EM - 
         0.035815 * SNAB_MH - 
         0.235708 * NHT_MH + 
         0.007082 * LAD_MH + 
         21.052693 * MRUE_MH + 
         0.831619 * DENS)
}
# Crop Biomass Accumulation ---------------------------------------------------
# Intercepted light is main driver of biomass accumulation (CropBiomassRate),
# based on Monteith (1977) model. 
# DMt = DMt−1 + PAR ×RIE ×RUE

# CropBiomass_t = CropBiomass_t−1 + (PAR_t  ×RIE_t × RUE_t)
crop_biomass <- function(PAR, RIE, RUE, previous_biomass) {
  return(previous_biomass + (PAR * RIE * RUE))
}

# Photosynthetically active radiation -----------------------------------------
# (PAR) is 48% of total radiation (Monteith, 1977) <-- citation unsure about
# PAR_t = Radiation_t  × 0.48

# Jenna : renamed fxn to avoid confusion with variable
photosynthetically_active_radiation <- function(radiation) {
  return(radiation * 0.48)
}

# Harvest Index ---------------------------------------------------------------
# symbol  description                                 unit  formula
# NETR_EF Edaphic water deficit (discrete)            d     sum(ET/PET < 0.6)
# NETR_FM Edaphic water deficit (discrete)            d     sum(ET/PET < 0.6)
# NETR_MH Edaphic water deficit (discrete)            d     sum(ET/PET < 0.6)
# STDM_F  Aerial Biomass at flowering                 g.m−2 max(TDM
# STR_FH  Sum of water loss through transpiration     mm    sum(TR)
# TT_FH   Thermal time since flowering (4.8 C basis)  C.d   sum(TM−4.8)
# HI      Potential harvest index

# following coefficients used to predict harvest index at harvest time (Casade-
# baig et al., 2011).
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 9.370e-02 6.996e-02 1.339 0.182276
# STDM_F -1.552e-04 6.376e-05 -2.434 0.015982 *
# NETR_EF -2.828e-03 1.335e-03 -2.118 0.035650 *
# NETR_FM -2.557e-03 1.174e-03 -2.178 0.030813 *
# NETR_MH -1.940e-03 4.995e-04 -3.884 0.000148 ***
# STR_FH -3.907e-04 1.696e-04 -2.304 0.022464 *
# TT_FH 1.274e-04 3.190e-05 3.992 9.80e-05 ***
# HI 8.189e-01 1.540e-01 5.317 3.34e-07 ***
# ---
# Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
# Adjusted R-squared: 0.3036 F-statistic: 11.84 on 7 and 167 DF, p-value: 3.311e-12

harvest_index <- function(STDM_F, NETR_EF, NETR_FM, NETR_MH, STR_FH, TT_FH) {
  return(0.09370 - 
         0.0001552 * STDM_F - 
         0.002828 * NETR_EF - 
         0.002557 * NETR_FM - 
         0.001940 * NETR_MH - 
         0.0003907 * STR_FH + 
         0.0001274 * TT_FH + 
         0.8189) # TODO is this correct? Should it be 0.8189 * HI?
}

# Leaf Area Index (LAI) -------------------------------------------------------
# LAI_t = SowingDensity × PlantLeafArea_t
# SowingDensity: 7.0 (plants/m^2) number of plants per unit area 
leaf_area_index <- function(sowing_density = 7.0, plant_leaf_area) {
  return(sowing_density * plant_leaf_area)
}
# Radiation Interception Efficiency (RIE) -------------------------------------
# Beer–Lambert law used to model light interception assuming homogeneous
# distribution of leaves for a given soil area (LAI).

# RIE= 1−exp(−k × LAI_t)
# k: Light extinction coefficient during vegetative growth (Lecoeur et al., 2011)
# k=0.9 
radiation_interception <- function(LAI, k = 0.9) {
  return(1 - exp(-k * LAI))
}

# Jenna note: commenting these out for now to use definitions from other files

# # TODO LeafArea PlantLeafArea ----------------------------------------------------
# # Individual leaf expansion is impacted by water and nitrogen stress during leaf
# # longevity. Leaf senescence is only function of temperature. Active leaf area is the
# # difference between total and senescent leaf area.
# # Total Leaf Area
# # TotalLeafArea_it = ∫(LeafExpansionRate_it × WaterStressExpansion_t × NitrogenStressExpansion_t) dt
# total_leaf_area <- function(leaf_expansion_rate, water_stress_expansion, nitrogen_stress_expansion, time_steps) {
#   return(sum(leaf_expansion_rate * water_stress_expansion * nitrogen_stress_expansion * time_steps))
# }
# 
# # Senescent Leaf Area
# # SenescentLeafArea_it = ∫(LeafSenescenceRate_it) dt
# senescent_leaf_area <- function(leaf_senescence_rate, time_steps) {
#   return(sum(leaf_senescence_rate * time_steps))
# }
# 
# # Plant Leaf Area
# # PlantLeafArea_t = Σ(TotalLeafArea_it - SenescentLeafArea_it) for i = 1 to LeafNumber
# plant_leaf_area <- function(total_leaf_area, senescent_leaf_area, leaf_number) {
#   return(sum((total_leaf_area - senescent_leaf_area)[1:leaf_number]))
# }

# Explanation:
# total_leaf_area:

# Integrates the product of leaf_expansion_rate, water_stress_expansion, and nitrogen_stress_expansion over time (time_steps).
# senescent_leaf_area:

# Integrates the leaf_senescence_rate over time (time_steps).
# plant_leaf_area:

# Calculates the sum of the difference between total_leaf_area and senescent_leaf_area for all leaves up to leaf_number.
# Assumptions:
# time_steps is a vector representing the time intervals for integration.
# leaf_number is the total number of leaves to consider for the calculation.

# integrate(f, lower, upper, ...)
# f: The function to integrate (must take a single numeric argument and return a numeric value).
# lower: The lower limit of integration.
# upper: The upper limit of integration.
# ...: Additional arguments to pass to the function f.
# # Define a function to integrate
# f <- function(x) {
#   x^2  # Example: x^2
# }

# # Compute the integral of f(x) = x^2 from 0 to 1
# result <- integrate(f, lower = 0, upper = 1)

# # Print the result
# print(result$value)  # The integral value

# Radiation Use Efficiency (RUE) ----------------------------------------------
# piecewise function computes potential RUE based on thermal time and abiotic stresses
# increase in energy cost of biomass produced (oil content), exponential decrease of RUE during grain filling
radiation_use_efficiency <- function(thermal_time, thermal_stress_rue, water_stress_rue, nitrogen_stress_rue,
                                     thermal_time_flowering = phenology_parameters$ThermalTimeFlowering, 
                                     thermal_time_maturity = phenology_parameters$ThermalTimeMaturity, 
                                     thermal_time_senescence = phenology_parameters$ThermalTimeSenescence, 
                                     r0 = 1.0, #  initial RUE during vegetative stage
                                     rmax = 3.0, # maximum RUE during flowering,
                                     rd = 4.5, # rate of decrease of RUE during reproduction
                                     rmin = 0.02, # minimum RUE during reproduction end/senescence
                                     a = 0.015, # final RUE
                                     b = 4.5 # rate of decrease of RUE during senescence,
                                     # TODO abiotic stresses
                                     ) {
  # Compute potential RUE based on thermal time
  potential_rue <- if (thermal_time < 300) {
    r0
  } else if (thermal_time < thermal_time_flowering) {
    r0 + 2 * (thermal_time - 300) / (thermal_time_flowering - 300)
  } else if (thermal_time < thermal_time_maturity) {
    rmax
  } else if (thermal_time < thermal_time_senescence) {
    a * exp(b * (1 - (thermal_time - thermal_time_maturity) / (thermal_time_senescence - thermal_time_maturity)))
  } else {
    0
  }
  
  # TODO check on this
  # Adjust RUE based on abiotic stresses
  rue <- potential_rue * thermal_stress_rue * water_stress_rue * nitrogen_stress_rue
  
  return(rue)
}



# Soil Parameters -------------------------------------------------------------
# Soil described by 2 layers (0-30 cm, 30 cm - root depth)
# summarised by water capacity (mm) & mineralization rate
# measured by standard soil analysis or estimated using a soil database (e.g. European Soil
# Database, ESDB) (Hiederer, 2013).

# Soil parameters
soil_parameters <- list(
  RootDepthLimit = 1000.0, # Maximum soil rooting depth (mm)
  SoilWaterCapacity_0_30 = 19.7, # Gravimetric water content at field capacity (0–30 cm) (%)
  SoilWaterCapacity_Wilting_0_30 = 9.7, # Gravimetric water content at wilting point (0–30 cm) (%)
  SoilWaterCapacity_30_Root = 19.7, # Gravimetric water content at field capacity (30 cm–root depth) (%)
  SoilWaterCapacity_Wilting_30_Root = 9.7, # Gravimetric water content at wilting point (30 cm–root depth) (%)
  SoilDensity_0_30 = 1.3, # Soil bulk density, sieved < 5 mm (0–30 cm) (g/cm³)
  SoilDensity_30_Root = 1.3, # Soil bulk density, sieved < 5 mm (30 cm–root depth) (g/cm³)
  StoneContent = 0.1, # Stone content (0–root depth) ([0, 1])
  PotentialMineralizationRate = 0.5 # Potential nitrogen mineralization rate (kg/ha/day)
)
# Cultivar Parameters ---------------------------------------------------------
# Genotype-dependent parameters obtained by measuring phenotypic traits
# in field (Casadebaig et al., 2016) & controlled conditions (Casadebaig et al., 2008)

cultivar_parameters <- list(
  ThermalTimeVegetative = 482.00, # Temperature sum to floral initiation (C.d)
  ThermalTimeFlowering = 836.00, # Temperature sum from emergence to the beginning of flowering (C.d)
  ThermalTimeSenescence = 1083.00, # Temperature sum from emergence to the beginning of grain filling (C.d)
  ThermalTimeMaturity = 1673.00, # Temperature sum from emergence to seed physiological maturity (C.d)
  PotentialLeafNumber = 29.00, # Potential number of leaves at flowering (leaf)
  PotentialLeafProfile = 17.00, # Potential rank of the plant's largest leaf at flowering (leaf)
  PotentialLeafSize = 448.00, # Potential area of the plant's largest leaf at flowering (cm²)
  ExtinctionCoefficient = 0.88, # Light extinction coefficient during vegetative growth (-)
  WaterResponseExpansion = -4.42, # Threshold for leaf expansion response to water stress (-)
  WaterResponseConductance = -9.30, # Threshold for stomatal conductance response to water stress (-)
  PotentialHarvestIndex = 0.40, # Potential harvest index (-)
  PotentialOilContent = 55.40 # Potential seed oil content (% dry)
)

# Phenology Parameters --------------------------------------------------------
# Parameters related to development & growth process of crop

phenology_parameters <- list(
  ThermalTimeVegetative = 482.0, # Temperature sum to floral initiation (C.d)
  ThermalTimeFlowering = 836.0, # Temperature sum from emergence to the beginning of flowering (C.d)
  ThermalTimeSenescence = 1083.0, # Temperature sum from emergence to the beginning of grain filling (C.d)
  ThermalTimeMaturity = 1673.0, # Temperature sum from emergence to seed physiological maturity (C.d)
  SowingDepth = 30.0, # Sowing depth (mm)
  TemperatureBase = 4.8, # Base temperature for development and growth process (C)
  Germination = 86.2, # Temperature sum from sowing to germination (C.d)
  ElongationRate = 1.2 # Reciprocal of hypocotyl elongation rate (C.d/mm)
)

# Emergence Function ----------------------------------------------------------
# Computes emergence time based on germination, elongation rate, & sowing depth
# Germination= 86, Thermal time for germination (°C.d);
# ElongationRate= 1.19, Hypocotyl elongation rate (°Cd mm-1)
# SowingDepth= 30, Default sowing depth (mm)
emergence_time <- function(germination = 86.2, 
                          elongation_rate = 1.19, 
                          sowing_depth = 30) {
  return(germination + (elongation_rate * sowing_depth))
}

# Thermal Time Function -------------------------------------------------------
# Computes thermal time based on base temperature, mean air temperature,
# & water stress

thermal_time <- function(mean_temperature, t, base_temperature = 4.8, water_stress_phenology = 0) {
  # vector of daily mean air temperature (°C)
  # If mean temperature is greater than base temperature, calculate thermal time
  if (mean_temperature[t] > base_temperature) {
    return(sum((mean_temperature[t] - base_temperature) * (1 + water_stress_phenology)))
  } else {
    # Otherwise, thermal time is 0 (no stress)
    return(0)
  }
}

# Leaf Area Parameters --------------------------------------------------------
# Parameters related to leaf area development and senescence

leaf_area_parameters <- list(
  PotentialLeafNumber = 29.0, # Potential number of leaves at flowering (leaf)
  PotentialLeafProfile = 17.0, # Potential rank of the plant's largest leaf at flowering (leaf)
  PotentialLeafSize = 448.0, # Potential area of the plant's largest leaf at flowering (cm²)
  Phyllotherm_1 = 71.4, # Phyllotherm for leaves <= 6 (C.d)
  Phyllotherm_7 = 16.3, # Phyllotherm for leaves > 6 (C.d)
  PotentialLeafDurationMin = 153.0, # Asymptote of leaf longevity function, base leaf duration (C.d)
  PotentialLeafDurationMax = 851.3, # Maximum thermal time between expansion and senescence (C.d)
  PotentialLeafDurationWidth = 0.8, # Width of leaf longevity function (leaf)
  PotentialGrowthSlope = 0.0 # Rate of leaf growth and senescence processes (-)
)


# Jenna note: commenting these out for now to use definitions from other files
# 
# # Leaf Initiation Time --------------------------------------------------------
# # Computes the thermal time for leaf initiation based on leaf rank
# # TODO check is i = leaf_rank
# leaf_initiation_time <- function(leaf_rank, phyllotherm_1 = 71.4, phyllotherm_7 = 16.3, potential_leaf_number = 29.0) {
#   if (leaf_rank <= 6) {
#     return(leaf_rank * phyllotherm_1)
#   } else if (leaf_rank <= potential_leaf_number) {
#     return((leaf_rank - 5) * phyllotherm_7 + 6 * phyllotherm_1)
#   } else { # TODO is this needed?
#     stop("Leaf rank exceeds potential leaf number.")
#   }
# }
# 
# # Leaf Expansion Time ---------------------------------------------------------
# # Computes  thermal time for leaf expansion based on leaf initiation time
# leaf_expansion_time <- function(leaf_initiation_time, a = 0.01379) {
#   return(leaf_initiation_time + (1 / a))
# }
# 
# # TODO Leaf Senescence Time --------------------------------------------------------
# # Computes the thermal time for leaf senescence based on leaf expansion time
# leaf_senescence_time <- function(leaf_expansion_time, 
#                                   potential_leaf_duration_min = 153.0, 
#                                  potential_leaf_duration_max = 851.3, 
#                                  potential_leaf_duration_width = 0.8) {
#   return(leaf_expansion_time + potential_leaf_duration_min + 
#            (potential_leaf_duration_max - potential_leaf_duration_min) / (1 + exp(-potential_leaf_duration_width)))
# }


# ThermalStressRUE ------------------------------------------------------------
# impact of temperature on photosynthesis is modeled with a piecewise linear
# function, with four thresholds defined below (Villalobos et al., 1996)
# input: mean temp for day, t
# output: single value for thermal stress on RUE

# assuming base_temperature = 4.8, optimal_lower_temperature = 20,
# optimal_upper_temperature = 28, critical_temperature = 37


thermal_stress_rue <- function(daily_mean_temperature) {
  stress_rue <- ifelse(
    daily_mean_temperature > 4.8 & daily_mean_temperature <= 20,
    daily_mean_temperature * (1 / (20 - 4.8)) - (4.8 / (20 - 4.8)),
    ifelse(
      daily_mean_temperature > 20 & daily_mean_temperature <= 28,
      1,
      ifelse(
        daily_mean_temperature > 28 & daily_mean_temperature <= 37,
        daily_mean_temperature * (1 / (28 - 37)) - (37 / (28 - 37)),
        0
      )
    )
  )
  return(stress_rue)
}

# ThermalStressMineralization -------------------------------------------------
# logistic function to describe effect of air temperature on net nitrogen
# mineralization (Valé, 2006; Valé et al., 2007)
# base_temperature <- 15 # Tb
# critical_temperature <- 36 # Tc

thermal_stress_mineralization <- function(daily_mean_temperature) {
  # Logistic function for thermal stress on mineralization
  stress_mineralization <- 36 / 
    (1 + (36 - 1) * exp(-0.119 * (daily_mean_temperature - 15)))
  return(stress_mineralization)
}



# 
