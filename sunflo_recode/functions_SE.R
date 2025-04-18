# Sarah Elizabeth's Functions

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

# CropBiomass_t = CropBiomass_t−1 + (PAR_t  ×RIE_t × RUE_t)
crop_biomass <- function(PAR, RIE, RUE, previous_biomass) {
  return(previous_biomass + (PAR * RIE * RUE))
}

# Photosynthetically active radiation -----------------------------------------
# (PAR) is 48% of total radiation (Monteith, 1977) <-- citation unsure about
# PAR_t = Radiation_t  × 0.48

PAR <- function(radiation) {
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





















# Thermal time accumulation
thermal_time <- function(Tm, Tb) {
  if (Tm > Tb) {
    return(Tm - Tb)
  } else {
    return(0)
  }
}

# Radiation Interception Efficiency (RIE)
radiation_interception <- function(LAI, k) {
  return(1 - exp(-k * LAI))
}

# Radiation Use Efficiency (RUE)
radiation_use_efficiency <- function(thermal_time, TDF1, TDM0, TDM3, r0, rmax, rd, rmin, a, b) {
  if (thermal_time < 300) {
    return(r0)
  } else if (thermal_time < TDF1) {
    return(r0 + 2 * (thermal_time - 300) / (TDF1 - 300))
  } else if (thermal_time < TDM0) {
    return(rmax)
  } else if (thermal_time < TDM3) {
    return(b * (1 - a * exp((thermal_time - TDM3) / (TDM3 - TDM0))))
  } else {
    return(0)
  }
}







# Water Stress
water_stress <- function(FTSW, a) {
  return(-1 + 1 / (1 + exp(a * FTSW)))
}

# Leaf Area Index (LAI)
leaf_area_index <- function(sowing_density, plant_leaf_area) {
  return(sowing_density * plant_leaf_area)
}

# Root Growth
root_growth <- function(Tm, RootGrowthRate, RootDepthLimit, current_root_depth) {
  new_root_depth <- current_root_depth + RootGrowthRate * Tm
  return(min(new_root_depth, RootDepthLimit))
}
