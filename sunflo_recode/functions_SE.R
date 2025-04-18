# Sarah Elizabeth's Functions
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

# Harvest Index
harvest_index <- function(TDF1, TDM0, TDM3, thermal_time) {
  if (thermal_time < TDF1) {
    return(0.2)
  } else if (thermal_time < TDM0) {
    return(0.4)
  } else if (thermal_time < TDM3) {
    return(0.5)
  } else {
    return(0.6)
  }
}


# Crop Biomass Accumulation
crop_biomass <- function(PAR, RIE, RUE, previous_biomass) {
  return(previous_biomass + (PAR * RIE * RUE))
}

# Crop Yeild
crop_yield <- function(crop_biomass, harvest_index) {
  return(biomass * harvest_index)
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
