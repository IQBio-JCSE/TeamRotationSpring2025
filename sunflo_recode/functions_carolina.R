# Carolina's functions <- function() {}
calc_N_deficit <- function(NNI) {
  return(sum(1-NNI))
}

calc_edaphic_water_deficit <- function(ET, PET) {
  division <- ET/PET
  below <- division < 0.6
  return(sum(division[below]))
}

calc_photo_act_rad <- function(GR) {
  return(sum(GR*0.48))
}

calc_LAD <- function(LAI) {
  return(sum(LAI))
}

calc_NNNID <- function(NNI) {
  return(sum(NNI < 0.8))
}

calc_pot_evapot <- function(PET) {
  return(sum(PET))
}

calc_int_rad <- function(RIE, GR) {
  return(sum(RIE*GR*0.48))
}

calc_climate_water_deficit <- function(RR, PET) {
  return(sum(RR-PET))
}

calc_rain <- function(RR) {
  return(sum(RR))
}

calc_thermal_time <- function(TM) {
  return(sum(TM-4.8))
}

calc_thermal_stress_low <- function(TM) {
  above <- TM < 20
  return(sum(TM[above]))
}

calc_thermal_stress_high <- function(TM) {
  above <- TM > 28
  return(sum(TM[above]))
}

