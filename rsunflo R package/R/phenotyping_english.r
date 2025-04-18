# Tools for phenotyping and genotypic parameterization

# Climate ####
# 1 watt/day = 86400 joules

# Compute reference evapotranspiration using the Penman-Monteith formula and assumptions from [@Wallach2006]
#' @export et_penman_monteith
et_penman_monteith <- function(tmin, tmax, tdew, rad, wind, lat, lon, elevation, day, ...) {
  
  # Inputs:
  # RAD      Daily Insolation Incident On A Horizontal Surface (MJ/m^2/day) 
  # TMIN     Minimum Air Temperature At 2 m Above The Surface Of The Earth (degrees C) 
  # TMAX     Maximum Air Temperature At 2 m Above The Surface Of The Earth (degrees C) 
  # TDEW     Dew/Frost Point Temperature At 2 m (degrees C) 
  # WIND     Wind Speed At 10 m Above The Surface Of The Earth (m/s)
  # DAY      Day of year
  
  # Output:
  # ET       Daily Reference Evapotranspiration (mm/day)
  
  latitude <- unique(lat) * pi / 180
  
  # Psychrometric constant
  PSC <- 0.665 * 10^-3 * 101.3 * ((293 - 0.0065 * unique(elevation)) / 293)^5.26
  
  # Wind speed at 2m
  ws2 <- wind * 4.87 / log(67.8 * 10 - 5.42)
  es <- ((0.6108 * exp(17.27 * tmax / (tmax + 237.3))) + (0.6108 * exp(17.27 * tmin / (tmin + 237.3)))) / 2
  slope <- (0.6108 * exp(17.27 * ((tmax + tmin) / 2) / (((tmax + tmin) / 2) + 237.3)) * 4098) / ((tmax + tmin) / 2 + 237.3)^2
  
  # Humidity
  ea <- 0.6108 * exp(17.27 * tdew / (tdew + 237.3))
  
  # Radiation
  SWR <- (1 - 0.23) * rad
  IRDES <- 1 + 0.033 * cos(2 * pi * day / 365.25)
  SD <- 0.409 * sin(2 * pi * day / 365.25 - 1.39)
  SSA <- acos(-tan(latitude) * tan(SD))
  extra <- 24 * 60 * 0.082 / pi * IRDES * (SSA * sin(latitude) * sin(SD) + cos(latitude) * cos(SD) * sin(SSA))
  CSR <- (0.75 + 2 * 10^-5 * unique(elevation)) * extra
  RRAD <- rad / CSR
  
  # Evapotranspiration
  LWR <- 4.903 * 10^-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * RRAD - 0.35)
  NRAD <- SWR - LWR
  ET <- (0.408 * slope * NRAD + PSC * (900 / ((tmax + tmin) / 2 + 273)) * ws2 * (es - ea)) / (slope + PSC * (1 + 0.34 * ws2))
  
  return(ET)
}

# Soil ####

# Pedotransfer function: Estimate volumetric water retention capacity from soil analysis
# θ = a + (b×%Clay) + (c×%Silt) + (d×%Sand) + (e×BulkDensity)
#' @export soil_water_capacity_type
soil_water_capacity_type <- function(
  Argile, # % Clay
  LimonFin, # % Fine Silt
  LimonGrossier, # % Coarse Silt
  SableFin, # % Fine Sand
  SableGrossier, # % Coarse Sand
  CaCO3, # % Calcium Carbonate
  MatiereOrganique, # % Organic Matter
  Profondeur, # Depth (mm)
  Cailloux # % Gravel
) {
  # [Vale2007]
  (CaCO3 + 2 * Argile + LimonFin + LimonGrossier + 0.7 * (SableFin + SableGrossier)) *
    ((100 - Cailloux) / 100) * (Profondeur / 1000) * (1 + 0.05 * MatiereOrganique - 0.1)
}

# Compute available soil water content (mm), default model inputs
#' @export soil_water_capacity
soil_water_capacity <- function(
  root_depth = 1000, stone_content = 0.1,  # Root depth (mm), % weight
  field_capacity_1 = 19.7, field_capacity_2 = 19.7, # % weight
  wilting_point_1 = 9.7, wilting_point_2 = 9.7, # % weight
  soil_density_1 = 1.3, soil_density_2 = 1.3, # Mass/volume (g/cm3)
  ...
) {
  
  # Available soil water content for surface layer 
  swc_1 <- (field_capacity_1 / 100 - wilting_point_1 / 100) * (1 - stone_content) * soil_density_1 * 300
  # Available soil water content for deep layer 
  swc_2 <- (field_capacity_2 / 100 - wilting_point_2 / 100) * (1 - stone_content) * soil_density_2 * (root_depth - 300)
  # Total available soil water content 
  swc <- swc_1 + swc_2
  
  return(swc)
}

# Phenology ####

# Compute temperature sum between two dates
#' @export thermal_time
thermal_time <- function(climate, id, start, end, base = 4.8, ...) {
  if (is.na(start) | is.na(end)) {
    return(NA)
  } else {
    # Select vector for mean temperature
    temperature <- table_climate %>% filter(trial_id == id) %>% slice(start:end) %>% .$TM
    # Conditional temperature sum
    thermal_time <- sum(ifelse(temperature - base < 0, 0, temperature - base))
    return(thermal_time)
  }
}

# Compute secondary phenological stages from flowering date
#' @export phenostage
phenostage <- function(flowering) {
  r <- data.frame(
    TDF1 = flowering, # Flowering date
    TDE1 = 0.576 * flowering, # End of flowering
    TDM0 = flowering + 246.5 # Maturity date
  )
  return(r)
}

# Architecture ####

# Leaf surface model = f(Length, Width) cm
#' @export leaf_size
leaf_size <- function(length, width, a0 = 0.7, a = 0.736, b = -8.86, c = 0.684, shape = "simple", ...) {
  
  switch(
    shape,
    
    # Default to simple linear model, c.f Heliaphen_platform repository
    simple = {
      area <- a0 * length * width
    },
    
    bilinear = {
      area <- ifelse(length * width < (b / (c - a)),
                     c * length * width,
                     a * length * width + b
      )
    }
  )
  return(area)
}

# Leaf profile model
#' @export leaf_profile
leaf_profile <- function(TLN, LLS, LLH, a = -2.05, b = 0.049, shape = "fixed", output = "profile", ...) {
  
  # Number of phytomers
  n <- 1:TLN
  
  # Compute leaf profile using two methods for shape coefficients
  switch(
    shape,
    fixed = {
      # a = -2.110168, b = 0.01447336 [Casadebaig2013]: average of linear model on the DB
      # a = -2.049676, b = 0.04937692 [Casadebaig2013]: average adjustments on observed DB
      r <- LLS * exp(a * ((n - LLH) / (LLH - 1))^2 + b * ((n - LLH) / (LLH - 1))^3)    
    },
    model = {
      b <- 1.5 - 0.2210304 * LLH - 0.0003529 * LLS + 0.0825307 * length(n)
      a <- -2.313409 + 0.018158 * LLH - 0.001637 * LLS + 0.019968 * length(n) + 0.920874 * b          
      r <- LLS * exp(a * ((n - LLH) / (LLH - 1))^2 + b * ((n - LLH) / (LLH - 1))^3)    
    }
  )
  
  # Output
  switch(
    output,
    profile = return(data.frame(leaf = n, size = r)),
    shape = return(data.frame(a = a, b = b))
  )
}

# Model extinction coefficient as a function of architectural traits
# TLN : Total leaf number (leaf rank)
# LLH : Largest leaf height (leaf rank)
# LLS : Largest leaf size (cm^2)
# H   : Plant height (cm)
#' @export coefficient_extinction
coefficient_extinction <- function(TLN, LLH, LLS, H, ...) {  
  # Pouzet-Bugat method [@Pouzet1985]
  TPA <- 0.5 * TLN * LLS + 30 * TLN  
  
  # Fitted in [@Casadebaig2008]
  K <- -1.11E-2 * LLH - 1.09E-2 * TLN - 1.12E-3 * LLS - 0.11E-2 * H + 6.5E-5 * TPA + 1.58
  return(K)
}

# Allocation ####

# Convert oil content expressed at norms of humidity (9%) and impurity (2%) to a dry matter basis
#' @export convert_oilcontent
convert_oilcontent <- function(x, humidity = 9, impurity = 2) {
  r <- (1 - impurity / 100) * (1 - humidity / 100)
  return(x * 1 / r)
}

# Convert grain content expressed at norms of humidity (9%) and impurity (2%) to a dry matter basis
#' @export convert_grainyield
convert_grainyield <- function(x, humidity = 9, impurity = 2) {
  r <- (1 - impurity / 100) * (1 - humidity / 100)
  return(x * r)
}

# Response ####

# Response of plant transpiration/stomatal conductance to water stress
#' @export curve_conductance
curve_conductance <- function(x, a) {
   t = 1.05 / (1 + 4.5 * exp(a * x))
   return(t)
}

# Response of expansion to water stress
#' @export curve_expansion
curve_expansion <- function(x, a) {
  t = (2 / (1 + exp(a * x))) - 1
  return(t)
}

# Compute quantitative low and high temperature stress index on RUE
#' @export curve_thermal_rue
curve_thermal_rue <- function(tm, tb = 4.8, tol = 20, tou = 28, tc = 37, type = "low") {
  
  switch(
    type, 
    
    low = {
      ifelse(tm > tb & tm < tol, tm * (1 / (tol - tb)) - (tb / (tol - tb)),
             ifelse(tm <= tb, 0, 1)
      )
    },
    
    high = {
      ifelse(tm > tou & tm < tc, tm * (1 / (tou - tc)) - (tc / (tou - tc)),
             ifelse(tm >= tc, 0, 1)
      )
    }
  )
}

# Break linear model
#' @export curve_breaklinear
curve_breaklinear <- function(x, a, b) {
  t = NULL
  for (i in 1:length(x)) {
    if (x[i] < a) y = (1 - b) / a * x[i] + b else y = 1
    t = c(t, y)
  }
  return(t)        
}