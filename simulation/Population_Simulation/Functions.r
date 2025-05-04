# Functions


# pest pressure ---------------------------------------------------------------
aphid_pest_pressure <- function(temp, # let's make this mean temperature
Tmin = 5, 
Tmax = 35, 
a = 0.0002) {
  # Brière-based growth rate
  aphid_growth_rate <- ifelse(
    temp > Tmin & temp < Tmax,
    a * temp * (temp - Tmin) * sqrt(Tmax - temp),
    0
  )
  # Normalize to 0–1 scale for pest pressure
  pressure <- 1 - (aphid_growth_rate / max(aphid_growth_rate, na.rm = TRUE))
                  return(pressure)
}

# growth function -------------------------------------------------------------
grow <- function(population) {
  population[, size := size + growth_rate * 0.1]
  return(population)
}


# Updated grow function based on trait and pest pressure
grow <- function(population, pest_pressure) {
  population[, growth_rate := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.05, # TODO what these values should be?
                               fifelse(trait < 0.5 & pest_pressure < 0.5, 0.05,
                               0.1))]
  population[, size := size + growth_rate]
  return(population)
}

# mortality function ---------------------------------------------------------
suffer_mortality <- function(population, pest_pressure) {
  mortality_prob <- pest_pressure * (1 - population$resistance) * 0.1
  survivors <- population[runif(.N) >= mortality_prob]
  return(survivors)
}

# Reproduction function -------------------------------------------------------
reproduce <- function(population) {
  # Calculate number of offspring for each individual plant
  reproduction_prob <- population$reproduction_rate * 0.05
  num_offspring <- floor(reproduction_prob) # Ensure integer
  
  # Create new plants based on reproduction probability
  new_plants <- population[rep(1:.N, num_offspring)]
  
  if (nrow(new_plants) > 0) {
    new_plants[, size := 1]
    new_plants[, growth_rate := 1 - trait]
    new_plants[, reproduction_rate := 1 - trait]
    new_plants[, resistance := trait]
    population <- rbind(population, new_plants)
  }
  
  return(population)
}

# distrubution of trait across time