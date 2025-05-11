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
# grow <- function(population) {
#   population[, size := size + growth_rate * 0.1]
#   return(population)
# }
# 
# 
# # Updated grow function based on trait and pest pressure
# grow <- function(population, pest_pressure) {
#   population[, growth_rate := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.05, # TODO what these values should be?
#                                fifelse(trait < 0.5 & pest_pressure < 0.5, 0.05,
#                                0.1))]
#   population[, size := size + growth_rate]
#   return(population)
# }

# mortality function ---------------------------------------------------------

# Mortality due to herbivory
herbivory_mortality <- function(pest_pressure, resistance, gamma = 2) {
  mortality <- pest_pressure * (1 - resistance)^gamma
  return(mortality)
}




# plot(rep(1:10),exp(rep(10:1)))
# plot(rep(1:10),(rep(1:10))^(-1/2))
# plot(rep(1:10),(rep(1:10))*(-1/3/150))
# 
# domesticate_mortaility_rate <- function(number_of_days, starting_number){
#   return(starting_number + (rep(1:number_of_days))*(-(3)/number_of_days))
# }
# plot(rep(1:150),dom_rate(150,10),ylim = c(0,10))
# 
# mortality_base <- function(is_domesticated = FALSE)
# 
# suffer_mortality <- function(population, pest_pressure) {
#   mortality_prob <- pest_pressure * (1 - population$resistance) * 0.1
#   survivors <- population[runif(.N) >= mortality_prob]
#   return(survivors)
# }
# 
# # additive mortality
# mortality = intrinsitic_rate +(pest_pressure * suscepibility)
# 
# # multiplicative mortality 
# survival_rate = (1 - intrinsic_rate) * (1 - pest_pressure * susceptibility)
# total_mortality = 1 - survival_rate
# 
# 
# # Updated mortality function based on trait and pest pressure
# suffer_mortality <- function(population, pest_pressure) {
#   population[, mortality_prob := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.01,
#                                          fifelse(trait < 0.5 & pest_pressure >= 0.5, 0.2,
#                                                  fifelse(trait >= 0.5 & pest_pressure < 0.5, 0.01,
#                                                          0.15)))]
#   survivors <- population[runif(.N) >= mortality_prob]
#   return(survivors)
# }
# 
# mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
#   climate_stress <- (max_temp/10 > 30) + (precip < 300)
#   herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
#   mortality_base + 0.1 * climate_stress + herbivory
# }
# 
# 
# 

# Reproduction function -------------------------------------------------------
# reproduce <- function(population) {
#   # Calculate number of offspring for each individual plant
#   reproduction_prob <- population$reproduction_rate * 0.05
#   num_offspring <- floor(reproduction_prob) # Ensure integer
#   
#   # Create new plants based on reproduction probability
#   new_plants <- population[rep(1:.N, num_offspring)]
#   
#   if (nrow(new_plants) > 0) {
#     new_plants[, size := 1]
#     new_plants[, growth_rate := 1 - trait]
#     new_plants[, reproduction_rate := 1 - trait]
#     new_plants[, resistance := trait]
#     population <- rbind(population, new_plants)
#   }
#   
#   return(population)
# }
# 
