#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' 
#' two populations: wild and domesticated
#' time step: 1 year
#' life cycle: growth/production, reproduction, mortality, selection
#' 3 parameters of interest:
    #' temperature
    #' percipitation
    #' pest pressure
#' wild population
    #' higher secondary metabolite expression
    #' growth/production function of base growth rate (lower than domesticated population),
    #' climate, & secondary metabolite expression (higher in wild population & decreases with domestication)
    #' mortality function of base mortality rate (linear relationship, dealth by 3-5 years), climate (high temperature & low percipitation),
    #' herbivory with high pest pressure (but lower than domesticated population),
    #' reproduction function of base reproduction rate (lower than domesticated population), 
    #' less with high pest pressure (so secondary metabolite expression higher),
    #' selection function of human selection on higher yeild production & natural selction
#' domesticated population
    #' lower secondary metabolite expression
    #' life cycle in one year
  #' growth/production function of base growth rate (higher than wild population) & climate (no extreme temperature & percipitation),
  #' reproduction function of base reproduction rate (higher than wild population), 
  #' mortality function of climate (high temperature & low percipitation), pest pressure (higher mortality rate with pest pressure than wild population),
  #' selection function of human selection on higher yeild production
  #' all indivduals (death every life cycle)
#' trait: secondary metabolite expression
#' when secondary metabolite expression high & high pest pressure then mortality low, but growth lowered
#' when secondary metabolite expression low & high pest pressure then mortality high, growth remain
#' when secondary metabolite expression high & low pest pressure then mortality low, growth remain
#' when secondary metabolite expression low & low pest pressure then mortality high, growth lowered

#' Human selection
#' linear relationship between growth and yeild
#' 
#' top 20% <-- soft selection
#' threshold <-- hard selection 
#' selection on indivduals 
#' selection for high yield in domesticated population

# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(ggplot2) # plotting
library(parallel)  # parallelization

# Set seed for reproducibility
set.seed(123)

# Simulation parameters
years <- 2 # Number of years to simulate
# Initial populations sizes
initial_wild <- 100
initial_domesticated <- 100

# Environmental parameters ( TODO add real data)
temperature <- runif(years, 15, 35)         # degrees Celsius

precipitation <- runif(years, 200, 800)     # mm/year


# Pest pressure (0 to 1, where 0 is no pest pressure and 1 is high pest pressure) 
# pest_pressure <- runif(years, 0.1, 1.0)     # 0 (none) to 1 (high)


# pest (Aphid) population directly influenced by temperature, with warmer temperatures generally leading to faster development, reproduction, and increased survival rates.
# pest_pressure_function <- function(temp) {
#   if (temp < 20) {
#     return(runif(1, 0.1, 0.3))  # Low pest pressure in cooler temperatures
#   } else if (temp < 30) {
#     return(runif(1, 0.3, 0.7))  # Moderate pest pressure in moderate temperatures
#   } else {
#     return(runif(1, 0.7, 1.0))  # High pest pressure in warmer temperatures
#   }
# }


#' Estimate aphid pest pressure based on temperature
#'
#' @param temp Numeric vector of daily temperatures (°C)
#' @param Tmin Minimum temperature threshold for activity
#' @param Tmax Maximum temperature threshold for activity
#' @param a Brière model coefficient
#' @return A vector of pest pressure values (scaled 0 to 1)

# The Brière model is a nonlinear temperature-dependent growth model that describes how the development rate of an ectothermic organism (like an insect) changes with temperature. It’s widely used in entomology and ecology because it captures the asymmetric, bell-shaped curve of biological performance versus temperature.


aphid_pest_pressure <- function(temp, Tmin = 5, Tmax = 35, a = 0.0002) {
  # Brière-based growth rate
  growth_rate <- ifelse(
    temp > Tmin & temp < Tmax,
    a * temp * (temp - Tmin) * sqrt(Tmax - temp),
    0
  )
  # Normalize to 0–1 scale for pest pressure
  pressure <- growth_rate / max(growth_rate, na.rm = TRUE)
  return(pressure)
}
test_temps <- seq(0, 40, by = 1)
pest_pressure <- aphid_pest_pressure(temps)

plot(day, pressure, type = "l", col = "red", lwd = 2,
     xlab = "Temperature (°C)", ylab = "Pest Pressure (0–1)",
     main = "Temperature-Driven Aphid Pest Pressure")











# Trait parameters ------------------------------------------------------------
# TODO add real data on gene expression HERE
# secondary_metabolite_wild <- 0.9
secondary_metabolite_wild = rnorm(initial_wild, mean = 0.7, sd = 0.3)  # Gaussian distribution
# Ensure trait value within [0, 1] for wild population
secondary_metabolite_wild <- pmin(pmax(secondary_metabolite_wild, 0), 1)


# secondary_metabolite_domesticated <- 0.3
secondary_metabolite_domesticated = rnorm(initial_domesticated, mean = 0.3, sd = 0.02)  # Narrow Gaussian distribution
# Ensure trait value within [0, 1] for domesticated population
secondary_metabolite_domesticated <- pmin(pmax(secondary_metabolite_domesticated, 0), 1)


# Base rates
growth_base_wild <- 1.1
growth_base_domesticated <- 1.4
repro_base_wild <- 2
repro_base_domesticated <- 4
mortality_base <- 0.3

# Storage
pop_wild <- numeric(years)
pop_domesticated <- numeric(years)
pop_wild[1] <- initial_wild
pop_domesticated[1] <- initial_domesticated



# Helper functions


mortality_function <- function(temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
  climate_stress <- (temp > 30) + (precip < 300)
  herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
  mortality_base + 0.1 * climate_stress + herbivory
}

reproduction_function <- function(base_repro, pest_pressure, secondary_metabolite) {
  stress_factor <- 1 - pest_pressure * (1 - secondary_metabolite)
  max(base_repro * stress_factor, 0)
}

# yeild function

# selection -----------------------------------------
selection_function <- function(pop, yield, is_domesticated = FALSE) {
  if (is_domesticated) {
    pop * (1 + 0.05 * yield)  # selection for high yield
  } else {
    pop * (1 + 0.02 * yield)  # weak natural selection on productivity
  }
}

# Parameters
set.seed(123)
n_individuals <- 100        # Population size
n_generations <- 20         # Number of generations
selection_proportion <- 0.2 # Top % selected
heritability <- 0.5        # Trait heritability (H^2)
trait_sd <- 1               # Standard deviation of trait

# Initialize population with random trait values
population <- rnorm(n_individuals, mean = 0, sd = trait_sd)
mean_trait <- numeric(n_generations)

# Simulate over generations
for (gen in 1:n_generations) {
  mean_trait[gen] <- mean(population)
  
  # Select top individuals based on trait value
  n_selected <- ceiling(n_individuals * selection_proportion)
  parents <- sort(population, decreasing = TRUE)[1:n_selected]
  
  # Offspring inherit average of selected trait + some random variation
  population <- rnorm(n_individuals, mean = mean(parents), sd = trait_sd)
}


# Simulation loop
for (gen in 1:n_generations) {
  # Record mean trait value
  mean_trait[gen] <- mean(population)
  
  # Select top individuals
  n_selected <- ceiling(n_individuals * selection_proportion)
  selected <- sort(population, decreasing = TRUE)[1:n_selected]
  
  # Generate next generation
  genetic_component <- rnorm(n_individuals, mean = mean(selected), sd = 0)
  environmental_component <- rnorm(n_individuals, mean = 0, sd = sqrt((1 - heritability) * trait_sd^2))
  population <- genetic_component * sqrt(heritability) + environmental_component
}

# Plot results
plot(1:n_generations, mean_trait, type = "b", pch = 19,
     xlab = "Generation", ylab = "Mean Trait Value",
     main = "Simulated Human Selection Over Generations")


# patch of plants 7.0 plants per m^2 for a field plot of 5 acres
# 5 acres = 20234.3 m^2
plants_per_acre <- 7.0 * 20234.3  # plants per acre
# new_wild_population_size <- plants_per_acre - wild_survived

# TODO think about the data structure for poolled trait of surviving wild populations
wild_trait_distibution_per_plot <- function(surviving_wild_populations, plants_per_acre) { # TODO add pest_pressure
  rnorm((plants_per_acre - surviving_wild_populations), # TODO some measure of n surviving in plot
  mean = mean(surviving_wild_populations),
  sd = sd(surviving_wild_populations))
}

# base_growth is above ground biomass production
wild_growth_function <- function(base_growth, temp, precip, secondary_metabolite) {
  climate_factor <- exp(-((temp - 25)^2)/50) * (precip / 400)
  base_growth * climate_factor * (1 + 0.5 * secondary_metabolite)
}

wild_growth_function <- function(base_growth, temp, precip, secondary_metabolite) {
  climate_stress <- (temp > 30) + (precip < 300)
  growth_base <- base_growth * (1 - climate_stress * 0.1)
  growth_base * (1 - secondary_metabolite)  # Higher secondary metabolite reduces growth
}

# plot(wild_growth_function(growth_base_wild, temperature, precipitation, secondary_metabolite_wild))

plot(wild_growth_function(growth_base_wild, temperature[t], precipitation[t], secondary_metabolite_wild))
plot(wild_growth_function(growth_base_wild, temperature[t], precipitation[t], secondary_metabolite_wild))

# Simulation ---------------------------------------------------
# for each year, loop through growing season (per day) for each plot of population


for (t in 2:years) {
  # pest
  # Wild
  # growth_wild <- growth_function(growth_base_wild, temperature[t], precipitation[t], secondary_metabolite_wild)
  growth_wild <- wild_growth_function(growth_base_wild, temperature[t], precipitation[t], secondary_metabolite_wild)
  mortality_wild <- mortality_function(temperature[t], precipitation[t], pest_pressure[t], secondary_metabolite_wild)
  survivors_wild <- pop_wild[t - 1] * (1 - mortality_wild)
  offspring_wild <- survivors_wild * reproduction_function(repro_base_wild, pest_pressure[t], secondary_metabolite_wild)
  selected_wild <- selection_function(offspring_wild, growth_wild, FALSE)
  pop_wild[t] <- selected_wild

  # Domesticated
  growth_dom <- growth_function(growth_base_domesticated, temperature[t], precipitation[t], secondary_metabolite_domesticated)
  mortality_dom <- mortality_function(temperature[t], precipitation[t], pest_pressure[t], secondary_metabolite_domesticated, TRUE)
  survivors_dom <- pop_domesticated[t - 1] * (1 - mortality_dom)
  offspring_dom <- survivors_dom * reproduction_function(repro_base_domesticated, pest_pressure[t], secondary_metabolite_domesticated)
  selected_dom <- selection_function(offspring_dom, growth_dom, TRUE)
  pop_domesticated[t] <- selected_dom
}

# Plot results
plot(1:years, pop_wild, type = "l", col = "forestgreen", ylim = range(c(pop_wild, pop_domesticated)), 
     ylab = "Population Size", xlab = "Year", lwd = 2, main = "Sunflower Population Dynamics")
lines(1:years, pop_domesticated, col = "goldenrod", lwd = 2)
legend("topright", legend = c("Wild", "Domesticated"), col = c("forestgreen", "goldenrod"), lty = 1, lwd = 2)
