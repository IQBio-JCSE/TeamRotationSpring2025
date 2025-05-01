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
years <- 20 # Number of years to simulate
# Initial populations sizes
initial_wild <- 100
initial_domesticated <- 100

# Environmental parameters ( TODO add real data)
temperature <- runif(years, 15, 35)         # degrees Celsius

precipitation <- runif(years, 200, 800)     # mm/year
# Pest pressure (0 to 1, where 0 is no pest pressure and 1 is high pest pressure) 
# TODO pest pressure relate to climate (high temp --> high pest pressure)
pest_pressure <- runif(years, 0.1, 1.0)     # 0 (none) to 1 (high)

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


selection_function <- function(pop, yield, is_domesticated = FALSE) {
  if (is_domesticated) {
    pop * (1 + 0.05 * yield)  # selection for high yield
  } else {
    pop * (1 + 0.02 * yield)  # weak natural selection on productivity
  }
}

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

# Simulation loop
for (t in 2:years) {
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
