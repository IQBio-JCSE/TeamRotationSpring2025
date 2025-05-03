#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' v3
#' 
# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(ggplot2) # plotting
library(parallel)  # parallelization

# Load custom functions --------------------------------------------------------
# source("functions.R") # Load helper functions

# Set seed for reproducibility -----------------------------------------------
set.seed(123)

# Define parameters -----------------------------------------------------------
years <- 2 # Number of years to simulate

# number of plots of populations
plots <- 10

# initial populations of wild and domesticated plants
initial_wild <- 10
initial_domesticated <- 10

# number of plants per plot

# selection proportion
selection_proportion <- 0.2
n_selected <- ceiling(plots * selection_proportion)

# data storage ----------------------------------------------------------------
# data structure for storing yield and trait expression
wild_population <- data.table(
  year = integer(),
  plot = integer(),
  n_survived = integer(),
  trait_mean = numeric(),
   trait_sd = numeric(),
  yield = numeric()
)
domesticated_population <- data.table(
  year = integer(),
  plot = integer(),
  n_survived = integer(),
  trait_mean = numeric(),
   trait_sd = numeric(),
  yield = numeric()
)
pest_pressure_data <- data.table(
  year = integer(),
  day = integer(),
  pressure = numeric()
)

# climate data ----------------------------------------------------------------
climate_data <- data.table(
  # study_period = 'A"
  year = rep(1:years, each = 150),
  day = rep(1:150, times = years),
  temperature = rnorm(years * 150, mean = 20, sd = 5), # Random temperature data
  rainfall = rnorm(years * 150, mean = 100, sd = 20) # Random rainfall data
)

# plot temperature & precipitation across    
plot(climate_data$day[climate_data$year==2], climate_data$temperature[climate_data$year==2], type = "l", col = "#d09220", lwd = 2,
     xlab = "Days", ylab = "Temperature (Cº)",
     main = "Temperature Across Study Period")
# TODO: add precip & add all study years

# plot precipitation across    
plot(climate_data$day[climate_data$year==2], climate_data$rainfall[climate_data$year==2], type = "l", col = "darkblue", lwd = 2,
     xlab = "Days", ylab = "Precipitation (mm)",
     main = "Precipitation Across Study Period")
# TODO: accumulation




# pest pressure ---------------------------------------------------------------
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


# pest pressure loop
for (year in 1:years) {
#   initialize population
    # trait_expression

    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure<- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, day = day, pressure = pest_pressure))
    }
    # # plot pest pressure over time
plot(pest_pressure_data$day[pest_pressure_data$year==2], pest_pressure_data$pressure[pest_pressure_data$year==2], type = "l", col = "#d09220", lwd = 2,
     xlab = "Days", ylab = "Pest Pressure (0–1)",
     main = "Aphid Pest Pressure Across Study Period")

}




# initialize populations




for (year in 1:years) {
#   initialize population

  for (plot in 1:plots) { # for each plot population
    
    # trait_expression

    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure <- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    # pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, day = day, pressure = pest_pressure))
    
    
    # growth: base growth rate, temperture,

    # death related to intrinisic propbability of death 

    }
    # plot yield

  }
  # Select top plots based on yield value
  selected_plots <- sort(plot_yield, decreasing = TRUE)[1:n_selected]

  # new plots inherit average of selected plots trait + some random variation
  wild_new_population <- rnorm(wild_n_individuals, 
                               mean = mean(wild_selected_plots_trait), 
                               sd = sd(wild_selected_plots_trait) * 0.1)
  wild_new_population <- pmax(0, pmin(1, wild_new_population)) # Ensure values within [0, 1]
  domesticated_new_population <- rnorm(domesticated_n_individuals, 
                                       mean = mean(domesticated_selected_plots_trait), 
                                       sd = sd(domesticated_selected_plots_trait) * 0.1)
  domesticated_new_population <- pmax(0, pmin(1, domesticated_new_population)) # Ensure values within [0, 1]
}

# Plot ------------------------------------------------------------------------


