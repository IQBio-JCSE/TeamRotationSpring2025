#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' full version
#' 
# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(tidyverse)
library(ggplot2) # plotting
library(parallel)  # parallelization

# Load functions --------------------------------------------------------
source("simulation/Population_Simulation/Functions.r") # Load helper functions

# TODO load sunflo model/function
# source("sunflo_model.R") # Load sunflower model

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


for (year in 1:years) {
#   initialize population

  for (plot in 1:plots) { # for each plot population
    
    # trait_expression <-- need ONE value for sunflo



    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure <- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    # pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, day = day, pressure = pest_pressure))
    
    
    # growth: base growth rate, temperture, effect of investment in trait expression
    # growth rate: SUNFLO
    # 

    # death related to intrinisic propbability of death 


    }
    # plot yield <-- TODO make sure sunflo here
    # store yield and trait expression
    wild_yield <- runif(1, 0, 100) # Placeholder for yield calculation
    wild_trait_expression <- runif(1, 0, 1) # Placeholder for trait expression calculation
    wild_population <- rbind(wild_population, data.table(
      year = year,
      plot = plot,
      n_survived = initial_wild, # Placeholder for number of survived plants
      trait_mean = wild_trait_expression,
      trait_sd = sd(wild_trait_expression), # Placeholder for standard deviation
      yield = wild_yield
    ))
    domesticated_yield <- runif(1, 0, 100) # Placeholder for yield calculation
    domesticated_trait_expression <- runif(1, 0, 1) # Placeholder for trait expression calculation
    domesticated_population <- rbind(domesticated_population, data.table(
      year = year,
      plot = plot,
      n_survived = initial_domesticated, # Placeholder for number of survived plants
      trait_mean = domesticated_trait_expression,
      trait_sd = sd(domesticated_trait_expression), # Placeholder for standard deviation
      yield = domesticated_yield
    ))

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

