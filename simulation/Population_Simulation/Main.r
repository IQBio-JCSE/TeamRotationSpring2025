#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' full version
#' 
# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(tidyverse)
# install.packages("segmented")  # Install if not already installed
library(segmented)

# Load functions --------------------------------------------------------
source("simulation/Population_Simulation/Functions.r") # Load helper functions

# Load sunflo model/function
source("sunflo_recode/run_model.R") # Load sunflower model

# test climate data
source("sunflo_recode/climate_data.R")

# import txt file first row as header, tab separated
climate_data <- read.table(
  "sunflo_french_repo/sunflo/data/AUZ_2014.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

# test sunflo
test <- run_sunflo(climate_data,
                   2, 
                   '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode')


plot(test$ThermalTime[test$CropYield>0],
     test$CropYield[test$CropYield>0])



lm(test$CropBiomass~test$ThermalTime)

# Fit a linear model
lm_model <- lm(CropBiomass ~ ThermalTime, data = test)

# Fit a piecewise linear model with a breakpoint
seg_model <- segmented(lm_model, seg.Z = ~ThermalTime, psi = 10)  # Initial guess for breakpoint at x=3

# View the summary of the segmented model
line_summary <- summary(seg_model)
line_summary$coefficients[2,1]

# Plot the data and the fitted piecewise model
plot(test$ThermalTime, test$CropBiomass, pch = 16, col = "blue", main = "Piecewise Linear Fit", xlab = "x", ylab = "y")
lines(test$ThermalTime, fitted(seg_model), col = "red", lwd = 2)  # Add the fitted line


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
    trait_expression <- .4



    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure <- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    # pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, plot = plot, day = day, pressure = pest_pressure))
    
    
    # sunflo
    sunflo <- run_sunflo(climate_data,
                        trait_expression, 
                       '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode')
    
    # Fit linear model
    lm_model <- lm(CropYeild ~ ThermalTime, data = sunflo)
    
    # Fit piecewise linear model with breakpoint
    seg_model <- segmented(lm_model, seg.Z = ~ThermalTime, psi = 10)  # Initial guess for breakpoint at x=3
    
    # summary of segmented model
    line_summary <- summary(seg_model)
    
    # pull growthrate from segmented model esimate
    # TODO: base growth rate based on climate --> add effect of investment in trait expression
    growth_rate <- line_summary$coefficients[2,1]
    
    
    

    # death related to intrinisic propbability of death 


    }
    # plot yield <-- TODO what are the sunflo unit here?
    
    # store yield and trait expression
    wild_population <- rbind(wild_population, data.table(
      year = year,
      plot = plot,
      n_survived = initial_wild, # Placeholder for number of survived plants
      trait_mean = wild_trait_expression,
      trait_sd = sd(wild_trait_expression), # Placeholder for standard deviation
      yield = wild_yield
    ))

    
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

