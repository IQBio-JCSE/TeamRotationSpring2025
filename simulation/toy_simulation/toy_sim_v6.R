#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' full version

# Load libraries --------------------------------------------------------------
library(tidyverse)

# Load functions --------------------------------------------------------------
source("simulation/Population_Simulation/Functions.r") # Load helper functions
source("sunflo_recode/run_model.R") # Load sunflower model

# Find all files in sunflo_recode directory -----------------------------------
sunflo_output_directory_path <- "sunflo_recode/examples_for_presentation"
files <- list.files(path = sunflo_output_directory_path, pattern = "^terp_stress_.*\\.csv$", full.names = TRUE)

# sunflo <- read_csv(files[1])

# Define the terpinoid values -------------------------------------------------
terpinoid_values <- c(0, 0.16667, 0.33333, 0.5, 0.66667, 0.83333, 1)

# Function to find the closest index
get_closest_index <- function(mean_trait_value) {
  differences <- abs(terpinoid_values - mean_trait_value)
  closest_index <- which.min(differences)
  return(closest_index)
}

# Load climate data -----------------------------------------------------------
climate_data <- read.csv("climate_data/Climate_data.csv")

# Set seed for reproducibility ------------------------------------------------
set.seed(123)

# Define parameters -----------------------------------------------------------
years <- length(unique(climate_data$Year))
plots <- 10
plants_per_acre <- 7.0 * 4046.86
wild_n_individuals <- round(plants_per_acre) * plots
domesticated_n_individuals <- round(plants_per_acre) * plots
selection_proportion <- 0.2
n_selected <- ceiling(plots * selection_proportion)

# Initialize populations ------------------------------------------------------
initialize_population <- function() {
  wild_population <- data.frame(
    trait = rnorm(plants_per_acre, mean = 2/3, sd = 0.3),
    group = "Wild Population",
    resistance = NA,
    size = plants_per_acre
  )
  domesticated_population <- data.frame(
    trait = rnorm(plants_per_acre, mean = 1/3, sd = 0.02),
    group = "Domesticated Population",
    resistance = NA,
    size = plants_per_acre
  )
  population <- rbind(wild_population, domesticated_population)
  population$trait <- pmax(0, pmin(1, population$trait)) # Ensure traits are within [0, 1]
  population$resistance <- population$trait
  return(population)
}

population <- initialize_population()

# Mortality function ----------------------------------------------------------
mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
  climate_stress <- (max_temp / 10 > 30) + (precip < 300)
  herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
  return(1 / 3 / nrow(climate_data_year) + 0.1 * climate_stress + herbivory)
}

# Data storage ----------------------------------------------------------------
pest_pressure_data <- data.frame(
  year = integer(),
  plot = integer(),
  day = integer(),
  pressure = numeric()
)

wild_plot_yield <- numeric(plots)
dom_plot_yield <- numeric(plots)

domesticated_mortality <- numeric()
wild_mortality <- numeric()

results <- data.frame(
  year = integer(),
  group = character(),
  plot = integer(),
  day = integer(),
  pressure = numeric(),
  yield = numeric(),
  avg_trait = numeric(),
  trait_variance = numeric(),
  upper_bound = numeric(),
  lower_bound = numeric()
)

wild_selected_plots_trait <- numeric(n_selected)
domesticated_selected_plots_trait <- numeric(n_selected)

wild_trait_expression <- rnorm(wild_n_individuals, mean = 0.6, sd = 0.3)
domesticated_trait_expression <- rnorm(domesticated_n_individuals, mean = 0.3, sd = 0.1)

# Main simulation loop --------------------------------------------------------
location <- "Georgia"

for (year in 1:years) {
  climate_data_year <- subset(climate_data, Year == unique(climate_data$Year)[year] & Location == location)
  
  for (plot in 1:plots) {
    wild_plot <- sample(wild_trait_expression, size = round(plants_per_acre), replace = FALSE)
    domesticated_plot <- sample(domesticated_trait_expression, size = round(plants_per_acre), replace = FALSE)
    
    wild_trait_mean_expression <- mean(wild_plot)
    domesticated_trait_mean_expression <- mean(domesticated_plot)
    
    pest_pressure <- aphid_pest_pressure(climate_data_year$Temp_mean)
    pest_pressure_data <- rbind(pest_pressure_data, data.frame(
      year = year,
      plot = plot,
      day = 1:nrow(climate_data_year),
      pressure = pest_pressure
    ))
    
    for (day in 1:nrow(climate_data_year)) {
      wild_mortality[day] <- mortality_function(
        climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure,
        mean(population$trait[population$group == "Wild Population"])
      )
      domesticated_mortality[day] <- mortality_function(
        climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure,
        mean(population$trait[population$group == "Domesticated Population"]), TRUE
      )
    }
    
    dom_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group == "Domesticated Population"]))]) %>%
      mutate(DayNumber = row_number())
    dom_plot_yield[plot] <- max(dom_sunflo$CropYield) * max(domesticated_mortality)
    
    wild_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group == "Wild Population"]))]) %>%
      mutate(DayNumber = row_number())
    wild_plot_yield[plot] <- max(wild_sunflo$CropYield) * max(wild_mortality)
    
    wild_upper_bound <- mean(population$trait[population$group == "Wild Population"]) +
      sd(population$trait[population$group == "Wild Population"])
    wild_lower_bound <- mean(population$trait[population$group == "Wild Population"]) -
      sd(population$trait[population$group == "Wild Population"])
    domesticated_upper_bound <- mean(population$trait[population$group == "Domesticated Population"]) +
      sd(population$trait[population$group == "Domesticated Population"])
    domesticated_lower_bound <- mean(population$trait[population$group == "Domesticated Population"]) -
      sd(population$trait[population$group == "Domesticated Population"])
    
    results <- rbind(results, data.frame(
      year = year,
      plot = plot,
      group = "Wild Population",
      yield = wild_plot_yield#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
      #' Sarah Elizabeth Stockman
      #' April 2025
      #' full version
      
      # Load libraries --------------------------------------------------------------
      library(tidyverse)
      
      # Load functions --------------------------------------------------------------
      source("simulation/Population_Simulation/Functions.r") # Load helper functions
      source("sunflo_recode/run_model.R") # Load sunflower model
      
      # Find all files in sunflo_recode directory -----------------------------------
      sunflo_output_directory_path <- "sunflo_recode/examples_for_presentation"
      files <- list.files(path = sunflo_output_directory_path, pattern = "^terp_stress_.*\\.csv$", full.names = TRUE)
      
      sunflo <- read_csv(files[1])
      
      # Define the terpinoid values -------------------------------------------------
      terpinoid_values <- c(0, 0.16667, 0.33333, 0.5, 0.66667, 0.83333, 1)
      
      # Function to find the closest index
      get_closest_index <- function(mean_trait_value) {
        differences <- abs(terpinoid_values - mean_trait_value)
        closest_index <- which.min(differences)
        return(closest_index)
      }
      
      # Load climate data -----------------------------------------------------------
      climate_data <- read.csv("climate_data/Climate_data.csv")
      
      # Set seed for reproducibility ------------------------------------------------
      set.seed(123)
      
      # Define parameters -----------------------------------------------------------
      years <- length(unique(climate_data$Year))
      plots <- 10
      plants_per_acre <- 7.0 * 4046.86
      wild_n_individuals <- round(plants_per_acre) * plots
      domesticated_n_individuals <- round(plants_per_acre) * plots
      selection_proportion <- 0.2
      n_selected <- ceiling(plots * selection_proportion)
      
      # Initialize populations ------------------------------------------------------
      initialize_population <- function() {
        wild_population <- data.frame(
          trait = rnorm(plants_per_acre, mean = 2/3, sd = 0.3),
          group = "Wild Population",
          resistance = NA,
          size = plants_per_acre
        )
        domesticated_population <- data.frame(
          trait = rnorm(plants_per_acre, mean = 1/3, sd = 0.02),
          group = "Domesticated Population",
          resistance = NA,
          size = plants_per_acre
        )
        population <- rbind(wild_population, domesticated_population)
        population$trait <- pmax(0, pmin(1, population$trait)) # Ensure traits are within [0, 1]
        population$resistance <- population$trait
        return(population)
      }
      
      population <- initialize_population()
      
      # Mortality function ----------------------------------------------------------
      mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
        climate_stress <- (max_temp / 10 > 30) + (precip < 300)
        herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
        return(1 / 3 / nrow(climate_data_year) + 0.1 * climate_stress + herbivory)
      }
      
      # Data storage ----------------------------------------------------------------
      pest_pressure_data <- data.frame(
        year = integer(),
        plot = integer(),
        day = integer(),
        pressure = numeric()
      )
      
      wild_plot_yield <- numeric(plots)
      dom_plot_yield <- numeric(plots)
      
      domesticated_mortality <- numeric()
      wild_mortality <- numeric()
      
      results <- data.frame(
        year = integer(),
        group = character(),
        plot = integer(),
        day = integer(),
        pressure = numeric(),
        yield = numeric(),
        avg_trait = numeric(),
        trait_variance = numeric(),
        upper_bound = numeric(),
        lower_bound = numeric()
      )
      
      wild_selected_plots_trait <- numeric(n_selected)
      domesticated_selected_plots_trait <- numeric(n_selected)
      
      wild_trait_expression <- rnorm(wild_n_individuals, mean = 0.6, sd = 0.3)
      domesticated_trait_expression <- rnorm(domesticated_n_individuals, mean = 0.3, sd = 0.1)
      
      # Main simulation loop --------------------------------------------------------
      location <- "Georgia"
      
      for (year in 1:years) {
        climate_data_year <- subset(climate_data, Year == unique(climate_data$Year)[year] & Location == location)
        
        for (plot in 1:plots) {
          wild_plot <- sample(wild_trait_expression, size = round(plants_per_acre), replace = FALSE)
          domesticated_plot <- sample(domesticated_trait_expression, size = round(plants_per_acre), replace = FALSE)
          
          wild_trait_mean_expression <- mean(wild_plot)
          domesticated_trait_mean_expression <- mean(domesticated_plot)
          
          pest_pressure <- aphid_pest_pressure(climate_data_year$Temp_mean)
          pest_pressure_data <- rbind(pest_pressure_data, data.frame(
            year = year,
            plot = plot,
            day = 1:nrow(climate_data_year),
            pressure = pest_pressure
          ))
          
          for (day in 1:nrow(climate_data_year)) {
            wild_mortality[day] <- mortality_function(
              climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure,
              mean(population$trait[population$group == "Wild Population"])
            )
            domesticated_mortality[day] <- mortality_function(
              climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure,
              mean(population$trait[population$group == "Domesticated Population"]), TRUE
            )
          }
          
          dom_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group == "Domesticated Population"]))]) %>%
            mutate(DayNumber = row_number())
          dom_plot_yield[plot] <- max(dom_sunflo$CropYield) * max(domesticated_mortality)
          
          wild_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group == "Wild Population"]))]) %>%
            mutate(DayNumber = row_number())
          wild_plot_yield[plot] <- max(wild_sunflo$CropYield) * max(wild_mortality)
          
          wild_upper_bound <- mean(population$trait[population$group == "Wild Population"]) +
            sd(population$trait[population$group == "Wild Population"])
          wild_lower_bound <- mean(population$trait[population$group == "Wild Population"]) -
            sd(population$trait[population$group == "Wild Population"])
          domesticated_upper_bound <- mean(population$trait[population$group == "Domesticated Population"]) +
            sd(population$trait[population$group == "Domesticated Population"])
          domesticated_lower_bound <- mean(population$trait[population$group == "Domesticated Population"]) -
            sd(population$trait[population$group == "Domesticated Population"])
          
          results <- rbind(results, data.frame(
            year = year,
            plot = plot,
            group = "Wild Population",
            yield = wild_plot_yield