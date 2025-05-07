# Combined Simulation Script
# Sarah Elizabeth Stockman
# May 2025

# Load libraries --------------------------------------------------------------
library(data.table) # Data manipulation
library(tidyverse)  # Data wrangling and visualization
library(segmented)  # Piecewise regression
library(ggplot2)    # Plotting

# Load functions --------------------------------------------------------------
source("simulation/Population_Simulation/Functions.r") # Load helper functions
source("sunflo_recode/run_model.R") # Load sunflower model

# Define parameters -----------------------------------------------------------
sunflo_output_directory_path <- "sunflo_recode/examples_for_presentation"
files <- list.files(path = sunflo_output_directory_path, pattern = "^terp_stress_.*\\.csv$", full.names = TRUE)

climate_data <- read.csv("climate_data/Climate_data.csv")
set.seed(123)

plots <- 10
plants_per_acre <- 7.0 * 4046.86
wild_n_individuals <- round(plants_per_acre) * plots
domesticated_n_individuals <- round(plants_per_acre) * plots

# Initialize populations ------------------------------------------------------
initialize_population <- function() {
  wild_population <- data.table(
    trait = rnorm(wild_n_individuals, mean = 0.6, sd = 0.3),
    group = "Wild Population"
  )
  domesticated_population <- data.table(
    trait = rnorm(domesticated_n_individuals, mean = 0.3, sd = 0.1),
    group = "Domesticated Population"
  )
  population <- rbind(wild_population, domesticated_population)
  population[, trait := pmax(0, pmin(1, trait))] # Ensure traits are within [0, 1]
  return(population)
}

# Growth function -------------------------------------------------------------
grow <- function(population, pest_pressure) {
  population[, growth_rate := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.05,
                                      fifelse(trait < 0.5 & pest_pressure < 0.5, 0.05, 0.1))]
  population[, size := size + growth_rate]
  return(population)
}

# Mortality function ----------------------------------------------------------
suffer_mortality <- function(population, pest_pressure) {
  population[, mortality_prob := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.01,
                                         fifelse(trait < 0.5 & pest_pressure >= 0.5, 0.2,
                                                 fifelse(trait >= 0.5 & pest_pressure < 0.5, 0.01, 0.15)))]
  survivors <- population[runif(.N) >= mortality_prob]
  return(survivors)
}

# Reproduction function -------------------------------------------------------
reproduce <- function(population) {
  reproduction_prob <- population$reproduction_rate * 0.8
  num_offspring <- floor(reproduction_prob)
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

# Simulation function ---------------------------------------------------------
simulate <- function(time_steps, pest_pressure) {
  population <- initialize_population()
  results <- data.table(
    time = integer(),
    group = character(),
    count = integer(),
    avg_trait = numeric(),
    trait_variance = numeric(),
    upper_bound = numeric(),
    lower_bound = numeric()
  )
  
  for (t in 1:time_steps) {
    pest_pressure <- pest_pressure + sin(t / 10) * 0.2
    pest_pressure <- min(max(pest_pressure, 0), 1)
    population <- grow(population, pest_pressure)
    population <- reproduce(population)
    population <- suffer_mortality(population, pest_pressure)
    
    # Track population metrics
    results <- rbind(results, data.table(
      time = t,
      group = "Wild Population",
      count = nrow(population[group == "Wild Population"]),
      avg_trait = mean(population[group == "Wild Population", trait]),
      trait_variance = var(population[group == "Wild Population", trait]),
      upper_bound = mean(population[group == "Wild Population", trait]) + sqrt(var(population[group == "Wild Population", trait])),
      lower_bound = mean(population[group == "Wild Population", trait]) - sqrt(var(population[group == "Wild Population", trait]))
    ))
    results <- rbind(results, data.table(
      time = t,
      group = "Domesticated Population",
      count = nrow(population[group == "Domesticated Population"]),
      avg_trait = mean(population[group == "Domesticated Population", trait]),
      trait_variance = var(population[group == "Domesticated Population", trait]),
      upper_bound = mean(population[group == "Domesticated Population", trait]) + sqrt(var(population[group == "Domesticated Population", trait])),
      lower_bound = mean(population[group == "Domesticated Population", trait]) - sqrt(var(population[group == "Domesticated Population", trait]))
    ))
  }
  
  # Plot results
  ggplot(results, aes(x = time)) +
    geom_line(aes(y = count, color = group), linewidth = 1) +
    geom_line(aes(y = avg_trait * max(results$count) / max(results$avg_trait), color = group, linetype = group)) +
    geom_ribbon(aes(ymin = lower_bound * max(results$count) / max(results$avg_trait), 
                    ymax = upper_bound * max(results$count) / max(results$avg_trait), 
                    fill = group), alpha = 0.1) +
    scale_y_continuous(
      name = "Population Size",
      sec.axis = sec_axis(~ . * max(results$avg_trait) / max(results$count), 
                          name = "Secondary Metabolite Expression (0-1)")
    ) +
    scale_color_manual(values = c("Wild Population" = "#27582b", "Domesticated Population" = "#59466a")) +
    scale_fill_manual(values = c("Wild Population" = "#27582b", "Domesticated Population" = "#59466a")) +
    labs(
      title = sprintf("Time Steps: %d, Pest Pressure: %.2f", time_steps, pest_pressure),
      x = "Time Steps",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal()
}

# Run simulation --------------------------------------------------------------
result_plot <- simulate(time_steps = 100, pest_pressure = 0.8)
ggsave("simulation_plot.png", plot = result_plot, width = 10, height = 6)