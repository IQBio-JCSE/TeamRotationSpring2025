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

# # find all files in sunflo_recode directory sunflo_recode/examples_for_presentation that start with "terp_stress_" .csv
# # Define the directory path
# sunflo_output_directory_path <- "sunflo_recode/examples_for_presentation"
# 
# # Find all files that match the pattern
# files <- list.files(path = sunflo_output_directory_path, pattern = "^terp_stress_.*\\.csv$", full.names = TRUE)
# 
# sunflo <- read_csv(files[1])
# 
# # terpinoid numbers
# # [1] "0 0"
# # [1] "0.5 0.166666666666667"
# # [1] "1 0.333333333333333"
# # [1] "1.5 0.5"
# # [1] "2 0.666666666666667"
# # [1] "2.5 0.833333333333333"
# # [1] "3 1"
# 
# # Define the terpinoid values
# terpinoid_values <- c(0, 0.16667, 0.33333, 0.5, 0.66667, 0.83333, 1)
# 
# # Function to find the closest index
# get_closest_index <- function(mean_trait_value) {
#   # Calculate the absolute difference between mean_trait_value and each terpinoid value
#   differences <- abs(terpinoid_values - mean_trait_value)
#   
#   # Find the index of the smallest difference
#   closest_index <- which.min(differences)
#   
#   return(closest_index)
# }


# find all files in sunflo_recode directory sunflo_recode/examples_for_presentation that start with "terp_stress_" .csv
# Define the directory path
# sunflo_output_directory_path <- "sunflo_recode/examples_for_presentation"
# 
# # Find all files that match the pattern
# files <- list.files(path = sunflo_output_directory_path, pattern = "^terp_stress_.*\\.csv$", full.names = TRUE)
# 
# sunflo <- read_csv(files[1])
# 
# # terpinoid numbers
# # [1] "0 0"
# # [1] "0.5 0.166666666666667"
# # [1] "1 0.333333333333333"
# # [1] "1.5 0.5"
# # [1] "2 0.666666666666667"
# # [1] "2.5 0.833333333333333"
# # [1] "3 1"
# 
# # Define the terpinoid values
# terpinoid_values <- c(0, 0.16667, 0.33333, 0.5, 0.66667, 0.83333, 1)
# 
# # Function to find the closest index
# get_closest_index <- function(mean_trait_value) {
#   # Calculate the absolute difference between mean_trait_value and each terpinoid value
#   differences <- abs(terpinoid_values - mean_trait_value)
#   
#   # Find the index of the smallest difference
#   closest_index <- which.min(differences)
#   
#   return(closest_index)
# }


# sunflo <- read_csv(files[1]) %>%
#   mutate(DayNumber = row_number())     # add day by row number
# plot_yield[plot] <- max(sunflo$CropYield)



# sunflo <- read_csv(files[1]) %>%
#   mutate(DayNumber = row_number())     # add day by row number
# plot_yield[plot] <- max(sunflo$CropYield)

# climate data
climate_data <- read.csv("climate_data/Climate_data.csv")

# trait values
source("simulation/Population_Simulation/Population_Gentic_Traits.R")

# Set seed for reproducibility -----------------------------------------------
set.seed(123)

# Define parameters -----------------------------------------------------------
years <- 2 # Number of years to simulate
years <- length(unique(climate_data$Year))

# number of plots of populations
plots <- 10

# initial populations of wild and domesticated plants assuming each plot is 1 acre
# initial_wild <- 10
# initial_domesticated <- 10

# patch of plants 7.0 plants per m^2 for a field plot of 5 acres
# 5 acres = 20234.3 m^2
plants_per_5_acres <- 7.0 * 20234.3  # plants per 5 acre
plants_per_acre <- 7.0 * 4046.86

wild_n_individuals <- round(plants_per_acre) * plots # Total number of wild individuals
domesticated_n_individuals <- round(plants_per_acre) * plots # Total number of domesticated individuals




# number of plants per plot

# selection proportion
selection_proportion <- 0.2
n_selected <- ceiling(plots * selection_proportion)






# Initialize populations ------------------------------------------------------
initialize_population <- function() {
  wild_population <- data.table(
    trait = numeric(),
    group = "Wild Population",
    size = plants_per_acre,
    resistance = numeric()
  )
  domesticated_population <- data.table(
    trait = numeric(),
    group = "Domesticated Population",
    size = plants_per_acre,
    resistance = numeric()
  )
  population <- rbind(wild_population, domesticated_population)
  population[, trait := pmax(0, pmin(1, trait))] # Ensure traits are within [0, 1]
  population[, resistance := trait]
  return(population)
}
population <- initialize_population()




# Initialize plant population
initialize_population <- function() {
  # Create wild population
  wild_population <- data.table(
    trait = rnorm(plants_per_acre, mean = 2/3, sd = 0.3),  # Gaussian distribution
    # growth_rate = NA,
    # reproduction_rate = NA,
    resistance = NA,
    size = plants_per_acre,
    group = "Wild Population"
  )
  
  # Ensure trait value within [0, 1] for wild population
  wild_population[trait < 0, trait := 0]
  wild_population[trait > 1, trait := 1]
  
  # Create domesticated population
  domesticated_population <- data.table(
    trait = rnorm(plants_per_acre, mean = 1/3, sd = 0.02),  # Narrow Gaussian distribution
    # growth_rate = NA,
    # reproduction_rate = NA,
    resistance = NA,
    size = plants_per_acre,
    group = "Domesticated Population"
  )
  
  # Ensure trait value within [0, 1] for domesticated population
  domesticated_population[trait < 0, trait := 0]
  domesticated_population[trait > 1, trait := 1]
  
  # Combine populations
  population <- rbind(wild_population, domesticated_population)
  
  # Initialize plant properties
  # population[, growth_rate := 1 - trait]
  # population[, reproduction_rate := 1 - trait]
  population[, resistance := trait]
  
  return(population)
}

# initialize_population <- function() {
#   wild_population <- data.table(
#     trait = numeric(),
#     group = "Wild Population",
#     size = plants_per_acre,
#     resistance = numeric()
#   )
#   domesticated_population <- data.table(
#     trait = numeric(),
#     group = "Domesticated Population",
#     size = plants_per_acre,
#     resistance = numeric()
#   )
#   population <- rbind(wild_population, domesticated_population)
#   population[, trait := pmax(0, pmin(1, trait))] # Ensure traits are within [0, 1]
#   population[, resistance := trait]
#   return(population)
# }
# population <- initialize_population()
# 
# 
# 
# 
# # Initialize plant population
# initialize_population <- function() {
#   # Create wild population
#   wild_population <- data.table(
#     trait = rnorm(plants_per_acre, mean = 2/3, sd = 0.3),  # Gaussian distribution
#     # growth_rate = NA,
#     # reproduction_rate = NA,
#     resistance = NA,
#     size = plants_per_acre,
#     group = "Wild Population"
#   )
#   
#   # Ensure trait value within [0, 1] for wild population
#   wild_population[trait < 0, trait := 0]
#   wild_population[trait > 1, trait := 1]
#   
#   # Create domesticated population
#   domesticated_population <- data.table(
#     trait = rnorm(plants_per_acre, mean = 1/3, sd = 0.02),  # Narrow Gaussian distribution
#     # growth_rate = NA,
#     # reproduction_rate = NA,
#     resistance = NA,
#     size = plants_per_acre,
#     group = "Domesticated Population"
#   )
#   
#   # Ensure trait value within [0, 1] for domesticated population
#   domesticated_population[trait < 0, trait := 0]
#   domesticated_population[trait > 1, trait := 1]
#   
#   # Combine populations
#   population <- rbind(wild_population, domesticated_population)
#   
#   # Initialize plant properties
#   # population[, growth_rate := 1 - trait]
#   # population[, reproduction_rate := 1 - trait]
#   population[, resistance := trait]
#   
#   return(population)
# }


# mortality function ---------------------------------------------------------
mortality <- function(population, max_temp, precip, pest_pressure) {
  with(population, {
    climate_stress <- 0.1 * ((max_temp - 35)/5)^2 + 0.1 * ((300 - precip)/50)^2
    herbivory <- pest_pressure * (1 - resistance)
    intrinsic <- ifelse(genotype == "domesticated", 0.05, 0.02)
    
    total_mortality <- 1 - ((1 - intrinsic) * (1 - herbivory) * exp(-climate_stress))
    survivors <- population[runif(nrow(population)) > total_mortality, ]
    return(survivors)
  })
}

mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
  climate_stress <- (max_temp/10 > 30) + (precip < 300)
  herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
  1/3/nrow(climate_data_year) + 0.1 * climate_stress + herbivory
}






# data storage ----------------------------------------------------------------
# data structure for storing population data, yield, trait expression
# wild_population <- data.table(
#   year = integer(),
#   plot = integer(),
#   n_survived = integer(),
#   trait_mean = numeric(),
#   trait_sd = numeric(),
#   yield = numeric()
# )
# domesticated_population <- data.table(
#   year = integer(),
#   plot = integer(),
#   n_survived = integer(),
#   trait_mean = numeric(),
#    trait_sd = numeric(),
#   yield = numeric()
# )
pest_pressure_data <- data.table(
  year = integer(),
  plot = integer(),
  day = integer(),
  pressure = numeric()
)

wild_plot_yield <- numeric(plots) # Placeholder for yield values for each plot 
dom_plot_yield <- numeric(plots) # Placeholder for yield values for each plot 

domesticated_mortality <- numeric()
wild_mortality <- numeric()

#   initialize population
population <- initialize_population()

results <- data.table(
  year = integer(),
  group = character(), 
  plot = integer(),
  day = integer(),
  pressure = numeric(),
  yield = integer(),
  avg_trait = numeric(),
  trait_variance = numeric(),
  upper_bound = numeric(),
  lower_bound = numeric())


# Initialize plot yield data
wild_selected_plots_trait <- numeric(n_selected) # Placeholder for selected wild plots trait expression
domesticated_selected_plots_trait <- numeric(n_selected) # Placeholder for selected domesticated plots trait expression



# Initialize trait expression for wild and domesticated populations
wild_trait_expression <- rnorm(wild_n_individuals, mean = 0.6, sd = 0.3) # Wild trait expression
domesticated_trait_expression <- rnorm(domesticated_n_individuals, mean = 0.3, sd = 0.1) # Domesticated trait expression






# Function to sample trait values within [0, 1]
resample_within_bounds <- function(n, mean, sd) {
  valid_values <- numeric(0)  # Initialize an empty vector to store valid values
  while (length(valid_values) < n) {
    # Sample additional values
    new_samples <- rnorm(n - length(valid_values), mean = mean, sd = sd)
    # Keep only values within [0, 1]
    valid_values <- c(valid_values, new_samples[new_samples >= 0 & new_samples <= 1])
  }
  return(valid_values)
}

# Initialize trait expression for wild and domesticated populations
wild_trait_expression_year <- resample_within_bounds(wild_n_individuals, mean = wild_median, sd = wild_sd)
summary(wild_trait_expression_year)

domesticated_trait_expression_year <- resample_within_bounds(domesticated_n_individuals, mean = domesticated_median, sd = domesticated_sd)
summary(domesticated_trait_expression_year)

# Plot initial trait distributions
hist(wild_trait_expression_year, main = 'Resampled Wild Trait Distribution')
hist(domesticated_trait_expression_year, main = 'Resampled Domesticated Trait Distribution')













# Main simulation loop --------------------------------------------------------

# set location
location <- "Georgia" # "Minnesota" # change location

# Loop through each year
for (year in 1:years) { # TODO: length(unique(climate_data$year)) # for each year in climate data
 # Filter climate data for the current year
  
  climate_data_year <- climate_data %>%
    filter(Year == climate_data$Year[year], Location == location)
  
  # climate_data_year <- climate_data[climate_data$Year == climate_data$Year[year],]  %>% # Filter climate data for the current year
  #   filter(Location == location)
# 
#   wild_trait_expression <- numeric(plots) # Reset trait expression values for each plot
#   domesticated_trait_expression <- numeric(plots) # Reset trait expression values for each plot


  for (plot in 1:plots) { # for each plot population
    # Initialize wild and domesticated populations for each plot

    wild_plot <- sample(wild_trait_expression, size = round(plants_per_acre), replace = FALSE)

    domesticated_plot <- sample(domesticated_trait_expression, size = round(plants_per_acre), replace = FALSE)


    # Reset plot yield for each year
  # TODO: plot yeild not start at 0 for wild populations
    
    # trait_expression <-- need ONE value for sunflo
    wild_trait_mean_expression <- mean(wild_plot)
    domesticated_trait_mean_expression <- mean(domesticated_plot)

    pest_pressure <- aphid_pest_pressure(climate_data_year$Temp_mean)
    # store pest pressure
    pest_pressure_data <- rbind(pest_pressure_data, 
                                data.table(year = year, 
                                           plot = plot, 
                                           day = day, 
                                           pressure = pest_pressure))

    for (day in 1:nrow(climate_data_year)) { # for each day in growing season
    # pest pressure

    
    
    # sunflo
    # sunflo <- run_sunflo(climate_data[year == year],
    #                     trait_expression, # wild_trait_expression or domesticated_trait_expression
    #                    '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode')
    
  
      #       # Fit a linear model
      #       wild_lm_model <- lm(CropBiomass ~ DayNumber, data = sunflo)
      #       # Fit a piecewise linear model with 2 breakpoints
      #       wild_seg_model <- segmented(lm_model, seg.Z = ~DayNumber, npsi=3)  # Initial guess for breakpoint at x=3
      #       #  summary of segmented model
      #       wild_line_summary <- summary(seg_model)
      #       wild_growth_rate <- line_summary$coefficients[2,1]
      
      
      
      # # Fit a linear model
      # domesticated_lm_model <- lm(CropBiomass ~ DayNumber, data = sunflo)
      # 
      # # Fit a piecewise linear model with 2 breakpoints
      # domesticated_seg_model <- segmented(lm_model, seg.Z = ~DayNumber, npsi=3)  # Initial guess for breakpoint at x=3
      # 
      # # TODO check coeffient
      # #  summary of segmented model
      # domesticated_line_summary <- summary(seg_model)
      # domesticated_growth_rate <- line_summary$coefficients[2,1]
      
      
      
      
      
    
    # # Fit a linear model
    # lm_model <- lm(CropBiomass ~ DayNumber, data = sunflo)
    # 
    # # Fit a piecewise linear model with 2 breakpoints
    # seg_model <- segmented(lm_model, seg.Z = ~DayNumber, npsi=3)  # Initial guess for breakpoint at x=3
    # 
    # # TODO check coeffient
    # #  summary of segmented model
    # line_summary <- summary(seg_model)
    # growth_rate <- line_summary$coefficients[2,1]
    # 
    # 

    # death related to intrinisic propbability of death 
    
    # population <- mortality_function(climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure, )
    
    wild_mortality[day] <- mortality_function(climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure, mean(population$trait[population$group=='Wild Population']) )
    
    domesticated_mortality[day] <-  mortality_function(climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure, mean(population$trait[population$group=='Domesticated Population']), TRUE )
    
  
    
    
    

    }

    
    dom_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group=='Domesticated Population']))]) %>%
      mutate(DayNumber = row_number())     # add day by row number
    dom_plot_yield[plot] <- max(dom_sunflo$CropYield) * max(domesticated_mortality)
    
    wild_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group=='Wild Population']))]) %>%
      mutate(DayNumber = row_number())     # add day by row number
    wild_plot_yield[plot] <- max(wild_sunflo$CropYield) * max(wild_mortality)
    
    
    
    
    
    
    # dom_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group=='Domesticated Population']))]) %>%
    #   mutate(DayNumber = row_number())     # add day by row number
    # dom_plot_yield[plot] <- max(dom_sunflo$CropYield) * max(domesticated_mortality)
    # 
    # wild_sunflo <- read_csv(files[get_closest_index(mean(population$trait[population$group=='Wild Population']))]) %>%
    #   mutate(DayNumber = row_number())     # add day by row number
    
    
    # TODO what are sunflo yeild units
    
    wild_upper_bound <- mean(population$trait[population$group=='Wild Population']) + sd(population$trait[population$group=='Wild Population'])
    wild_lower_bound <- mean(population$trait[population$group=='Wild Population']) - sd(population$trait[population$group=='Wild Population'])
    domesticated_upper_bound <- mean(population$trait[population$group=='Domesticated Population']) + sd(population$trait[population$group=='Domesticated Population'])
    domesticated_lower_bound <- mean(population$trait[population$group=='Domesticated Population']) - sd(population$trait[population$group=='Domesticated Population'])
    
    

    # Append results to the tracking data.table
    # results <- rbind(results, data.table(
    #   year = year,
    #   plot = plot,
    #   group = "Wild Population",
    #   yield = wild_plot_yield,
    #   avg_trait = mean(population$trait[population$group=='Wild Population']),
    #   trait_variance = sd(population$trait[population$group=='Wild Population']),
    #   upper_bound = wild_upper_bound,
    #   lower_bound = wild_lower_bound
    # ))
    # results <- rbind(results, data.table(
    #   year = year,
    #   plot = plot,
    #   group = "Domesticated Population",
    #   yield = dom_plot_yield,
    #   avg_trait = mean(population$trait[population$group=='Domesticated Population']),
    #   trait_variance = sd(population$trait[population$group=='Domesticated Population']),
    #   upper_bound = domesticated_upper_bound,
    #   lower_bound = domesticated_lower_bound
    # ))
    
    # Append results to the tracking data.table
    results <- rbindlist(list(
      results,
      data.table(
        year = year,
        plot = plot,
        group = "Wild Population",
        yield = wild_plot_yield,
        avg_trait = mean(population$trait[population$group == 'Wild Population']),
        trait_variance = sd(population$trait[population$group == 'Wild Population']),
        upper_bound = wild_upper_bound,
        lower_bound = wild_lower_bound
      ),
      data.table(
        year = year,
        plot = plot,
        group = "Domesticated Population",
        yield = dom_plot_yield,
        avg_trait = mean(population$trait[population$group == 'Domesticated Population']),
        trait_variance = sd(population$trait[population$group == 'Domesticated Population']),
        upper_bound = domesticated_upper_bound,
        lower_bound = domesticated_lower_bound
      )
    ), fill = TRUE)
    
    # new_wild_population_size <- plants_per_acre - wild_survived
    
    # # store yield and trait expression
    # wild_population <- rbind(wild_population, data.table(
    #   year = year,
    #   plot = plot,
    #   n_survived = initial_wild, # Placeholder for number of survived plants
    #   trait_mean = wild_trait_expression,
    #   trait_sd = sd(wild_trait_expression), # Placeholder for standard deviation
    #   yield = wild_yield
    # ))
    # 
    # 
    # domesticated_population <- rbind(domesticated_population, data.table(
    #   year = year,
    #   plot = plot,
    #   n_survived = initial_domesticated, # Placeholder for number of survived plants
    #   trait_mean = domesticated_trait_expression,
    #   trait_sd = sd(domesticated_trait_expression), # Placeholder for standard deviation
    #   yield = domesticated_yield
    # ))
    
    # Append results to the tracking data.table
    results <- rbind(results, data.table(
      year = year,
      plot = plot,
      group = "Wild Population",
      yield = wild_plot_yield,
      avg_trait = mean(population$trait[population$group=='Wild Population']),
      trait_variance = sd(population$trait[population$group=='Wild Population']),
      upper_bound = wild_upper_bound,
      lower_bound = wild_lower_bound
    ))
    results <- rbind(results, data.table(
      year = year,
      plot = plot,
      group = "Domesticated Population",
      yield = dom_plot_yield,
      avg_trait = mean(population$trait[population$group=='Domesticated Population']),
      trait_variance = sd(population$trait[population$group=='Domesticated Population']),
      upper_bound = domesticated_upper_bound,
      lower_bound = domesticated_lower_bound
    ))
    
    
    
    
    # new_wild_population_size <- plants_per_acre - wild_survived
    # # store yield and trait expression
    # wild_population <- rbind(wild_population, data.table(
    #   year = year,
    #   plot = plot,
    #   n_survived = initial_wild, # Placeholder for number of survived plants
    #   trait_mean = wild_trait_expression,
    #   trait_sd = sd(wild_trait_expression), # Placeholder for standard deviation
    #   yield = wild_yield
    # ))
    # 
    # domesticated_population <- rbind(domesticated_population, data.table(
    #   year = year,
    #   plot = plot,
    #   n_survived = initial_domesticated, # Placeholder for number of survived plants
    #   trait_mean = domesticated_trait_expression,
    #   trait_sd = sd(domesticated_trait_expression), # Placeholder for standard deviation
    #   yield = domesticated_yield
    # ))

  }




  # Select top plots based on yield value
  wild_selected_plots_trait <- sort(wild_plot_yield, decreasing = TRUE)[1:n_selected]
  domesticated_new_population <- sort(dom_plot_yield, decreasing = TRUE)[1:n_selected]

  # new plots inherit average of selected plots trait + some random variation
  wild_new_population <- rnorm(wild_n_individuals, 
                               mean = mean(wild_selected_plots_trait), 
                               sd = sd(wild_selected_plots_trait) * 0.1)
  wild_new_population <- pmax(0, pmin(1, wild_new_population)) # Ensure values within [0, 1]
  domesticated_new_population <- rnorm(domesticated_n_individuals, 
                                       mean = mean(domesticated_selected_plots_trait), 
                                       sd = sd(domesticated_selected_plots_trait) * 0.1)
  domesticated_new_population <- pmax(0, pmin(1, domesticated_new_population)) # Ensure values within [0, 1]
  
  
  
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
      title = sprintf("Time Steps: %d, Pest Pressure: %.2f", year, pest_pressure),
      x = "Time Steps",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal()
  
  
  
  
  
  
  
  

  
  
  
  
}

# Run simulation --------------------------------------------------------------



