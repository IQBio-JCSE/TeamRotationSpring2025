#' sunflower simulation
#' 2 populations: wild and domesticated ith different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' March 2025
#' 
#' TODO:
#' two populations: wild and domesticated
#' time step: 1 year
#' life cycle: growth/production, reproduction, mortality, selection
#' 3 parameters of interest:
#' temperature
#' percipitation
#' pest pressure
#' wild population has higher secondary metabolite expression
  #' growth/production function of base growth rate (lower than domesticated population),
  #' climate, & secondary metabolite expression (higher in wild population & decreases with domestication)
  #' mortality function of base mortality rate (linear relationship, dealth by 3-5 years), climate (high temperature & low percipitation),
  #' herbivory with high pest pressure (but lower than domesticated population),
  #' reproduction function of base reproduction rate (lower than domesticated population), 
  #' less with high pest pressure (so secondary metabolite expression higher),
  #' selection function of human selection on higher yeild production & natural selction
#' domesticated population has lower secondary metabolite expression
#' life cycle in one year
  #' growth/production function of base growth rate (higher than wild population) & climate (no extreme temperature & percipitation),
  #' reproduction function of base reproduction rate (higher than wild population), 
  #' mortality function of climate (high temperature & low percipitation), pest pressure (higher mortality rate with pest pressure than wild population),
  #' selection function of human selection on higher yeild production
  #' all indivduals (death every life cycle)
#' traits: secondary metabolite expression
#' when econdary metabolite expression high & high pest pressure then mortality low, but growth lowered
#' when secondary metabolite expression low & high pest pressure then mortality high, growth remain
#' when secondary metabolite expression high & low pest pressure then mortality low, growth remain
#' when secondary metabolite expression low & low pest pressure then mortality high, growth lowered
#' 
#' 

library(data.table) # data manipulation
library(ggplot2) # plotting
library(parallel)  # parallelization




# linear relationship between tempurture and yeild

# relationship between yield
# serve as a place holder until we have better data for the species


# Initialize parameters
num_wild_population <- 1000
num_domesticated_population <- 1000
time_steps <- 0




# Initialize plant population
initialize_population <- function() {
  # Create wild population
  wild_population <- data.table(
    trait = rnorm(num_wild_population, mean = 0.7, sd = 0.3),  # Gaussian distribution
    growth_rate = NA,
    reproduction_rate = NA,
    resistance = NA,
    size = 1,
    group = "Wild Population"
  )
  
  # Ensure trait value within [0, 1] for wild population
  wild_population[trait < 0, trait := 0]
  wild_population[trait > 1, trait := 1]

  # Create domesticated population
  domesticated_population <- data.table(
    trait = rnorm(num_domesticated_population, mean = 0.5, sd = 0.02),  # Narrow Gaussian distribution
    growth_rate = NA,
    reproduction_rate = NA,
    resistance = NA,
    size = 1,
    group = "Domesticated Population"
  )
  
  # Ensure trait value within [0, 1] for domesticated population
  domesticated_population[trait < 0, trait := 0]
  domesticated_population[trait > 1, trait := 1]

  # Combine populations
  population <- rbind(wild_population, domesticated_population)

  # Initialize plant properties
  population[, growth_rate := 1 - trait]
  population[, reproduction_rate := 1 - trait]
  population[, resistance := trait]

  return(population)
}

# # Grow function
# grow <- function(population) {
#   population[, size := size + growth_rate * 0.1]
#   return(population)
# }

# Updated grow function based on trait and pest pressure
grow <- function(population, pest_pressure) {
  population[, growth_rate := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.05,
                               fifelse(trait < 0.5 & pest_pressure < 0.5, 0.05,
                               0.1))]
  population[, size := size + growth_rate]
  return(population)
}


# # mortality function
# suffer_mortality <- function(population, pest_pressure) {
#   mortality_prob <- pest_pressure * (1 - population$resistance) * 0.1
#   survivors <- population[runif(.N) >= mortality_prob]
#   return(survivors)
# }
# Updated mortality function based on trait and pest pressure
suffer_mortality <- function(population, pest_pressure) {
  population[, mortality_prob := fifelse(trait >= 0.5 & pest_pressure >= 0.5, 0.01,
                                  fifelse(trait < 0.5 & pest_pressure >= 0.5, 0.2,
                                  fifelse(trait >= 0.5 & pest_pressure < 0.5, 0.01,
                                  0.15)))]
  survivors <- population[runif(.N) >= mortality_prob]
  return(survivors)
}


# Reproduce function
reproduce <- function(population) {
  # Calculate number of offspring for each individual plant
  reproduction_prob <- population$reproduction_rate * 0.8
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








plot_population <- function(population, time_step, results) {
  wild_population_count <- nrow(population[group == "Wild Population"])
  domesticated_population_count <- nrow(population[group == "Domesticated Population"])
  wild_avg_trait <- mean(population[group == "Wild Population", trait])
  domesticated_avg_trait <- mean(population[group == "Domesticated Population", trait])
  wild_trait_variance <- var(population[group == "Wild Population", trait])
  domesticated_trait_variance <- var(population[group == "Domesticated Population", trait])
  
  # Calculate bounds for variance
  wild_upper_bound <- wild_avg_trait + sqrt(wild_trait_variance)
  wild_lower_bound <- wild_avg_trait - sqrt(wild_trait_variance)
  domesticated_upper_bound <- domesticated_avg_trait + sqrt(domesticated_trait_variance)
  domesticated_lower_bound <- domesticated_avg_trait - sqrt(domesticated_trait_variance)
  
  # Append results to the tracking data.table
  results <- rbind(results, data.table(
    time = time_step,
    group = "Wild Population",
    count = wild_population_count,
    avg_trait = wild_avg_trait,
    trait_variance = wild_trait_variance,
    upper_bound = wild_upper_bound,
    lower_bound = wild_lower_bound
  ))
  results <- rbind(results, data.table(
    time = time_step,
    group = "Domesticated Population",
    count = domesticated_population_count,
    avg_trait = domesticated_avg_trait,
    trait_variance = domesticated_trait_variance,
    upper_bound = domesticated_upper_bound,
    lower_bound = domesticated_lower_bound
  ))
  
  return(results)
}


# simulate <- function(time_steps, pest_pressure) {
#   population <- initialize_population()
#   results <- data.table()
  
#   for (t in 1:time_steps) {
#     pest_pressure <- pest_pressure + sin(t/10) * 0.2  # Oscillates around base value
#     pest_pressure <- min(max(pest_pressure, 0), 1)  # Clamp between 0 and 1
    
#     population <- grow(population, pest_pressure)
#     population <- reproduce(population)
#     population <- suffer_mortality(population, pest_pressure)
#     results <- plot_population(population, t, results)
#   }

#   # [ ... plotting code unchanged ... ]
# }








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
    pest_pressure <- pest_pressure + sin(t/10) * 0.2  # Oscillates around base value
    pest_pressure <- min(max(pest_pressure, 0), 1)  # Clamp between 0 and 1
    

    population <- grow(population, pest_pressure)
    population <- reproduce(population)
    population <- suffer_mortality(population, pest_pressure)
    results <- plot_population(population, t, results)
  }
  

  ggplot(results, aes(x = time)) +
    # Population size
    geom_line(aes(y = count, color = group), linewidth = 1) +
    # Average trait (scaled to align with population size)
    geom_line(aes(y = avg_trait * max(results$count) / max(results$avg_trait), 
                  color = group, linetype = group)) +
    # Bounding region for variance (scaled to align with population size)
    geom_ribbon(aes(ymin = lower_bound * max(results$count) / max(results$avg_trait), 
                    ymax = upper_bound * max(results$count) / max(results$avg_trait), 
                    fill = group), alpha = 0.1) +
    scale_y_continuous(
      name = "Population Size",
      sec.axis = sec_axis(~ . * max(results$avg_trait) / max(results$count), 
                          name = "Secondary Metabolite Expression (0-1)")
    ) +
    scale_color_manual(
      values = c("Wild Population" = "#27582b", "Domesticated Population" = "#59466a")  # Green and Purple
    ) +
    scale_fill_manual(
      values = c("Wild Population" = "#27582b", "Domesticated Population" = "#59466a")  # Green and Purple
    ) +
    scale_linetype_manual(
    values = c("Wild Population" = "dashed", "Domesticated Population" = "dashed")
    ) +
    guides(
      color = guide_legend(order = 1, title = "Population Size"),
      fill = guide_legend(order = 2, title = "Trait Expression"),
      linetype = guide_legend(order = 2, title = "Trait Expression")
    ) +
    labs(
      title = sprintf("Time Steps: %d, Pest Pressure: %.2f", time_steps, pest_pressure),
      x = "Time Steps",
      color = "Group",
      fill = "Group",
      linetype = "Group"
    ) +
    theme_minimal()
}
  
 # TODO add evoltution
# reproduce <- function(population) {
#   reproduction_prob <- population$reproduction_rate * 0.05
#   num_offspring <- floor(reproduction_prob)
#   new_plants <- population[rep(1:.N, num_offspring)]
#   
#   if (nrow(new_plants) > 0) {
#     new_plants[, trait := pmin(pmax(trait + rnorm(.N, 0, 0.01), 0), 1)]  # Mutation
#     new_plants[, size := 1]
#     new_plants[, growth_rate := 1 - trait]
#     new_plants[, reproduction_rate := 1 - trait]
#     new_plants[, resistance := trait]
#     population <- rbind(population, new_plants)
#   }
#   
#   return(population)
# }


# Run simulation for single instance to test
result_plot <- simulate(time_steps = 100, pest_pressure = 0.8)
result_plot$data

ggsave("simulation_plot.png", plot = result_plot, width = 10, height = 6)

run_simulations_parallel <- function(num_simulations, time_steps, pest_pressure) {
  # Detect the number of available cores
  num_cores <- detectCores() - 1  # Leave one core free for system processes
  
  # Set up a cluster
  cl <- makeCluster(num_cores)
  
  # Load required libraries on each worker
  clusterEvalQ(cl, {
    library(data.table)
    library(ggplot2)
  })
  
  # Export necessary functions and variables to the cluster
  clusterExport(cl, varlist = c("initialize_population", "grow", "reproduce", 
                                "suffer_mortality", "plot_population", "simulate", 
                                "time_steps", "pest_pressure", "num_wild_population", 
                                "num_domesticated_population"), envir = environment())
  
  # Run simulations in parallel
  results_list <- parLapply(cl, 1:num_simulations, function(i) {
    simulate(time_steps, pest_pressure)
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  return(results_list)
}

# Run 1000 simulations in parallel
num_simulations <- 100
time_steps <- 100
pest_pressure <- 0.5

all_results <- run_simulations_parallel(num_simulations, time_steps, pest_pressure)
# TODO aggregate results or visualize
