#' SUNFLO Crop Model Simulation
#' 
#' This script provides a complete workflow for running simulations with the SUNFLO crop model
#' using the VLE simulation platform and R. It includes installation checks, data preparation,
#' model execution, and results analysis.

# Installation and Setup --------------------------------------------------------

#' Check and install required packages
install_required_packages <- function() {
  # Check and install devtools if not present
  if (!requireNamespace("devtools", quietly = TRUE)) {
    message("Installing devtools package...")
    install.packages("devtools")
  }
  
  # Check and install rvle if not present
  if (!requireNamespace("rvle", quietly = TRUE)) {
    message("Installing rvle package...")
    devtools::install_github("picasa/rvle")
  }
  
  # Check and install rsunflo if not present
  if (!requireNamespace("rsunflo", quietly = TRUE)) {
    message("Installing rsunflo package...")
    devtools::install_github("picasa/rsunflo")
  }
  
  # Load required packages
  library(rvle)
  library(rsunflo)
  library(tidyverse)
  
  # Verify VLE installation
  if (!"Rvle" %in% class(rvle::get_vle())) {
    stop("VLE simulation platform not installed. Please install from VLE project website.")
  }
  
  message("All required packages installed and loaded successfully.")
}

#' Check for SUNFLO model file
check_sunflo_model <- function() {
  if (!file.exists("sunflo_web.vpz")) {
    stop("SUNFLO model file 'sunflo_web.vpz' not found. Please download from RECORD project.")
  }
  message("SUNFLO model file found.")
}

# Model Setup and Execution ---------------------------------------------------

#' Initialize SUNFLO model
#' @param model_path Path to the SUNFLO model file
#' @return Rvle object
initialize_sunflo <- function(model_path = "sunflo_web.vpz") {
  tryCatch({
    sunflo <- new("Rvle", file = model_path, pkg = "sunflo")
    message("SUNFLO model initialized successfully.")
    return(sunflo)
  }, error = function(e) {
    stop("Failed to initialize SUNFLO model: ", e$message)
  })
}

#' Run single simulation
#' @param design Design of experiments
#' @param unit_id Unit ID to simulate
#' @return Simulation results
run_single_simulation <- function(design, unit_id) {
  tryCatch({
    results <- design %>% 
      play(unit = unit_id) %>% 
      shape(view = "timed")
    return(results)
  }, error = function(e) {
    warning("Simulation failed for unit ", unit_id, ": ", e$message)
    return(NULL)
  })
}

#' Run multiple simulations in parallel
#' @param design Design of experiments
#' @param n_cores Number of CPU cores to use (default: 4)
#' @return Data frame with simulation results
run_parallel_simulations <- function(design, n_cores = 4) {
  if (!requireNamespace("doMC", quietly = TRUE)) {
    install.packages("doMC")
  }
  library(doMC)
  registerDoMC(cores = n_cores)
  
  list_simulation <- design %>% select(id)
  
  output <- list_simulation %>%
    plyr::mdply(possibly(run_single_simulation, NULL), design = design) %>% 
    as_tibble()
  
  return(output)
}

# Results Analysis -----------------------------------------------------------

#' Calculate crop performance indicators
#' @param simulation_results Simulation results data frame
#' @return Data frame with performance indicators
calculate_indicators <- function(simulation_results) {
  output_indicators <- simulation_results %>% 
    group_by(id) %>% 
    do(indicate(.)) %>%
    ungroup()
  
  return(output_indicators)
}

#' Plot simulation results
#' @param results Simulation results
#' @param indicator Indicator to plot
plot_results <- function(results, indicator) {
  ggplot(results, aes(x = id, y = !!sym(indicator))) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(title = paste("SUNFLO Simulation Results -", indicator),
         x = "Simulation ID",
         y = indicator)
}

# Main Execution ------------------------------------------------------------

main <- function() {
  # Install and check requirements
  install_required_packages()
  check_sunflo_model()
  
  # Load design of experiments
  design <- readRDS("inst/doc/files/design.rds")
  
  # Initialize model
  sunflo <- initialize_sunflo()
  
  # Run default simulation
  default_results <- sunflo %>% 
    run() %>% 
    results() %>% 
    shape(view = "timed")
  
  # Display default results
  display(default_results)
  
  # Run parallel simulations
  simulation_results <- run_parallel_simulations(design)
  
  # Calculate indicators
  performance_indicators <- calculate_indicators(simulation_results)
  
  # Plot results (example with yield indicator)
  if ("yield" %in% names(performance_indicators)) {
    p <- plot_results(performance_indicators, "yield")
    print(p)
  }
  
  return(list(
    default_results = default_results,
    simulation_results = simulation_results,
    performance_indicators = performance_indicators
  ))
}

# Execute main function if script is run directly
if (!interactive()) {
  results <- main()
}
