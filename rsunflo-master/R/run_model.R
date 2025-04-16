# check if the required packages are installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load the required packages
library(tidyverse)
library(lubridate)
library(testthat)
library(ggplot2)
library(dplyr)

# Load script model
source("rsunflo-master/R/All_Rsunflo_Files.R")



# Simulation

# load design of experiments
design <- readRDS("rsunflo-master/inst/doc/files/design.rds")

# load txt file with multiple tabs
climate_data <- read.table("sunflo-master/sunflo/data/AUZ_2014.txt", header = TRUE, sep = "\t")
# climate_data <- read_excel("rsunflo-master/inst/doc/files/climate_data.xlsx", sheet = "Sheet1")



# Run the sunflo model
play <- function(data, model = sunflo_model, unit) {
  # Get the design row for the specified unit
  design_row <- data %>% slice(unit)
  
  # Load climate data (replace with actual climate data loading logic)
  climate_data <- read.table("sunflo-master/sunflo/data/AUZ_2014.txt", header = TRUE, sep = "\t")
  
  # Run the model
  output <- model(design_row, climate_data)
  
  return(output)
}


# Load design of experiments
design <- readRDS("rsunflo-master/inst/doc/files/design.rds")

# Run the model for a single unit
result <- play(data = design, unit = 1)

# Print the results
print(result)








# # load the sunflo model
# sunflo <- load_model("rsunflo-master/inst/doc/files/sunflo_model.rds")

# # run the model with default parameterization
# default_result <- play(data = design, model = sunflo, unit = 1)

# # run the model with parameters defined in the design of experiments
# experiment_results <- lapply(1:nrow(design), function(unit) play(data = design, model = sunflo, unit = unit))

# # multi-simulation (4 core i7 : 71 ms/sim), for larger design, parallelization is possible with the *doMC* package
# if (requireNamespace("doMC", quietly = TRUE)) {
#   library(doMC)
#   registerDoMC(cores = parallel::detectCores())
#   parallel_results <- foreach(unit = 1:nrow(design), .combine = rbind) %dopar% {
#     play(data = design, model = sunflo, unit = unit)
#   }
# }

# # compute crop performance indicator (one experiment -> one vector)
# crop_indicators <- lapply(experiment_results, function(res) indicate(shape(res, view = "generic"), integration = "crop"))





# result <- play(data = design, model = sunflo, unit = 1)
# formatted_result <- shape(result, view = "generic")
# indicators <- indicate(formatted_result, integration = "crop")
# display(formatted_result, view = "timed")
# results <- lapply(1:nrow(design), function(unit) play(data = design, model = sunflo, unit = unit))
