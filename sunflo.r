# Installation ----------------------------------------------------------------
# To run simulations with the SUNFLO crop model and this R package, two
# softwares (VLE & R) and two packages (sunflo & rsunflo) need to be installed.
# install the VLE simulation platform : VLE project github or website
# and RVLE R package github
# install the SUNFLO crop model for VLE simulation platform : RECORD project
# model library
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# install the VLE simulation platform (if not already installed)
if (!requireNamespace("rvle", quietly = TRUE)) {
  devtools::install_github("picasa/rvle") # install the RVLE package from github
}
# Load the RVLE package
library(rvle)s
# Check if VLE is installed and load it
if (!"Rvle" %in% class(rvle::get_vle())) {
  stop("The VLE simulation platform is not installed. Please install it from the VLE project github or website.")
} else {
  message("VLE simulation platform loaded successfully.")
}
# Install the SUNFLO crop model for VLE simulation platform
if (!file.exists("sunflo_web.vpz")) {
  # Download the SUNFLO model from the RECORD project (or your source)
  # This is a placeholder, replace with actual download code or instructions
  stop("The SUNFLO model file 'sunflo_web.vpz' is missing. Please download it from the RECORD project.")
} else {
  message("SUNFLO model file found.")
}
# Note: The above code assumes you have the 'sunflo_web.vpz' file in your working directory.
# Install the rsunflo package from GitHub
if (!requireNamespace("rsunflo", quietly = TRUE)) {
  # Install the rsunflo package from GitHub
  devtools::install_github("picasa/rsunflo")
} else {
  message("rsunflo package already installed.")
}
# # In case you want to ensure the latest version of rsunflo, you can reinstall it
# if (requireNamespace("rsunflo", quietly = TRUE)) {
#   # Reinstall to ensure the latest version
#   message("Reinstalling rsunflo to ensure the latest version.")
#   devtools::install_github("picasa/rsunflo")
# } else {
#   message("rsunflo installation was successful.")
# }
# install the development version of rsunflo or clone the rsunflo repository
install.packages("devtools")
devtools::install_github("picasa/rsunflo")


# Load packages ---------------------------------------------------------------
library(tidyverse)
library(rvle)
library(rsunflo)



# Simulation workflow ---------------------------------------------------------
# load design of experiments
design <- readRDS("inst/doc/files/design.rds")

# load the sunflo model
sunflo <- new("Rvle", file = "sunflo_web.vpz", pkg = "sunflo")
run_sunflo <- function(id){design %>% play(unit=id) %>% shape(view="timed")}

# run the model with default parameterization 
sunflo %>% run() %>% results() %>% shape(view="timed") %>% display()

# run the model with parameters defined in the design of experiments
design %>% play(unit=1) %>% shape(view="timed") %>% display()

# multi-simulation (4 core i7 : 71 ms/sim), for larger design, parallelization is possible with the *doMC* package
list_simulation <- design %>% select(id) 

output <- list_simulation %>%
  plyr::mdply(possibly(run_sunflo, NULL)) %>% as_tibble() 

# compute crop performance indicator (one experiment -> one vector)
output_indicators <- output %>% group_by(id) %>% do(indicate(.))