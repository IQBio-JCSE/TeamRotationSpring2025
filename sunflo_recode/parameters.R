# load parameters

#' @title Parameters for Sunflower Recode
#' @description This script loads the parameters for the sunflower recode project.
#' @import readxl


# load required package
library(readxl)

# set file directory
file_directory <- "rsunflo R package/inst/doc/files"

# load parameter excel file tab
parameter_design <- read_excel(file.path(file_directory, "parameterization_default.xlsx"),
                                sheet = "design_metadata"
                               )

# convert to data frame
parameter_design <- as.data.frame(parameter_design)