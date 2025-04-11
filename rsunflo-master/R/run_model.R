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
source("All_Rsunflo_Files.R")

result <- play(data = design, model = sunflo, unit = 1)
formatted_result <- shape(result, view = "generic")
indicators <- indicate(formatted_result, integration = "crop")
display(formatted_result, view = "timed")
results <- lapply(1:nrow(design), function(unit) play(data = design, model = sunflo, unit = unit))

