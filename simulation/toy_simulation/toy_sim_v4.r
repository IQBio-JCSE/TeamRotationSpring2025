#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' v3
#' 
# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(tidyverse)
library(ggplot2) # plotting
library(parallel)  # parallelization

# Load custom functions --------------------------------------------------------
# source("functions.R") # Load helper functions

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

# climate data ----------------------------------------------------------------
# georgia_climate <- rbind(read.csv("climate_data/Georgia_clean_90_00.csv"),
#                          read.csv("climate_data/Georgia_clean_14_24.csv"))
# minnesota_climate <- rbind(read.csv("climate_data/Minnesota_clean_90_00.csv"),
#                            read.csv("climate_data/Minnesota_clean_14_24.csv"))

georgia_climate_p1 <- read.csv("climate_data/Georgia_clean_90_00.csv")
georgia_climate_p2 <- read.csv("climate_data/Georgia_clean_14_24.csv")
minnesota_climate_p1 <- read.csv("climate_data/Minnesota_clean_90_00.csv")
minnesota_climate_p2 <- read.csv("climate_data/Minnesota_clean_14_24.csv")

georgia_climate <- rbind(georgia_climate_p1,georgia_climate_p2)%>%
  mutate(Location = "Georgia")
minnesota_climate <- rbind(minnesota_climate_p1, minnesota_climate_p2) %>%
  mutate(Location = "Minnesota")


climate_data <- rbind(georgia_climate,minnesota_climate) %>%
  mutate(Year = ifelse(Year < 50, Year + 2000, Year + 1900))  %>%
  
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"))%>%
  mutate(ID = paste(Location, Date, sep = "_"))  %>%
 filter(Month %in% 03:12)  # Filter for growing season (April to October)



# climate_data <- data.table(
#   study_period = 'A',
#   year = rep(1:years, each = 150),
#   day = rep(1:150, times = years),
#   temperature = rnorm(years * 150, mean = 20, sd = 5), # Random temperature data
#   rainfall = rnorm(years * 150, mean = 100, sd = 20) # Random rainfall data
# )
# # unique ID for each study period, year & day
# climate_data[, ID := paste(study_period, year, day, sep = "_")]
# 
# # Plot temperature by ID
# ggplot(climate_data, aes(x = ID, y = temperature, group = year, color = as.factor(year))) +
#   geom_line() +
#   geom_vline(
#     xintercept = cumsum(rep(150, years))[-length(cumsum(rep(150, years)))], # Add vertical lines at year boundaries
#     linetype = "dashed", color = "black", alpha = 0.5
#   ) +
#   labs(
#     title = "Temperature by Day",
#     x = "Day",
#     y = "Temperature (°C)",
#     color = "Year"
#   )+
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     panel.grid.major = element_blank(),  # Removes grid lines
#     panel.grid.minor = element_blank()
#     # axis.ticks.x = element_blank(),
#     # panel.grid.major.x = element_blank(),  # Removes major vertical grid lines
#     # panel.grid.major.y = element_blank()   # Removes major horizontal grid lines
#   )
  

# Plot temperature by ID
ggplot(climate_data[climate_data$Year==2000,], aes(x = ID, y = TMAX/10, group = Location, color = as.factor(Location))) +
  geom_line() +
  # geom_smooth()
  # geom_vline(
  #   xintercept = cumsum(rep(150, years))[-length(cumsum(rep(150, years)))], # Add vertical lines at year boundaries
  #   linetype = "dashed", color = "black", alpha = 0.5
  # ) +
  labs(
    # title = "Temperature by Day",
    x = "Day",
    y = "Temperature (°C)",
    color = "Time"
  )+
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 2),  # Rotate x-axis text by 45 degrees
    
    panel.grid.major = element_blank(),  # Removes grid lines
    panel.grid.minor = element_blank()
    # axis.ticks.x = element_blank(),
    # panel.grid.major.x = element_blank(),  # Removes major vertical grid lines
    # panel.grid.major.y = element_blank()   # Removes major horizontal grid lines
  )


# Group data by 7-day intervals and calculate the average TMAX
climate_data_weekly <- climate_data %>%
  group_by(Location, Year) %>%  # Group by Year, Location, and Week
  mutate(Week = ceiling(Day / 7)) %>%  # Create a Week column (1 = days 1-7, 2 = days 8-14, etc.)
  mutate(Loc_Year_Wk = paste(Location, Year, Week, sep = '_')) %>%
  ungroup()  %>%
  group_by(Location, Year, Week, Loc_Year_Wk)  %>%
  summarize(Avg_TMAX = mean(TMAX, na.rm = TRUE))  # Calculate average TMAX for each week

# Plot the smoothed line
ggplot(climate_data_weekly, aes(x = Loc_Year_Wk, y = Avg_TMAX, group = Location, color = Location)) +
  geom_line() +
  labs(
    title = "Weekly Average TMAX",
    x = "Week",
    y = "Average Temperature (°C)",
    color = "Location"
  ) +
  theme_minimal()



# plot temperature & precipitation across    
plot(climate_data$day[climate_data$year==2], climate_data$temperature[climate_data$year==2], type = "l", col = "#d09220", lwd = 2,
     xlab = "Days", ylab = "Temperature (Cº)",
     main = "Temperature Across Study Period")
# TODO: add precip & add all study years

# plot precipitation across    
plot(climate_data$day[climate_data$year==2], climate_data$rainfall[climate_data$year==2], type = "l", col = "darkblue", lwd = 2,
     xlab = "Days", ylab = "Precipitation (mm)",
     main = "Precipitation Across Study Period")
# TODO: accumulation




# pest pressure ---------------------------------------------------------------
# aphid_pest_pressure <- function(temp, Tmin = 5, Tmax = 35, a = 0.0002) {
#   # Brière-based growth rate
#   growth_rate <- ifelse(
#     temp > Tmin & temp < Tmax,
#     a * temp * (temp - Tmin) * sqrt(Tmax - temp),
#     0
#   )
#   # Normalize to 0–1 scale for pest pressure
#   pressure <- growth_rate / max(growth_rate, na.rm = TRUE)
#   return(pressure)
# }


# pest pressure ---------------------------------------------------------------
aphid_pest_pressure <- function(temp, Tmin = 5, Tmax = 35, a = 0.0002) {
  # Brière-based growth rate
  aphid_growth_rate <- ifelse(
    temp > Tmin & temp < Tmax,
    a * temp * (temp - Tmin) * sqrt(Tmax - temp),
    0
  )
  # Normalize to 0–1 scale for pest pressure
  pressure <- 1 - (aphid_growth_rate / max(aphid_growth_rate, na.rm = TRUE))
                  return(pressure)
}


# pest pressure loop
for (year in 1:years) {
#   initialize population
    # trait_expression

    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure<- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, day = day, pressure = pest_pressure))
    }
    # # plot pest pressure over time
plot(pest_pressure_data$day[pest_pressure_data$year==2], pest_pressure_data$pressure[pest_pressure_data$year==2], type = "l", col = "#d09220", lwd = 2,
     xlab = "Days", ylab = "Pest Pressure (0–1)",
     main = "Aphid Pest Pressure Across Study Period")

}




# initialize populations




for (year in 1:years) {
#   initialize population

  for (plot in 1:plots) { # for each plot population
    
    # trait_expression

    for (day in 1:150) { # for each day in growing season
    # pest pressure
    pest_pressure <- aphid_pest_pressure(climate_data[year == year & day == day, temperature])
    # store pest pressure
    # pest_pressure_data <- rbind(pest_pressure_data, data.table(year = year, day = day, pressure = pest_pressure))
    
    
    # growth: base growth rate, temperture,

    # death related to intrinisic propbability of death 

    }
    # plot yield

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


