#' Population simulation of wild vs domesticated sunflower plants with different levels of secondary metabolite expression
#' Sarah Elizabeth Stockman
#' April 2025
#' full version
#' 
# Load libraries --------------------------------------------------------------
library(data.table) # data manipulation
library(tidyverse)
# install.packages("segmented")  # Install if not already installed
# library(segmented)


# set run location & study period

location <- "Georgia" # "Minnesota" "Georgia"# change location
study_period <- 2 # 1, 2
gamma <- 10 #5, 10, 20, 50,100

# location <- c("Georgia","Minnesota") # "Minnesota" "Georgia"# change location
# study_period <- c(1,2) # 1, 2
# gamma <- c(5, 10, 20, 50,100)

# Load functions --------------------------------------------------------
source("simulation/Population_Simulation/Functions.r") # Load helper functions

# Load sunflo model/function
source("sunflo_recode/run_model.R") # Load sunflower model

# climate data
climate_data <- read.csv("climate_data/Climate_data.csv")

# which(is.na(climate_data$TMAX))

# trait values
source("simulation/Population_Simulation/Population_Gentic_Traits.R")

# Set seed for reproducibility -----------------------------------------------
set.seed(123)

# Define parameters -----------------------------------------------------------
# years <- 2 # Number of years to simulate
years <- length(unique(climate_data$Year[climate_data$StudyPeriod==2]))

# number of plots of populations
plots <- 10

# patch of plants 7.0 plants per m^2 for a field plot of 5 acres
# 5 acres = 20234.3 m^2
plants_per_5_acres <- 7.0 * 20234.3  # plants per 5 acre
# initial populations of wild and domesticated plants assuming each plot is 1 acre
plants_per_acre <- 7.0 * 4046.86

# number of plants per plot
wild_n_individuals <- round(plants_per_acre) * plots # Total number of wild individuals
domesticated_n_individuals <- round(plants_per_acre) * plots # Total number of domesticated individuals

# selection proportion
selection_proportion <- 0.2
n_selected <- ceiling(plots * selection_proportion)


# Graphs ----------------------------------------------------------------------
color_options <- c('#F4E9CB',"#F1DBA9","#F4E9CB","#F2D590",
                   "#000000","#FEFEFE","#F1A692",
                   "#814627","#834C28","#663333",
                   '#DA9D20',"#EEB612","#DA9D20",
                   "#AB5300","#AE5524","#AF5726",
                   "#AB7836","#63601C","#818511")


# mortality function ---------------------------------------------------------
# mortality <- function(population, max_temp, precip, pest_pressure) {
#   with(population, {
#     climate_stress <- 0.1 * ((max_temp - 35)/5)^2 + 0.1 * ((300 - precip)/50)^2
#     herbivory <- pest_pressure * (1 - resistance)
#     intrinsic <- ifelse(genotype == "domesticated", 0.05, 0.02)
#     
#     total_mortality <- 1 - ((1 - intrinsic) * (1 - herbivory) * exp(-climate_stress))
#     survivors <- population[runif(nrow(population)) > total_mortality, ]
#     return(survivors)
#   })
# }

# mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
#   climate_stress <- (max_temp/10 > 30) + (precip < 300)
#   herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
#   1/3/nrow(climate_data_year) + 0.1 * climate_stress + herbivory
# }

# mortality <- function(population, max_temp, precip, pest_pressure) {
#   with(population, {
#     climate_stress <- 0.1 * ((max_temp - 35)/5)^2 + 0.1 * ((300 - precip)/50)^2
#     herbivory <- pest_pressure * (1 - resistance)
#     intrinsic <- ifelse(genotype == "domesticated", 0.05, 0.02)
#     
#     total_mortality <- 1 - ((1 - intrinsic) * (1 - herbivory) * exp(-climate_stress))
#     survivors <- population[runif(nrow(population)) > total_mortality, ]
#     return(survivors)
#   })
# }
# 
# 
# # Combine effects
# total_mortality <- base_mortality + climate_stress + herbivory
# total_mortality <- min(total_mortality, 1)  # cap at 100%
# 
# return(total_mortality)



# 
# 
# sunflower_mortality <- function(age, temp, precip, pest, resistance, is_domesticated) {
#   base_mortality <- ifelse(is_domesticated, 0.05, 0.02)
#   climate_penalty <- 0.1 * ((temp - 30)^2 / 25 + (300 - precip)^2 / 2500)
#   herbivory_penalty <- pest * (1 - resistance) * (ifelse(is_domesticated, 1.2, 0.8))
#   total_mortality <- base_mortality + climate_penalty + herbivory_penalty
#   return(min(total_mortality, 1))  # cap at 100% mortality
# }

# herbivory <- function(pest_pressure, trait_value){
#   base_mortality <- 1/3/nrow(climate_data_year)
#   herbivory <- pest_pressure * (1 - trait_value)
#   total_mortality <- base_mortality + herbivory
#   return(min(total_mortality, 1))  # cap at 100% mortality
# }
# 
# mortality_function <- function(max_temp, precip, pest_pressure, secondary_metabolite, is_domesticated = FALSE) {
#   climate_stress <- (max_temp> 30) + (precip < 300)
#   herbivory <- pest_pressure * (if (is_domesticated) 1.2 else 0.8) * (1 - secondary_metabolite)
#   return(1/3/nrow(climate_data_year) + 0.1 * climate_stress + herbivory)
# 
# }






# data storage ----------------------------------------------------------------
pest_pressure_data <- data.table(
  location = character(),
  year = integer(),
  day = integer(),
  pressure = numeric()
)

wild_plot_yield_vector <- numeric(plots) # Placeholder for yield values for each plot 
domesticated_plot_yield_vector <- numeric(plots) # Placeholder for yield values for each plot 

domesticated_mortality <- numeric()
wild_mortality <- numeric()


plot_trait_values <- data.table(
  plot = integer(),
  group = character(),
  trait_values = numeric()
)



results <- data.table(
  location = character(),
  year = integer(),
  plot = integer(),
  group = character(), 
  yield = integer(),
  trait_mean = numeric(),
  trait_sd = numeric(),
  upper_bound = numeric(),
  lower_bound = numeric()
  )


# Initialize -------------------------------------------------------------------


# Initialize trait expression for wild and domesticated populations
wild_trait_expression_year <- rnorm(wild_n_individuals, mean = wild_median, sd = wild_sd) # Wild trait expression
summary(wild_trait_expression_year)
domesticated_trait_expression_year <- rnorm(domesticated_n_individuals, mean = domesticated_median, sd = domesticated_sd) # Domesticated trait expression
summary(domesticated_trait_expression_year)

# plot intial trait distribution
hist(wild_trait_expression_year, main = 'Min-Max Normalized Wild Trait Distribution')
hist(domesticated_trait_expression_year, main = 'Min-Max Normalized Domesticated Trait Distribution')

wild_trait_expression_year <- pmax(0, pmin(1, wild_trait_expression_year))
domesticated_trait_expression_year <- pmax(0, pmin(1, domesticated_trait_expression_year))

hist(wild_trait_expression_year, main = 'Bounded [0,1] Wild Trait Distribution')
hist(domesticated_trait_expression_year, main = 'Bounded [0,1] Domesticated Trait Distribution')




# Main simulation loop --------------------------------------------------------

# Loop through each year
for (year in 1:years) { # TODO: length(unique(climate_data$year)) # for each year in climate data
  # Filter climate data for the current year
  
  climate_data_year <- climate_data %>%
    filter(Year == unique(climate_data$Year[climate_data$StudyPeriod==study_period])[year], 
           Location == location, 
           StudyPeriod == study_period)
  
  # pest pressure
  pest_pressure <- aphid_pest_pressure(climate_data_year$Temp_mean)
  plot(pest_pressure, 
       pch = 20,
       col = '#EEB612',
       # ylab = expression("Grain Yield" ~ (q.ha^-1)),
       ylab = expression("Pest Pressure [0,1]"),
       # ylab = expression("Grain Yield (quintals per hectare)"),
       xlab = 'Day of Growing Season', 
       main = sprintf('Pest Pressure for Year: %d',
                      unique(climate_data$Year[climate_data$StudyPeriod==study_period])[year])
       
  ) 
  
  
  # # store pest pressure
  # pest_pressure_data <- rbind(pest_pressure_data, 
  #                             data.table(year = year, 
  #                                        plot = plot, 
  #                                        day = day, 
  #                                        pressure = pest_pressure))
  
  
  
  for (plot in 1:plots) { # for each plot population
    # Initialize wild and domesticated populations for each plot
    wild_trait_expression_year_plot <- sample(wild_trait_expression_year, 
                                              size = round(plants_per_acre), 
                                              replace = FALSE)
    
    domesticated_trait_expression_year_plot <- sample(domesticated_trait_expression_year, 
                                                      size = round(plants_per_acre), 
                                                      replace = FALSE)
    
    
    
    
    # Append results to the tracking data.table
    plot_trait_values <- rbindlist(list(
      plot_trait_values,
      data.table(
        plot = plot,
        group = "Wild",
        trait_values = wild_trait_expression_year_plot
      ),
      data.table(
        plot = plot,
        group = "Domesticated",
        trait_values = domesticated_trait_expression_year_plot
      )
    ), fill = TRUE)
    
    
    
    
    
    
    # TODO: plot yield not start at 0 for wild populations
    
    # trait_expression <-- need ONE value for sunflo
    wild_trait_mean_expression <- mean(wild_trait_expression_year_plot)
    wild_trait_sd_plot <- sd(wild_trait_expression_year_plot)
    domesticated_trait_mean_expression <- mean(domesticated_trait_expression_year_plot)
    domesticated_trait_sd_plot <- sd(domesticated_trait_expression_year_plot)
    
    # sunflo ###################################################################
    wild_sunflo <- run_sunflo(climate_data_year,
                              wild_trait_mean_expression, # wild_trait_expression or domesticated_trait_expression
                              '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode')%>%
      mutate(GrowingSeasonDay = row_number(),
             DailyYield = c(CropYield[1], diff(CropYield)))

    # Apply herbivory_mortality function to each pest pressure value
    wild_herbivory_mortality <- sapply(pest_pressure, function(p) {
      herbivory_mortality(pest_pressure = p, resistance = wild_trait_mean_expression, gamma = gamma)
    })
    # TODO add mortality effect to yield
    
    wild_plot_yield <- max(wild_sunflo$CropYield)
    wild_plot_yield_with_pest <- sum(wild_herbivory_mortality * wild_sunflo$DailyYield)
    wild_plot_yield_vector[plot] <- wild_plot_yield_with_pest # Placeholder for yield values for each plot 
    
    # TODO sunflo yeild units
    # 1 q.ha⁻¹ = 100 kg/ha
    
    # tiff(sprintf("simulation/results/Yield_%s_Wild_%d_plot%d.png",
    #              location,
    #              unique(climate_data$Year)[year], 
    #              plot), 
    #      width = 1000,  # Increase width
    #      height = 800,  
    #      res = 400, 
    #      bg = "transparent",
    #      pointsize = 5,
    #      compression = 'none')
    # plot(wild_sunflo$GrowingSeasonDay, wild_sunflo$CropYield, 
    #      pch = 20,
    #      col = '#DA9D20',
    #      # ylab = expression("Grain Yield" ~ (q.ha^-1)),
    #      ylab = expression("Grain Yield (q/ha)"),
    #      # ylab = expression("Grain Yield (quintals per hectare)"),
    #      xlab = 'Day of Growing Season', 
    #      main = sprintf('Wild Grain Yield: %g Year: %d, Plot: %d, Trait: %g', 
    #                     round(max(wild_sunflo$CropYield), digits = 2),
    #                     unique(climate_data$Year)[year], 
    #                     plot, 
    #                     round(wild_trait_mean_expression, digits = 3))
    #     ) 
    # dev.off() 
    
    
    domesticated_sunflo <- run_sunflo(climate_data_year,
                                      domesticated_trait_mean_expression, # wild_trait_expression or domesticated_trait_expression
                                      '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode') %>%
      mutate(GrowingSeasonDay = row_number(),
             DailyYield = c(CropYield[1], diff(CropYield)))
    
    # Apply herbivory_mortality function to each pest pressure value
    domesticated_herbivory_mortality <- sapply(pest_pressure, function(p) {
      herbivory_mortality(pest_pressure = p, resistance = domesticated_trait_mean_expression, gamma = gamma)
    })
    
    domesticated_plot_yield <- max(domesticated_sunflo$CropYield)
    domesticated_plot_yield_with_pest <- sum(domesticated_herbivory_mortality * domesticated_sunflo$DailyYield)
    
    domesticated_plot_yield_vector[plot] <- domesticated_plot_yield_with_pest 

    # Open PNG device
    # png(sprintf("simulation/results/Yield_%s_Domesticated_%d_plot%d.png",
    #                 location,
    #                 unique(climate_data$Year)[year], 
    #                 plot), 
    #     width = 700, 
    #     height = 500,
    #     bg = "transparent")  
    
    # tiff(sprintf("simulation/results/Yield_%s_Domesticated_%d_plot%d.png",
    #          location,
    #          unique(climate_data$Year)[year], 
    #          plot), 
    #  width = 1000,  # Increase width
    #  height = 800,  
    #  res = 400, 
    #  bg = "transparent",
    #  pointsize = 5,
    #  compression = 'none')
    # 
    # plot(domesticated_sunflo$GrowingSeasonDay, domesticated_sunflo$DailyYield, # CropYield
    #      pch = 20,
    #      size = .5,
    # 
    #      col = "#814627",
    #      # ylab = expression("Grain Yield" ~ (q.ha^-1)),
    #      ylab = expression("Grain Yield (q/ha)"),
    #      # ylab = expression("Grain Yield (quintals per hectare)"),
    #      xlab = 'Day of Growing Season',
    #      main = sprintf('Domesticated Yield:%g, Year: %d, Plot: %d, Trait: %g',
    #                     round(max(domesticated_sunflo$CropYield), digits = 2),
    #                     unique(climate_data$Year)[year],
    #                     plot,
    #                     round(domesticated_trait_mean_expression, digits = 3)))
    # dev.off() 
    
    
    # TODO herbivory
    
    # for (day in 1:nrow(climate_data_year)) { # for each day in growing season
    # 
    #   # death related to intrinsic probability of death
    #   wild_mortality <- mortality_function(climate_data_year$TMAX[day], climate_data_year$PRCP[day], pest_pressure, wild_trait_mean_expression )
    #   plot(wild_mortality,
    #        pch = 20,
    #        col = '#DA9D20',
    #        ylim = c(0,1.4),
    #        ylab = 'mortality',
    #        xlab = 'growing season days',
    #        main = 'mortality across growing season for wild population'
    #   )
    # 
    #   domesticated_mortality <-  mortality_function(max_temp = climate_data_year$TMAX[day], 
    #                                                 precip= climate_data_year$PRCP[day], 
    #                                                 pest_pressure = pest_pressure[day], 
    #                                                 secondary_metabolite = domesticated_trait_mean_expression, 
    #                                                 is_domesticated = TRUE )
    #   plot(domesticated_mortality,
    #        pch = 20,
    #        col = "#814627",
    #        ylim = c(0,1.4),
    #        ylab = 'mortality',
    #        xlab = 'growing season days',
    #        main = 'mortality across growing season for domesticated population'
    #   )
    # }



    # Append results to the tracking data.table
    results <- rbindlist(list(
      results,
      data.table(
        location = location,
        year = unique(climate_data$Year[climate_data$StudyPeriod==study_period])[year],
        plot = plot,
        group = "Wild Population",
        yield = wild_plot_yield_with_pest,
        trait_mean = wild_trait_mean_expression,
        trait_sd = wild_trait_sd_plot,
        upper_bound = wild_trait_mean_expression+wild_trait_sd_plot,
        lower_bound = wild_trait_mean_expression-wild_trait_sd_plot
      ),
      data.table(
        location = location,
        year = unique(climate_data$Year[climate_data$StudyPeriod==study_period])[year],
        plot = plot,
        group = "Domesticated Population",
        yield = domesticated_plot_yield_with_pest,
        trait_mean = domesticated_trait_mean_expression,
        trait_sd = domesticated_trait_sd_plot,
        upper_bound = domesticated_trait_mean_expression+domesticated_trait_sd_plot,
        lower_bound = domesticated_trait_mean_expression-domesticated_trait_sd_plot
      )
    ), fill = TRUE)



  }
  
  
  # set trait expression values for next year
  
  # Select top plots based on yield value for each group
  wild_selected_plots <- order(wild_plot_yield_vector, decreasing = TRUE)[1:n_selected]  # plot numbers for wild
  domesticated_selected_plots <- order(domesticated_plot_yield_vector, decreasing = TRUE)[1:n_selected]  # Get plot numbers for domesticated
  
  
  
  # new plots inherit average of selected plots trait
  
  # Ensure the correct subsetting of plot_trait_values
  wild_trait_expression_year <- rnorm(
    wild_n_individuals,
    mean = mean(plot_trait_values[group == 'Wild' & plot %in% wild_selected_plots, trait_values]),
    sd = sd(plot_trait_values[group == 'Wild' & plot %in% wild_selected_plots, trait_values])
  )
  
  domesticated_trait_expression_year <- rnorm(
    domesticated_n_individuals,
    mean = mean(plot_trait_values[group == 'Domesticated' & plot %in% domesticated_selected_plots, trait_values]),
    sd = sd(plot_trait_values[group == 'Domesticated' & plot %in% domesticated_selected_plots, trait_values])
  )
  
  
  
  
  
  # wild_trait_expression_year <- rnorm(wild_n_individuals,
  #                                     mean = mean(plot_trait_values$trait_values[plot %in% wild_selected_plots & group == 'Wild']), 
  #                                     sd = sd(plot_trait_values$trait_values[plot %in% wild_selected_plots]))
  wild_trait_expression_year <- pmax(0, pmin(1, wild_trait_expression_year)) # Ensure values within [0, 1]
  # domesticated_trait_expression_year <- rnorm(domesticated_n_individuals,
  #                                             mean = mean(plot_trait_values$trait_values[plot %in% domesticated_selected_plots & group == 'Domesticated']),
  #                                             sd = sd(plot_trait_values$trait_values[plot %in% domesticated_selected_plots & group == 'Domesticated']) )
  domesticated_trait_expression_year <- pmax(0, pmin(1, domesticated_trait_expression_year)) # Ensure values within [0, 1]
  

  # # Initialize trait expression for wild and domesticated populations
  # wild_trait_expression_year <- rnorm(wild_n_individuals, mean = selected_wild_median, sd = selected_wild_sd) # Wild trait expression
  # # summary(wild_trait_expression_year)
  # domesticated_trait_expression_year <- rnorm(domesticated_n_individuals, mean = selected_domesticated_median, sd = selected_domesticated_sd) # Domesticated trait expression
  # # summary(domesticated_trait_expression_year)
  # wild_trait_expression_year <- pmax(0, pmin(1, wild_trait_expression_year))
  # domesticated_trait_expression_year <- pmax(0, pmin(1, domesticated_trait_expression_year))
  # 
  # 
  plot_trait_values <- data.table(
    plot = integer(),
    group = character(),
    trait_values = numeric()
  )

  
}

# Plot results
results_graph <- ggplot(results, aes(x = year)) +
  geom_line(aes(y = yield, color = group), linewidth = 1.2, alpha = .8) +
  geom_line(aes(y = trait_mean * max(results$yield) / max(results$trait_mean), color = group), linewidth = 1.2, alpha =.8, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower_bound * max(results$yield) / max(results$trait_mean),
                  ymax = upper_bound * max(results$yield) / max(results$trait_mean),
                  fill = group), alpha = 0.1) +
  scale_y_continuous(
    name = "Yield Per Acre",
    sec.axis = sec_axis(~ . * max(results$trait_mean) / max(results$yield),
                        name = "Terpenoid Trait Expression [0-1]")
  ) +
  scale_x_continuous(
    breaks = unique(results$year),  # Ensure only whole years are shown
    name = "Years") +
  scale_color_manual(values = c("Wild Population" = "dodgerblue3", "Domesticated Population" = "firebrick3")) +
  scale_fill_manual(values = c("Wild Population" = "dodgerblue3", "Domesticated Population" = "firebrick3")) +
labs(
  title = sprintf("%s Yield for Years %d-%d", 
                  location,
                  min(results$year),
                  max(results$year)),
  x = "Years",
  color = "Yield (q/ha)",
  fill = "Terpenoid Trait Expression"
) +
theme_minimal() +
  theme(
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

results_graph
  
# Save the plot as a PNG file
ggsave(sprintf("Yield_Trait_%s_StudyPeriod%d.png",
               location,
               study_period),
       plot = results_graph, path = 'simulation/results',
       width = 10, height = 6, dpi = 300)
