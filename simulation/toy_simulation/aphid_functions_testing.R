#' Predict aphid population growth rate based on temperature
#' 
#' @param temp Vector of daily temperature values (°C)
#' @param Tmin Minimum temperature threshold for development (°C)
#' @param Tmax Maximum temperature threshold for development (°C)
#' @param a Fitted coefficient for the Brière model
#' @return Vector of intrinsic growth rates for each temperature value

aphid_growth_rate <- function(temp, Tmin = 5, Tmax = 35, a = 0.0002) {
  # Apply the Brière model
  growth_rate <- ifelse(
    temp > Tmin & temp < Tmax,
    a * temp * (temp - Tmin) * sqrt(Tmax - temp),
    0
  )
  return(growth_rate)
}

# Daily temperature data (°C)
temps <- seq(0, 40, by = 1)

# Get predicted growth rates
rates <- aphid_growth_rate(temps)

# Plot the temperature-performance curve
plot(temps, rates, type = "l", col = "darkgreen", lwd = 2,
     xlab = "Temperature (°C)", ylab = "Intrinsic growth rate",
     main = "Temperature-Dependent Aphid Growth Rate")


# Estimate aphid population index based on temperature
estimate_aphid_population <- function(temp_vec, baseline_pop = 100) {
  # Define temperature-response function based on literature (Gaussian curve)
  # Peak growth around 23°C, poor growth <15 or >30
  temp_response <- function(T) {
    if (is.na(T)) return(0)
    # Gaussian-shaped growth response
    opt_temp <- 23
    sd_temp <- 4.5  # Controls breadth of optimal range
    growth_factor <- exp(-((T - opt_temp)^2) / (2 * sd_temp^2))
    
    # Suppress values outside survival range
    if (T < 10 || T > 33) return(0)
    
    return(growth_factor)
  }
  
  # Apply temperature response function to each day
  daily_growth_rates <- sapply(temp_vec, temp_response)
  
  # Assume population multiplies by (1 + daily_growth_rate * rate_scale)
  rate_scale <- 0.15  # Controls how fast population grows per day
  population_over_time <- numeric(length(temp_vec))
  population_over_time[1] <- baseline_pop
  
  for (i in 2:length(temp_vec)) {
    population_over_time[i] <- population_over_time[i - 1] * (1 + daily_growth_rates[i] * rate_scale)
  }
  
  return(data.frame(Day = seq_along(temp_vec),
                    Temperature = temp_vec,
                    GrowthFactor = daily_growth_rates,
                    Population = population_over_time))
}

# Simulate 30 days of temperature data
set.seed(123)
temps <- rnorm(30, mean = 22, sd = 3)

# Run model
aphid_sim <- estimate_aphid_population(temps)

# Plot
library(ggplot2)
ggplot(aphid_sim, aes(x = Day, y = Population)) +
  geom_line(color = "darkgreen") +
  labs(title = "Simulated Aphid Population Based on Temperature",
       y = "Aphid Population", x = "Day") +
  theme_minimal()



#' Estimate aphid pest pressure based on temperature
#'
#' @param temp Numeric vector of daily temperatures (°C)
#' @param Tmin Minimum temperature threshold for activity
#' @param Tmax Maximum temperature threshold for activity
#' @param a Brière model coefficient
#' @return A vector of pest pressure values (scaled 0 to 1)


# The Brière model is a nonlinear temperature-dependent growth model that describes how the development rate of an ectothermic organism (like an insect) changes with temperature. It’s widely used in entomology and ecology because it captures the asymmetric, bell-shaped curve of biological performance versus temperature.


aphid_pest_pressure <- function(temp, Tmin = 5, Tmax = 35, a = 0.0002) {
  # Brière-based growth rate
  growth_rate <- ifelse(
    temp > Tmin & temp < Tmax,
    a * temp * (temp - Tmin) * sqrt(Tmax - temp),
    0
  )
  # Normalize to 0–1 scale for pest pressure
  pressure <- growth_rate / max(growth_rate, na.rm = TRUE)
  return(pressure)
}
temps <- seq(0, 40, by = 1)
pressure <- aphid_pest_pressure(temps)

plot(temps, pressure, type = "l", col = "red", lwd = 2,
     xlab = "Temperature (°C)", ylab = "Pest Pressure (0–1)",
     main = "Temperature-Driven Aphid Pest Pressure")


