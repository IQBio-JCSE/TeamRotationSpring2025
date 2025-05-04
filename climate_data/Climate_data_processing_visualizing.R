# Climate data


# climate data ----------------------------------------------------------------
georgia_climate <- rbind(read.csv("climate_data/Georgia_clean_90_00.csv"),
                         read.csv("climate_data/Georgia_clean_14_24.csv"))
minnesota_climate <- rbind(read.csv("climate_data/Minnesota_clean_90_00.csv"),
                           read.csv("climate_data/Minnesota_clean_14_24.csv"))

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

  # Plot temperature by ID
ggplot(climate_data, aes(x = ID, y = TMAX/10, group = Location, color = as.factor(Location))) +
  geom_line() +
  # geom_smooth()
  # geom_vline(
  #   xintercept = cumsum(rep(150, years))[-length(cumsum(rep(150, years)))], # Add vertical lines at year boundaries
  #   linetype = "dashed", color = "black", alpha = 0.5
  # ) +
  labs(
    title = "All Years",
    x = "Day",
    y = "Max Temperature (°C)",
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