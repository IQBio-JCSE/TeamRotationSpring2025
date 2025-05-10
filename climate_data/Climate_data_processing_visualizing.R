# Climate data


# climate data ----------------------------------------------------------------
# georgia_climate <- rbind(read.csv("climate_data/Georgia_clean_90_00.csv"),
#                          read.csv("climate_data/Georgia_clean_14_24.csv"))
# minnesota_climate <- rbind(read.csv("climate_data/Minnesota_clean_90_00.csv"),
#                            read.csv("climate_data/Minnesota_clean_14_24.csv"))

georgia_climate_p1 <- read.csv("climate_data/Georgia_clean_90_00.csv")%>%
  mutate(StudyPeriod = 1)

georgia_climate_p2 <- read.csv("climate_data/Georgia_clean_14_24.csv")%>%
  mutate(StudyPeriod = 2)

which(is.na(georgia_climate_p2$TMAX))

# impute missing TMAX values
# 109 32.76667
# 110 32.23334
# 113 32.5
georgia_climate_p2$TMAX[3121] <- round(32.76667 *10, 0)
georgia_climate_p2$TMAX[3122] <- round(32.23334 * 10, 1)
georgia_climate_p2$TMAX[3125] <- 32.5 *10




minnesota_climate_p1 <- read.csv("climate_data/Minnesota_clean_90_00.csv")%>%
  mutate(StudyPeriod = 1)
minnesota_climate_p2 <- read.csv("climate_data/Minnesota_clean_14_24.csv")%>%
  mutate(StudyPeriod = 2)

# minnesota_climate_p3 <- read.csv("climate_data/Minnesota_clean_98_23.csv")

# Add location column
georgia_climate <- rbind(georgia_climate_p1,georgia_climate_p2)%>%
  mutate(Location = "Georgia")
minnesota_climate <- rbind(minnesota_climate_p1, minnesota_climate_p2) %>%
  mutate(Location = "Minnesota")


# Combine climate data
climate_data <- rbind(georgia_climate,minnesota_climate) %>%
  mutate(Year = ifelse(Year < 50, Year + 2000, Year + 1900))  %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")) %>%
  mutate(ID = paste(Location, StudyPeriod, Date, sep = "_"))  %>%
  mutate(TMAX = TMAX/10) %>%
  mutate(TMIN = TMIN/10) %>%
  mutate(Temp_mean = (TMAX + TMIN)/2)  %>% 
  filter(Month %in% 04:08)  %>% # Filter for growing season (April to October)
  group_by(Year,Location) %>%  # Group by Year
  mutate(GrowingSeasonDay = row_number()) %>%  # Assign sequential numbers within each year
  ungroup() %>% # Remove grouping
  # filter GrowingSeasonDay > 150
  filter(GrowingSeasonDay <= 150) # Filter for first 150 days of growing season


# export climate data as csv
write.csv(climate_data, "climate_data/Climate_data.csv")








# plot(climate_data_year$PRCP, 
#      type= "l",
#      pch = 20,
#      col = '#5B9DD9',
#      # ylab = expression("Grain Yield" ~ (q.ha^-1)),
#      ylab = expression("Daily Precipitation (mm)"),
#      # ylab = expression("Grain Yield (quintals per hectare)"),
#      xlab = 'Day of Growing Season', 
#      main = sprintf('Climate for year: %d',
#                     unique(climate_data$Year)[year])
# ) +
#   lines(climate_data_year$Temp_mean,
#         col='#C84B1B')


# Set up the plotting area
par(mar = c(5, 4, 4, 4) + 0.1)  # Adjust margins to make space for the right y-axis

# # Plot precipitation (PRCP) on the left y-axis
# plot(climate_data_year$PRCP, 
#      type = "l",
#      col = '#5B9DD9',
#      ylab = "Daily Precipitation (mm)",  # Left y-axis label
#      xlab = "Day of Growing Season", 
#      ylim = c(0,900),
#      main = sprintf("Climate for year: %d", unique(climate_data$Year)[year]))
# 
# Plot precipitation (PRCP) on the left y-axis
plot(climate_data_year$PRCP, 
     type = "l",
     col = '#5B9DD9',  # Line color for precipitation
     ylim = c(0,900),  # Set y-axis limits for the left axis
     ylab = "",  # Suppress default y-axis label
     xlab = "Day of Growing Season", 
     main = sprintf("Climate for year: %d", unique(climate_data$Year)[year]))

# Add custom y-axis label and color for the left axis
mtext("Daily Precipitation (mm)", side = 2, line = 3, col = '#5B9DD9')  # Left y-axis label
axis(side = 2, col.axis = '#5B9DD9')  # Left y-axis numbers in blue


# Add temperature (Temp_mean) on the right y-axis
par(new = TRUE)  # Overlay a new plot on the same graph
plot(climate_data_year$Temp_mean, 
     type = "l",
     col = '#C84B1B',
     axes = FALSE,  # Suppress axes for this plot
     xlab = "",  # Suppress x-axis label
     ylab = "",  # Suppress y-axis label
     ylim = c(-5,max(climate_data_year$Temp_mean))
)

# Add the right y-axis
axis(side = 4, col.axis = '#C84B1B',)  # Add the right y-axis
mtext("Temperature (°C)", side = 4, line = 3, col = '#C84B1B')  # Label for the right y-axis























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

# weekly --------------------------------------------------------------------------------
# # Group data by 7-day intervals and calculate the average TMAX
# climate_data_weekly <- climate_data %>%
#   group_by(Location, Year) %>%  # Group by Year, Location, and Week
#   mutate(Week = ceiling(Day / 7)) %>%  # Create a Week column (1 = days 1-7, 2 = days 8-14, etc.)
#   mutate(Loc_Year_Wk = paste(Location, Year, Week, sep = '_')) %>%
#   ungroup()  %>%
#   group_by(Location, Year, Week, Loc_Year_Wk)  %>%
#   summarize(Avg_TMAX = mean(TMAX, na.rm = TRUE))  # Calculate average TMAX for each week
# 
# # Plot the smoothed line
# ggplot(climate_data_weekly, aes(x = Loc_Year_Wk, y = Avg_TMAX, group = Location, color = Location)) +
#   geom_line() +
#   labs(
#     title = "Weekly Average TMAX",
#     x = "Week",
#     y = "Average Temperature (°C)",
#     color = "Location"
#   ) +
#   theme_minimal()
















# # plot temperature & precipitation across     -----------------------------------------------
# plot(climate_data$day[climate_data$year==2], climate_data$temperature[climate_data$year==2], type = "l", col = "#d09220", lwd = 2,
#      xlab = "Days", ylab = "Temperature (Cº)",
#      main = "Temperature Across Study Period")
# # TODO: add precip & add all study years
# 
# # plot precipitation across    
# plot(climate_data$day[climate_data$year==2], climate_data$rainfall[climate_data$year==2], type = "l", col = "darkblue", lwd = 2,
#      xlab = "Days", ylab = "Precipitation (mm)",
#      main = "Precipitation Across Study Period")
# # TODO: accumulation















# # example code:
# 
# # Plot Boston maximum temperature ---------------------------------------------
# ggplot(data = BOS_climate) + 
#   geom_line(mapping = aes(x = date, y = tmax, col = station)) +
#   labs(x = "Date", y = "Maximum Temperature (C)")
# 
# # Clip data to 1981-2010 ------------------------------------------------------
# BOS_climrecent <- BOS_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011)
# 
# # Plot Phoenix and Boston maximum temperature, 1981-2010
# ggplot(data = BOS_climrecent) + 
#   geom_line(mapping = aes(x = date, y = tmax)) +
#   labs(x = "Date", y = "Maximum Temperature (C)")
# 
# # Calculate 30-year climatology -----------------------------------------------
# BOS_tmaxClimatology <- BOS_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   group_by(month_day) %>%
#   summarize(tmax_mean = mean(tmax, na.rm = TRUE))
# 
# # Plot Boston climatological maximum temperature, 1981-2010
# ggplot(data = BOS_tmaxClimatology) + 
#   geom_line(mapping = aes(x = as.Date(month_day, format = "%m-%d"), 
#                           y = tmax_mean)) + 
#   labs(x = "Date", y = "Maximum Temperature (C)") +
#   scale_x_date(date_labels = "%b")
# 
# # Calculate daily departures from climatology ---------------------------------
# BOS_tmaxDiff <- BOS_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   left_join(BOS_tmaxClimatology, by = "month_day") %>%
#   mutate(tmax_diff = tmax - tmax_mean)
# 
# # Plot Boston difference from climatological maximum temperature, 1981-2010
# ggplot(data = BOS_tmaxDiff) + 
#   geom_line(mapping = aes(x = date, y = tmax_diff)) + 
#   labs(x = "Date", y = "Temperature difference from 30-year average (C)")
# 
# # Phoenix climatology and departures ------------------------------------------
# PHO_tmaxClimatology <- PHO_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   group_by(month_day) %>%
#   summarize(tmax_mean = mean(tmax, na.rm = TRUE))
# 
# PHO_tmaxDiff <- PHO_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   left_join(PHO_tmaxClimatology, by = "month_day") %>%
#   mutate(tmax_diff = tmax - tmax_mean)
# 
# # Plot Boston and Phoenix predictability --------------------------------------
# ggplot(data = BOS_tmaxDiff) + 
#   geom_density(mapping = aes(x = tmax_diff)) +
#   labs(x = "Daily max temp difference from 30-year normal [C]", y = "Probability")
# 
# ggplot(data = PHO_tmaxDiff) + 
#   geom_density(mapping = aes(x = tmax_diff)) +
#   labs(x = "Daily max temp difference from 30-year normal [C]", y = "Probability")
# 
# # Calculate statistics for predictability -------------------------------------
# mean(BOS_tmaxDiff$tmax_diff)  # Mean departure for Boston
# sd(BOS_tmaxDiff$tmax_diff)    # Standard deviation for Boston
# quantile(BOS_tmaxDiff$tmax_diff, c(0.05, 0.95))  # 5% tails for Boston
# 
# mean(PHO_tmaxDiff$tmax_diff)  # Mean departure for Phoenix
# sd(PHO_tmaxDiff$tmax_diff)    # Standard deviation for Phoenix
# quantile(PHO_tmaxDiff$tmax_diff, c(0.05, 0.95))  # 5% tails for Phoenix
# 





















# ```{r}
# # Plot Boston maximum temperature
# ggplot(data = BOS_climate) + 
#   geom_line(mapping = aes(x = date, y = tmax, col=station)) +
#   labs(x = "Date", y = "Maximum Temperature (C)")

# ```

# The syntax of ggplot and its arguments should look familiar from the R4DS reading. The first argument to ggplot identifies the data frame for plotting, and `geoms` are added that specify how the data are mapped onto the plot. For this plot, we are mapping `date` onto the x-axis, `tmax` onto the y-axis, and coloring the lines by station. We can also change the axis labels by adding the `labs` function to the `ggplot` list. 

# ***
# **Code Challenge 1:
# Within your group, create a plot that shows the daily precipitation values for Boston.**

# ***

# ### 2. Climatology

# Now we'll aggregate these data into a climatology to determine whether any one day, month, or year is different from the long-term normal value. A 30-year averaging period is typically used in climatology because this period captures many years that can vary with cyclical patterns like El Niño. 

# Climatological periods are defined by the World Meteorological Organization as a 30-year interval that starts with a year that ends in a 1 and end on a year that ends on a 0, so the nearest climatological period to 2019 (the last complete year) is 1981-2010. This is called a climatological "normal" because we use it to evaluate whether any particular value is different from normal conditions. 

# The first step for our climatology is to clip our data to 1981-2010. To do this, we'll use the `filter()` function from the `tidyverse` package. Almost all of the `tidyverse` data processing functions have a similar structure to arguments (you'll read more about these in detail in PreLab 2):

# 1. The first argument is the data frame we're using.
# 2. The second argument describes how we'd like to change the data. 

# Here, the second argument is using a function from the `lubridate` package that we loaded at the beginning -- `year()` -- to return just the year from the `date` variable. We're using the `year()` function to only select the years that are greater than 1980 and less than 2011. (Equivalently, we could have written `year(date) >= 1981 & year(date) <= 2010` with less than/greater than or equal for this second argument). 

# ```{r}
# # Clip out 1981-2010 in BOS from the climate data
# BOS_climrecent <- filter(BOS_climate, year(date) > 1980 & year(date) < 2011)

# # Plot Phoenix and Boston maximum temperature, 1981-2010
# ggplot(data = BOS_climrecent) + 
#   geom_line(mapping = aes(x = date, y = tmax)) +
#   labs(x = "Date", y = "Maximum Temperature (C)")

# ```

# The next step is to take the 30-year average for maximum temperature on each day. We'll do this with a set of three functions from the `tidyverse`: 

# 1. `mutate()` to add a column with just the month & day for each date (removing year)
# 2. `group_by()` to define the month-day column as the grouping variable
# 3. `summarize()` to calculate the 30-year statistics over the grouping variable

# ```{r, warning=FALSE}
# # Add a new column with just month & day from date
# BOS_climrecent_v2 <- mutate(BOS_climrecent,
#                             month_day = format(date, "%m-%d"))

# # Look at the new month_day column
# BOS_climrecent_v2

# # Group data frame by month_day for climatology
# # (we want to average over all 30 years for max temp on Jan-01, Jan-02, etc.)
# BOS_clim_grouped <- group_by(BOS_climrecent_v2, month_day)

# # Calculate 30-year average tmax for each day of the year
# BOS_tmax30yr <- summarize(BOS_clim_grouped, 
#                           tmax_mean = mean(tmax, na.rm=TRUE))

# # Plot Boston normal maximum temperature, climatology 1981-2010
# ggplot(data = BOS_tmax30yr) + 
#   geom_line(mapping = aes(x = as.Date(month_day, format = "%m-%d"), 
#                           y = tmax_mean)) + 
#   labs(x = "Date", y = "Maximum Temperature (C)") +
#   scale_x_date(date_labels = "%b")
# ```

# Whenever we are writing a set of processing steps with intermediate objects (e.g., `BOS_climrecent` and `BOS_climrecent_v2`), it's good to think about putting those steps together into neater blocks of code by using `pipes %>%` from the `maggritr` package (included in the `tidyverse`). A pipe takes the output from one line and "pipes" it to the following line without creating an intermediate object. 

# For example, we can re-write the steps from above to find the 30-year Boston climatology using pipes:

# ```{r}
# # Summarize 30-year 1981-2010 max temperature
# BOS_tmaxClimatology <- BOS_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   group_by(month_day) %>%
#   summarize(tmax_mean = mean(tmax, na.rm=TRUE))

# # Plot Boston climatological maximum temperature, 1981-2010
# ggplot(data = BOS_tmaxClimatology) + 
#   geom_line(mapping = aes(x = as.Date(month_day, format = "%m-%d"), 
#                           y = tmax_mean)) + 
#   labs(x = "Date", y = "Maximum Temperature (C)") +
#   scale_x_date(date_labels = "%b")

# ```


# ***
# **Code Challenge 2:
# Repeat the steps that we used here to calculate and visualize the 30-year climatological mean daily maximum temperature for Phoenix. BEFORE YOU START CODING, outline and review the steps that you are going to take within your group.**

# ***

# Next, we'll calculate the difference between the daily weather data of maximum temperature and the climatological 30-year mean maximum temperature to see when values differ from normal. 

# The first step is to attach a column with the 30-year mean daily values onto the original daily weather data frame. We'll use the `left_join()` function to connect the 30-year climatology by matching up the day-month in both datasets. We'll learn more about `joins` in detail later on. Then, we'll use `mutate()` to make a new column that is tmax minus tmax_mean for the difference between the actual temperature and the 30-year mean temperature on that day. 

# ```{r}
# # Join 30-year mean daily max temp to all yearly data and calculate difference
# BOS_tmaxDiff <- BOS_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   left_join(BOS_tmaxClimatology, by = "month_day") %>%
#   mutate(tmax_diff = tmax - tmax_mean)

# # Look at the new data frame
# BOS_tmaxDiff

# # Plot Boston difference from climatological maximum temperature, 1981-2010
# ggplot(data = BOS_tmaxDiff) + 
#   geom_line(mapping = aes(x = date, y = tmax_diff)) + 
#   labs(x = "Date", y = "Temperature difference from 30-year average (C)")

# ```

# Now let's do the same steps to calculate the Phoenix daily departures in maximum temperature from the 30-year climatology: 

# ```{r}
# # Summarize 30-year 1981-2010 max temperature
# PHO_tmaxClimatology <- PHO_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   group_by(month_day) %>%
#   summarize(tmax_mean = mean(tmax, na.rm=TRUE))

# # Join 30-year mean daily max temp to all yearly data and calculate difference
# PHO_tmaxDiff <- PHO_climate %>%
#   filter(year(date) > 1980 & year(date) < 2011) %>%
#   mutate(month_day = format(date, "%m-%d")) %>%
#   left_join(PHO_tmaxClimatology, by = "month_day") %>%
#   mutate(tmax_diff = tmax - tmax_mean)
# ```


# ### 3. Weather predictability

# Departures from mean climate are important for ecology, including human health. For example, in Boston our infrastructure is built to handle large amounts of snow during the winter, but the same sized snowstorm further South could bring things to a standstill for days. Similarly, heat stress is an important factor when considering climate change: if infrastructure is built for a particular temperature within a city, unexpected temperature departures can have devastating impacts for people who cannot quickly adjust temperature conditions in their homes. 

# We can assess the predictability of weather within a location by comparing daily weather to the 30-year climatology. We’ll define unpredictability as larger deviations from the mean climatology. We've already calculated the difference between daily maximum temperatures and their 30-year average. To visualize this difference, we can look at a probability density of the departures from the long-term average: 

# ```{r}
# # Plot Boston difference from climatological maximum temperature, 1981-2010
# ggplot(data = BOS_tmaxDiff) + 
#   geom_density(mapping = aes(x = tmax_diff)) +
#   labs(x = "Daily max temp difference from 30-year normal [C]", y = "Probability") 

# ```

# We can also get values for the mean, standard deviation, and different percentiles of the climatological departures:

# ```{r}
# mean(BOS_tmaxDiff$tmax_diff) # mean departure (basically zero)
# sd(BOS_tmaxDiff$tmax_diff) # standard deviation
# quantile(BOS_tmaxDiff$tmax_diff, c(0.05, 0.95)) # 5% tails of the probability values

# ```

# The above plot and quantile calculation shows that in Boston, there about a 5% chance of having a day that is about 14-15 degrees warmer or colder than average. This is a small but non-negligible probability. We can use the probability density for Boston maximum temperature departures as a benchmark to assess whether Phoenix's maximum temperatures are more or less predictable than Boston's. 

# ***
# **Code Challenge 3:
# Produce a plot that shows the probability density for the difference between daily maximum tempearture and the 30-year climatological mean for Phoenix. Comparing the probability densities of maximum temperature departures for Boston and Phoenix, which city is more likely to experience an "abnormally hot" day?**
