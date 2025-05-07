# run SUNFLO
# test climate data
source("sunflo_recode/climate_data.R")

# import txt file first row as header, tab separated
french_climate_data <- read.table(
  "sunflo_french_repo/sunflo/data/AUZ_2014.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

french_climate_data <- read.csv("sunflo_recode/truncated_AUZ_2014.txt")
# Rename French variable names to English equivalents
french_climate_data <- french_climate_data %>%
  rename(
    Day = Jour,
    Year = Annee,
    Month = Mois,
    JulianDay = JourJ,
    TMIN = Tmin,
    TMAX = Tmax,
    ETP = ETP,
    RAD = RAD,
    PRCP = Pluie
  )

# test sunflo
test <- run_sunflo(french_climate_data,
                   2, 
                   '/Users/sestockman/Library/CloudStorage/OneDrive-UCB-O365/Courses/MAS/Rotation4/TeamRotationSpring2025/sunflo_recode')

# %>%
# mutate(DayNumber = row_number())



plot(test$DayNumber,
     test$CropYield)


lm(test$CropBiomass~test$DayNumber)

# Fit a linear model
lm_model <- lm(CropBiomass ~ DayNumber, data = test)

# Fit a piecewise linear model with a breakpoint
seg_model <- segmented(lm_model, seg.Z = ~DayNumber, npsi=3)  # Initial guess for breakpoint at x=3

# TODO: add in this segmentation & check coeffient
# View the summary of the segmented model
line_summary <- summary(seg_model)
line_summary$coefficients[2,1]

# Plot the data and the fitted piecewise model
plot(test$DayNumber, test$CropBiomass, pch = 16, col = "blue", main = "Piecewise Linear Fit", xlab = "x", ylab = "y")
lines(test$DayNumber, fitted(seg_model), col = "red", lwd = 2)  # Add the fitted line
