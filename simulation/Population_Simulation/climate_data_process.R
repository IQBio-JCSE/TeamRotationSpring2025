# Functions for processing climate data 

climate_data_df <- read.table(
  "climate_data/Georgia_clean_14_24.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)
  
get_year_climate_data <- function(climate_data_df, year) {
  # year_data <- 
  
  climate_data_year <- climate_data_df[climate_data_df$Year == year, ]
  
  # climate_data_year <- 
  head(climate_data_year)
  climate_data_year$Tmax <- climate_data_year$TMAX/10
  climate_data_year$Tmin <- climate_data_year$TMIN/10
  climate_data_year$Pluie <- climate_data_year$PRCP/10
  
  # Logical condition across multiple columns
  condition <- climate_data_year$Month == 4 & climate_data_year$Day == 1
  
  # Find the index of the first matching row
  start_index <- which(condition)[1]
  end_index <- start_index + 149
  result <- climate_data_year[start_index:end_index, ]
  
}

processed <- get_year_climate_data(climate_data_df, 14)
