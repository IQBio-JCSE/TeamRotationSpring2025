# import climate

# import txt file first row as header, tab separated
climate_data <- read.table(
  "sunflo french repo/sunflo/data/AUZ_2014.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)
