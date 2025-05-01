# import climate

# import txt file first row as header, tab separated
climate_data <- read.table(
  "~/Boulder/team_rotation/TeamRotationSpring2025/sunflo_french_repo/sunflo/data/AUZ_2014.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)
