# Run rsunflo and save output to a csv

working_directory = "/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode"
source(file.path(working_directory, "run_model.R"))

climate_data_2006 <- read.table(
  "truncated_AUZ_2006.txt", 
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)
out_2006 <- run_sunflo(climate_data_2006, 0,working_directory)

outpath_2006 = "out_2006_noterp.csv"
# print(outpath)
write.csv(out_2006, outpath_2006)



# working_directory = "/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode"
# source(file.path(working_directory, "run_model.R"))

climate_data_2003 <- read.table(
  "truncated_AUZ_2003.txt", 
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

out_2003 <- run_sunflo(climate_data_2003, 0, working_directory)

outpath_2003 <- "out_2003_noterp.csv"
# print(outpath)
write.csv(out_2003, outpath_2003)



climate_data_2014 <- read.table(
  "truncated_AUZ_2014.txt", 
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

out_2014 <- run_sunflo(climate_data_2014, 0, working_directory)

outpath_2014 = "out_2014_noterp.csv"
# print(outpath)
write.csv(out_2014, outpath_2014)

#### #### #### #### #### ####

# for (terp_lvl in seq(0,3,0.5)) {
#   out <- run_sunflo(climate_data_2014, terp_lvl, working_directory)
#   outpath = paste0("terp_stress_", sprintf("%.2f", terp_lvl),"_out.csv")
#   write.csv(out, outpath)
#   
# }



climate_data_clean <- read.table(
  "rad_data.txt", 
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

tmp <- climate_data_2006[c("Annee","Mois","Jour","ETP","RAD")]
saveRDS(tmp, "radiation_data_AUZ_2008.RDS" )
readRDS("radiation_data_AUZ_2008.RDS")
