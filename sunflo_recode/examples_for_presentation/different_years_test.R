climate_data_df <- read.table(
  "climate_data/Georgia_clean_14_24.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

wt_mean = 8.22
wt_sd = 2.46

dom_mean = 1
dom_sd = 2.46

for (i in 1:10) {
  for (year in c(14)){
    dom_pest <- rnorm(1, mean = dom_mean, sd= dom_sd)
    clim <- get_year_climate_data(climate_data_df, year)
    out <- run_sunflo(clim, dom_pest, working_directory)
    outpath = paste0("/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode/examples_for_presentation/years_output/dom_testyears_out_2014_",sprintf("%.0f", i),".csv")
    write.csv(out, outpath)
    
  }
  
} 

for (i in 1:10) {
  for (year in c(14)){
    wt_pest <- rnorm(1, mean = wt_mean, sd= wt_sd)
    clim <- get_year_climate_data(climate_data_df, year)
    out <- run_sunflo(clim, wt_pest, working_directory)
    outpath = paste0("/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode/examples_for_presentation/years_output/WT_testyears_out_2014_",sprintf("%.0f", i),".csv")
    write.csv(out, outpath)
    
  }
  
} 


climate_data_df <- read.table(
  "climate_data/Georgia_clean_90_00.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)


for (i in 1:10) {
  for (year in c(94)){
    dom_pest <- rnorm(1, mean = dom_mean, sd= dom_sd)
    clim <- get_year_climate_data(climate_data_df, year)
    out <- run_sunflo(clim, dom_pest, working_directory)
    outpath = paste0("/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode/examples_for_presentation/years_output/dom_testyears_out_1994_",sprintf("%.0f", i),".csv")
    write.csv(out, outpath)
    
  }
  
} 

for (i in 1:10) {
  for (year in c(94)){
    wt_pest <- rnorm(1, mean = wt_mean, sd= wt_sd)
    clim <- get_year_climate_data(climate_data_df, year)
    out <- run_sunflo(clim, wt_pest, working_directory)
    outpath = paste0("/Users/jennastanislaw/Boulder/team_rotation/TeamRotationSpring2025/sunflo_recode/examples_for_presentation/years_output/WT_testyears_out_1994_",sprintf("%.0f", i),".csv")
    write.csv(out, outpath)
    
  }
  
}

