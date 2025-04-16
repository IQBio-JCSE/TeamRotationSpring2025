#' @import ggplot2
#' @import dplyr
#' @import tidyr

# Tools for phenotyping and genotypic parameterization

# Climate ####
# 1 watt/day = 86400 joules

# Compute reference evapotranspiration using the Penman-Monteith formula and assumptions from [@Wallach2006]
#' @export et_penman_monteith
et_penman_monteith <- function(tmin, tmax, tdew, rad, wind, lat, lon, elevation, day, ...) {
  
  # Inputs:
  # RAD      Daily Insolation Incident On A Horizontal Surface (MJ/m^2/day) 
  # TMIN     Minimum Air Temperature At 2 m Above The Surface Of The Earth (degrees C) 
  # TMAX     Maximum Air Temperature At 2 m Above The Surface Of The Earth (degrees C) 
  # TDEW     Dew/Frost Point Temperature At 2 m (degrees C) 
  # WIND     Wind Speed At 10 m Above The Surface Of The Earth (m/s)
  # DAY      Day of year
  
  # Output:
  # ET       Daily Reference Evapotranspiration (mm/day)
  
  latitude <- unique(lat) * pi / 180
  
  # Psychrometric constant
  PSC <- 0.665 * 10^-3 * 101.3 * ((293 - 0.0065 * unique(elevation)) / 293)^5.26
  
  # Wind speed at 2m
  ws2 <- wind * 4.87 / log(67.8 * 10 - 5.42)
  es <- ((0.6108 * exp(17.27 * tmax / (tmax + 237.3))) + (0.6108 * exp(17.27 * tmin / (tmin + 237.3)))) / 2
  slope <- (0.6108 * exp(17.27 * ((tmax + tmin) / 2) / (((tmax + tmin) / 2) + 237.3)) * 4098) / ((tmax + tmin) / 2 + 237.3)^2
  
  # Humidity
  ea <- 0.6108 * exp(17.27 * tdew / (tdew + 237.3))
  
  # Radiation
  SWR <- (1 - 0.23) * rad
  IRDES <- 1 + 0.033 * cos(2 * pi * day / 365.25)
  SD <- 0.409 * sin(2 * pi * day / 365.25 - 1.39)
  SSA <- acos(-tan(latitude) * tan(SD))
  extra <- 24 * 60 * 0.082 / pi * IRDES * (SSA * sin(latitude) * sin(SD) + cos(latitude) * cos(SD) * sin(SSA))
  CSR <- (0.75 + 2 * 10^-5 * unique(elevation)) * extra
  RRAD <- rad / CSR
  
  # Evapotranspiration
  LWR <- 4.903 * 10^-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * RRAD - 0.35)
  NRAD <- SWR - LWR
  ET <- (0.408 * slope * NRAD + PSC * (900 / ((tmax + tmin) / 2 + 273)) * ws2 * (es - ea)) / (slope + PSC * (1 + 0.34 * ws2))
  
  return(ET)
}

# Soil ####

# Pedotransfer function: Estimate volumetric water retention capacity from soil analysis
# θ = a + (b×%Clay) + (c×%Silt) + (d×%Sand) + (e×BulkDensity)
#' @export soil_water_capacity_type
soil_water_capacity_type <- function(
  Argile, # % Clay
  LimonFin, # % Fine Silt
  LimonGrossier, # % Coarse Silt
  SableFin, # % Fine Sand
  SableGrossier, # % Coarse Sand
  CaCO3, # % Calcium Carbonate
  MatiereOrganique, # % Organic Matter
  Profondeur, # Depth (mm)
  Cailloux # % Gravel
) {
  # [Vale2007]
  (CaCO3 + 2 * Argile + LimonFin + LimonGrossier + 0.7 * (SableFin + SableGrossier)) *
    ((100 - Cailloux) / 100) * (Profondeur / 1000) * (1 + 0.05 * MatiereOrganique - 0.1)
}

# Compute available soil water content (mm), default model inputs
#' @export soil_water_capacity
soil_water_capacity <- function(
  root_depth = 1000, stone_content = 0.1,  # Root depth (mm), % weight
  field_capacity_1 = 19.7, field_capacity_2 = 19.7, # % weight
  wilting_point_1 = 9.7, wilting_point_2 = 9.7, # % weight
  soil_density_1 = 1.3, soil_density_2 = 1.3, # Mass/volume (g/cm3)
  ...
) {
  
  # Available soil water content for surface layer 
  swc_1 <- (field_capacity_1 / 100 - wilting_point_1 / 100) * (1 - stone_content) * soil_density_1 * 300
  # Available soil water content for deep layer 
  swc_2 <- (field_capacity_2 / 100 - wilting_point_2 / 100) * (1 - stone_content) * soil_density_2 * (root_depth - 300)
  # Total available soil water content 
  swc <- swc_1 + swc_2
  
  return(swc)
}

# Phenology ####

# Compute temperature sum between two dates
#' @export thermal_time
thermal_time <- function(climate, id, start, end, base = 4.8, ...) {
  if (is.na(start) | is.na(end)) {
    return(NA)
  } else {
    # Select vector for mean temperature
    temperature <- table_climate %>% filter(trial_id == id) %>% slice(start:end) %>% .$TM
    # Conditional temperature sum
    thermal_time <- sum(ifelse(temperature - base < 0, 0, temperature - base))
    return(thermal_time)
  }
}

# Compute secondary phenological stages from flowering date
#' @export phenostage
phenostage <- function(flowering) {
  r <- data.frame(
    TDF1 = flowering, # Flowering date
    TDE1 = 0.576 * flowering, # End of flowering
    TDM0 = flowering + 246.5 # Maturity date
  )
  return(r)
}

# Architecture ####

# Leaf surface model = f(Length, Width) cm
#' @export leaf_size
leaf_size <- function(length, width, a0 = 0.7, a = 0.736, b = -8.86, c = 0.684, shape = "simple", ...) {
  
  switch(
    shape,
    
    # Default to simple linear model, c.f Heliaphen_platform repository
    simple = {
      area <- a0 * length * width
    },
    
    bilinear = {
      area <- ifelse(length * width < (b / (c - a)),
                     c * length * width,
                     a * length * width + b
      )
    }
  )
  return(area)
}

# Leaf profile model
#' @export leaf_profile
leaf_profile <- function(TLN, LLS, LLH, a = -2.05, b = 0.049, shape = "fixed", output = "profile", ...) {
  
  # Number of phytomers
  n <- 1:TLN
  
  # Compute leaf profile using two methods for shape coefficients
  switch(
    shape,
    fixed = {
      # a = -2.110168, b = 0.01447336 [Casadebaig2013]: average of linear model on the DB
      # a = -2.049676, b = 0.04937692 [Casadebaig2013]: average adjustments on observed DB
      r <- LLS * exp(a * ((n - LLH) / (LLH - 1))^2 + b * ((n - LLH) / (LLH - 1))^3)    
    },
    model = {
      b <- 1.5 - 0.2210304 * LLH - 0.0003529 * LLS + 0.0825307 * length(n)
      a <- -2.313409 + 0.018158 * LLH - 0.001637 * LLS + 0.019968 * length(n) + 0.920874 * b          
      r <- LLS * exp(a * ((n - LLH) / (LLH - 1))^2 + b * ((n - LLH) / (LLH - 1))^3)    
    }
  )
  
  # Output
  switch(
    output,
    profile = return(data.frame(leaf = n, size = r)),
    shape = return(data.frame(a = a, b = b))
  )
}

# Model extinction coefficient as a function of architectural traits
# TLN : Total leaf number (leaf rank)
# LLH : Largest leaf height (leaf rank)
# LLS : Largest leaf size (cm^2)
# H   : Plant height (cm)
#' @export coefficient_extinction
coefficient_extinction <- function(TLN, LLH, LLS, H, ...) {  
  # Pouzet-Bugat method [@Pouzet1985]
  TPA <- 0.5 * TLN * LLS + 30 * TLN  
  
  # Fitted in [@Casadebaig2008]
  K <- -1.11E-2 * LLH - 1.09E-2 * TLN - 1.12E-3 * LLS - 0.11E-2 * H + 6.5E-5 * TPA + 1.58
  return(K)
}

# Allocation ####

# Convert oil content expressed at norms of humidity (9%) and impurity (2%) to a dry matter basis
#' @export convert_oilcontent
convert_oilcontent <- function(x, humidity = 9, impurity = 2) {
  r <- (1 - impurity / 100) * (1 - humidity / 100)
  return(x * 1 / r)
}

# Convert grain content expressed at norms of humidity (9%) and impurity (2%) to a dry matter basis
#' @export convert_grainyield
convert_grainyield <- function(x, humidity = 9, impurity = 2) {
  r <- (1 - impurity / 100) * (1 - humidity / 100)
  return(x * r)
}

# Response ####

# Response of plant transpiration/stomatal conductance to water stress
#' @export curve_conductance
curve_conductance <- function(x, a) {
   t = 1.05 / (1 + 4.5 * exp(a * x))
   return(t)
}

# Response of expansion to water stress
#' @export curve_expansion
curve_expansion <- function(x, a) {
  t = (2 / (1 + exp(a * x))) - 1
  return(t)
}

# Compute quantitative low and high temperature stress index on RUE
#' @export curve_thermal_rue
curve_thermal_rue <- function(tm, tb = 4.8, tol = 20, tou = 28, tc = 37, type = "low") {
  
  switch(
    type, 
    
    low = {
      ifelse(tm > tb & tm < tol, tm * (1 / (tol - tb)) - (tb / (tol - tb)),
             ifelse(tm <= tb, 0, 1)
      )
    },
    
    high = {
      ifelse(tm > tou & tm < tc, tm * (1 / (tou - tc)) - (tc / (tou - tc)),
             ifelse(tm >= tc, 0, 1)
      )
    }
  )
}

# Break linear model
#' @export curve_breaklinear
curve_breaklinear <- function(x, a, b) {
  t = NULL
  for (i in 1:length(x)) {
    if (x[i] < a) y = (1 - b) / a * x[i] + b else y = 1
    t = c(t, y)
  }
  return(t)        
}


# Unit conversions and tools ####


# File management ####

#' @export yday_date
yday_date <- function(yday, year) {
  as.Date(yday - 1, origin = paste0(year,"-01-01"))  
}


#' @export spread_management
spread_management <- function(x) {
  
  # nitrogen 
  s <- x[x$fertilisation != 0,c("date","fertilisation")]
  names(s) <- c("date","dose")
  n <- data.frame(
    nitrogen_date_1 = s$date[1],
    nitrogen_dose_1 = s$dose[1],
    nitrogen_date_2 = s$date[2],
    nitrogen_dose_2 = s$dose[2]
  )
  
  # water
  s <- x[x$irrigation != 0,c("date","irrigation")]
  names(s) <- c("date","dose")
  w <- data.frame(
    water_date_1 = s$date[1],
    water_dose_1 = s$dose[1],
    water_date_2 = s$date[2],
    water_dose_2 = s$dose[2],
    water_date_3 = s$date[3],
    water_dose_3 = s$dose[3],
    water_date_4 = s$date[4],
    water_dose_4 = s$dose[4],
    water_date_5 = s$date[5],
    water_dose_5 = s$dose[5],
    water_date_6 = s$date[6],
    water_dose_6 = s$dose[6],
    water_date_7 = s$date[7],
    water_dose_7 = s$dose[7],
    water_date_8 = s$date[8],
    water_dose_8 = s$dose[8],
    water_date_9 = s$date[9],
    water_dose_9 = s$dose[9]
  )
  
  # output
  return(data.frame(n,w))
}


# Statistical indicators and function ####


# Calculer l'efficience du modèle
#' @export efficiency
efficiency <- function (obs, sim) {
  1 - (sum((na.omit(obs- sim))^2)/sum((na.omit(obs) - mean(obs, na.rm=TRUE))^2))
}

# Not in
#' @export '%ni%'
'%ni%' <- Negate('%in%')



# Graphical representations ####

# Biplot pour les objets produits par agricolae::AMMI
#' @export biplot_ammi
biplot_ammi <- function(m) {
  
  env <- m$biplot[m$biplot$type == "ENV",]
  env <- mutate(env, names = rownames(env))
  gen <- m$biplot[m$biplot$type == "GEN",] 
  gen <- mutate(gen, names = rownames(gen))
  
  ggplot() +
    geom_vline(x=0, colour="grey50") + 
    geom_hline(y=0, colour="grey50") + 
    geom_text(
      data=env,
      aes(x=PC1, y=PC2, label=names),
      angle=0, size=3, colour="grey50"
    ) + 
    geom_segment(
      data=env,
      aes(x=0, y=0, xend=PC1, yend=PC2),
      size=0.2, colour="grey70"
    ) + 
    geom_text(
      data=gen,
      aes(x=PC1, y=PC2, label=names),
      angle=0, size=3, vjust=1
    ) +
    labs(
      x=paste("PC 1 (",m$analysis$percent[1]," %)", sep=""),
      y=paste("PC 2 (",m$analysis$percent[2]," %)", sep="")
    ) +
    theme_bw()  
}

# Scatterplot matrix
# input should be a dataframe or data_table without key and in wide format
#' @export splom

splom <- function(data, plot_size_lims=c(0,1)) {
  
  # add id and normalize values
  data <- data %>% mutate_all(funs(rescale)) %>% mutate(id=1:n())
  
  # list selected variables (traits) 
  list_variables <- data %>% select(-id) %>% names(.)
  
  # compute possible variable combinations, use factors to keep order
  list_variables_design <- list_variables %>%
    combn(., m=2) %>% t() %>% data.frame() %>%
    mutate(panel=1:n()) %>%
    rename(x_lab=X1, y_lab=X2) %>%
    mutate(
      x_lab=factor(x_lab, levels=list_variables),
      y_lab=factor(y_lab, levels=list_variables)
    )
  
  # create labels for matrix diagonal
  data_labels <- data_frame(
    x_lab=list_variables,
    y_lab=list_variables,
    x=0.5, y=0.5
  ) %>%
    mutate(
      x_lab=factor(x_lab, levels=list_variables),
      y_lab=factor(y_lab, levels=list_variables)
    )
  
  # join design with actual dataset (input space)
  data_splom <- list_variables_design %>%
    left_join(data %>% gather(x_lab, x, -id, factor_key=TRUE)) %>%
    left_join(data %>% gather(y_lab, y, -id, factor_key=TRUE))
  
  # compute correlations between variables
  data_cor <- data_splom %>%
    group_by(x_lab, y_lab) %>%
    summarise(
      cor=cor(x, y, use="pairwise.complete.obs"),
      p=cor.test(x, y)$p.value,
      n=n()
    ) %>%
    mutate(test=ifelse(p < 0.05, TRUE, FALSE)) %>%
    rename(x_lab=y_lab, y_lab=x_lab)
  
  # plot performance of optimal solution in feasibility space  
  plot <- ggplot() +
    geom_point(data=data_splom, aes(x, y)) +
    geom_text(data=data_cor, aes(x=0.5, y=0.5, label=round(cor, digits=2), color=test), size=4) +
    geom_text(data=data_labels, aes(x, y, label=x_lab), size=4, alpha=0.5) +
    facet_grid(y_lab ~ x_lab, drop=FALSE) +
    xlim(plot_size_lims) + ylim(plot_size_lims) + 
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_blank(),
      legend.position="none"
    )
  
  return(plot)
}







# Tools for input, simulation and output data.

#' @import ggplot2
#' @import dplyr

# Climate ####
# Fonction pour la gestion des données climatiques
#' 
#' @export climate
climate <- function(x, 
	input.format = "date",
	input.labels,
	output.labels = c("JourJ","Annee","Mois","Jour","Tmin","Tmax","ETP","RAD","Pluie"), 
	output.prefix = "TMP")
	{

	switch(
    input.format,
    
    # Format site, date, [mesures]
    date = {
      # Ajout des colonnes utilisées par RECORD
      x <- mutate(
        x,
        JourJ = lubridate::yday(date),
        Annee = lubridate::year(date),
        Mois = lubridate::month(date),
        Jour = lubridate::day(date) 
      )
      
      # Mise en forme du fichier de sortie
      o <- cbind(
        x[,c("JourJ","Annee","Mois","Jour")],
        x[,match(input.labels, colnames(x))]
      )
      # Renomme les colonnes au format RECORD
      colnames(o) <- output.labels
      
      # Test sur la présence de données manquantes
      try(na.fail(o))
      
      # Ecriture des fichiers de sortie
      filename <- paste(unique(x$id),".txt", sep="")
      write.table(o, file = filename, sep="\t", dec=".", row.names = FALSE)
      
      # Sortie
      return(data.frame(o))
    },
		
		climatik = {
			# Selection des colonnes utilisées pour la simulation
			o <- x[,match(input.labels, colnames(x))]
			# Ajout colonne Jour calendaires 
			o <- cbind(JourJ = 1:dim(o)[1], o)
			# Renomme les colonnes au format RECORD
			colnames(o) <- output.labels
			
			# Test sur la présence de données manquantes
			try(na.fail(o))
					
			# Ecriture des fichiers de sortie
			filename <- paste(output.prefix,"_",unique(o$Annee),".txt", sep="")
			write.table(o, file = filename, sep="\t", dec=".", row.names = FALSE)
			
			# Sortie
			return(data.frame(o))
		},
		
    # Format id, site, [mesures]
		simple = {	
			# Selection des colonnes utilisées pour la simulation
			o <- x[,match(input.labels, colnames(x))]
			
      # Renommage au format RECORD 
			colnames(o) <- output.labels
			
			# Test sur la présence de données manquantes
			try(na.fail(o))
					
			# Ecriture des fichiers de sortie
			filename <- paste(unique(x$id),"_",unique(o$Annee),".txt", sep="")
			write.table(o, file = filename, sep="\t", dec=".", row.names = FALSE)
			
			# Sortie
			return(data.frame(o))
		}
	)
}

# Simulation ####

# Test design object for default column name and missing data
#' @export test_design
test_design <- function(object){
  
  # list default inputs for simulation
  list_inputs <- c(
    "file", "root_depth", "stone_content", "field_capacity_1", "wilting_point_1",
    "field_capacity_2", "wilting_point_2", "soil_density_1", "soil_density_2",
    "mineralization", "crop_sowing", "crop_harvest", "crop_density",
    "nitrogen_date_1", "nitrogen_dose_1", "nitrogen_date_2", "nitrogen_dose_2",
    "water_date_1", "water_dose_1", "water_date_2", "water_dose_2", "water_date_3",
    "water_dose_3", "crop_emergence", "water_initial_1", "water_initial_2",
    "nitrogen_initial_1", "nitrogen_initial_2", "TDE1", "TDF1", "TDM0", "TDM3",
    "TLN", "LLH", "LLS", "K", "LE", "TR", "HI", "OC", "begin", "end",
    "duration", "file")
  
  # test for default set of input names in object headers 
  testthat::expect_true(all(list_inputs %in% names(object)))
  
  # test for missing values in object columns
  # TODO : better test by returning positions object %>% filter_at(.vars=list_inputs, any_vars(is.na(.)))
  testthat::expect_false(any(object %>% select(!!list_inputs) %>% is.na()))
  
  # TODO test for formatting and units
  
  # TODO test for existence of files in indicated path
  # list_files <- paste0("~/.vle/pkgs-2.0/sunflo/data/", design %>% distinct(file) %>% pull(file))
  # file.exists(list_files)
  
  # TODO more test ideas
  # range(design$stone_content)
  # error_soil <- design %>% filter(field_capacity_1 <= wilting_point_1)
  # error_date <- design %>% mutate(cycle = crop_harvest - crop_sowing) %>% filter(cycle < 100)
  # error_climate : test year length in climate files
}


# run sunflo VLE model as a function of experimental design

#' @export play

play <- function(data, model=sunflo, unit) {
  
  # get parameters from design
  # TODO : get default parameter values from model
  data <- data %>% slice(unit)
  
  # run model with new parameters  
  output <- model %>% 
    run(
      cBegin.begin_date                   = as.character(data$begin),
      simulation_engine.duration          = as.numeric(data$duration),
      sunflo_climat.meteo_file            = as.character(data$file),
      itk.jsemis                          = as.character(data$crop_sowing),
      itk.jrecolte                        = as.character(data$crop_harvest),
      itk.densite                         = as.numeric(data$crop_density),
      itk.fertilization_1                 = paste0("date=",as.character(data$nitrogen_date_1),"$dose=",data$nitrogen_dose_1),
      itk.fertilization_2                 = paste0("date=",as.character(data$nitrogen_date_2),"$dose=",data$nitrogen_dose_2),
      itk.irrigation_1                    = paste0("date=",as.character(data$water_date_1),"$dose=",data$water_dose_1),
      itk.irrigation_2                    = paste0("date=",as.character(data$water_date_2),"$dose=",data$water_dose_2),
      CONFIG_SimuInit.init_value_N1       = as.numeric(data$nitrogen_initial_1),
      CONFIG_SimuInit.init_value_N3       = as.numeric(data$nitrogen_initial_2),
      CONFIG_Sol.Hini_C1                  = as.numeric(data$water_initial_1),
      CONFIG_Sol.Hini_C2                  = as.numeric(data$water_initial_2),
      CONFIG_Sol.profondeur               = as.numeric(data$root_depth),
      CONFIG_Sol.Vp  		                  = as.numeric(data$mineralization),
      CONFIG_Sol.Hcc_C1 		              = as.numeric(data$field_capacity_1),
      CONFIG_Sol.Hcc_C2 		              = as.numeric(data$field_capacity_2),
      CONFIG_Sol.Hpf_C1 		              = as.numeric(data$wilting_point_1),
      CONFIG_Sol.Hpf_C2 		              = as.numeric(data$wilting_point_2),
      CONFIG_Sol.da_C1 		 	              = as.numeric(data$soil_density_1),
      CONFIG_Sol.da_C2 		    	          = as.numeric(data$soil_density_2),
      CONFIG_Sol.TC 		                  = as.numeric(data$stone_content),
      CONFIG_Variete.date_TT_E1  		      = as.numeric(data$TDE1),
      CONFIG_Variete.date_TT_F1  			    = as.numeric(data$TDF1),
      CONFIG_Variete.date_TT_M0  			    = as.numeric(data$TDM0),
      CONFIG_Variete.date_TT_M3  			    = as.numeric(data$TDM3),
      CONFIG_Variete.TLN     			        = as.numeric(data$TLN),
      CONFIG_Variete.ext     			        = as.numeric(data$K),
      CONFIG_Variete.bSF   				        = as.numeric(data$LLH),
      CONFIG_Variete.cSF   				        = as.numeric(data$LLS),
      CONFIG_Variete.a_LE  				        = as.numeric(data$LE),
      CONFIG_Variete.a_TR  				        = as.numeric(data$TR),
      CONFIG_Variete.IRg   				        = as.numeric(data$HI),
      CONFIG_Variete.thp   				        = as.numeric(data$OC)
    ) %>%
    results()
  
  return(output) 
}

# call to rvle::run to expose all conditions for potential optimization, with fixed values listed in design (list)
#' @export play_optimize

play_optimize <- function(
  model, design, unit,
  
  # set parameters default value from design (from vpz ?)
  begin = design[["begin"]][unit],
  duration = design[["duration"]][unit],
  file = as.character(design[["file"]][unit]),
  nitrogen_initial_1 = design[["nitrogen_initial_1"]][unit],
  nitrogen_initial_2 = design[["nitrogen_initial_2"]][unit],
  water_initial_1 = design[["water_initial_1"]][unit],
  water_initial_2 = design[["water_initial_2"]][unit],
  crop_emergence = as.POSIXct("2016-01-01"),
  root_depth = design[["root_depth"]][unit],
  mineralization = design[["mineralization"]][unit],
  field_capacity_1 = design[["field_capacity_1"]][unit],
  field_capacity_2 = design[["field_capacity_2"]][unit],
  wilting_point_1 = design[["wilting_point_1"]][unit],
  wilting_point_2 = design[["wilting_point_2"]][unit],
  soil_density_1 = design[["soil_density_1"]][unit],
  soil_density_2 = design[["soil_density_2"]][unit],
  stone_content = design[["stone_content"]][unit],
  crop_sowing = design[["crop_sowing"]][unit],
  crop_harvest = design[["crop_harvest"]][unit],
  crop_density = design[["crop_density"]][unit],
  nitrogen_date_1 = design[["nitrogen_date_1"]][unit],
  nitrogen_date_2 = design[["nitrogen_date_2"]][unit],
  nitrogen_dose_1 = design[["nitrogen_dose_1"]][unit],
  nitrogen_dose_2 = design[["nitrogen_dose_2"]][unit],
  water_date_1 = design[["water_date_1"]][unit],
  water_date_2 = design[["water_date_2"]][unit],
  water_date_3 = design[["water_date_3"]][unit],
  water_dose_1 = design[["water_dose_1"]][unit],
  water_dose_2 = design[["water_dose_2"]][unit],
  water_dose_3 = design[["water_dose_3"]][unit],
  TDE1 = design[["TDE1"]][unit],
  TDF1 = design[["TDF1"]][unit],
  TDM0 = design[["TDM0"]][unit],
  TDM3 = design[["TDM3"]][unit],
  TLN = design[["TLN"]][unit],
  K = design[["K"]][unit],
  LLH = design[["LLH"]][unit],
  LLS = design[["LLS"]][unit],
  LE = design[["LE"]][unit],
  TR = design[["TR"]][unit],
  HI = design[["HI"]][unit],
  OC = design[["OC"]][unit],
  ...
){
  
  # set defaults conditions : 2 soils layers, all genotype-dependant parameters, high input management, emergence (n=42)
  setDefault(
    model,
    begin  							                = begin,
    duration					              		= duration,
    CONFIG_ClimatNomFichier.meteo_file	= file,
    CONFIG_SimuInit.rh1                 = nitrogen_initial_1,
    CONFIG_SimuInit.rh2                 = nitrogen_initial_2,
    CONFIG_SimuInit.Hini_C1             = water_initial_1,
    CONFIG_SimuInit.Hini_C2             = water_initial_2,
    CONFIG_SimuInit.dateLevee_casForcee = ifelse(
      format(crop_emergence,"%m") == "01",
      "00/00", format(crop_emergence, "%d/%m")),
    CONFIG_Sol.profondeur               = root_depth,
    CONFIG_Sol.Vp  		                  = mineralization,
    CONFIG_Sol.Hcc_C1 		              = field_capacity_1,
    CONFIG_Sol.Hcc_C2 		              = field_capacity_2,
    CONFIG_Sol.Hpf_C1 		              = wilting_point_1,
    CONFIG_Sol.Hpf_C2 		              = wilting_point_2,
    CONFIG_Sol.da_C1 		 	              = soil_density_1,
    CONFIG_Sol.da_C2 		    	          = soil_density_2,
    CONFIG_Sol.TC 		                  = stone_content,
    CONFIG_Conduite.jsemis    		    	= format(crop_sowing, format="%d/%m"),
    CONFIG_Conduite.jrecolte            = format(crop_harvest, format="%d/%m"),
    CONFIG_Conduite.densite        	    = crop_density,
    CONFIG_Conduite.date_ferti_1       	= format(nitrogen_date_1, format="%d/%m"),
    CONFIG_Conduite.date_ferti_2        = format(nitrogen_date_2, format="%d/%m"),
    CONFIG_Conduite.apport_ferti_1     	= nitrogen_dose_1,
    CONFIG_Conduite.apport_ferti_2      = nitrogen_dose_2,
    CONFIG_Conduite.date_irrig_1        = format(water_date_1, format="%d/%m"),
    CONFIG_Conduite.date_irrig_2        = format(water_date_2, format="%d/%m"),
    CONFIG_Conduite.date_irrig_3        = format(water_date_3, format="%d/%m"),
    CONFIG_Conduite.apport_irrig_1     	= water_dose_1,
    CONFIG_Conduite.apport_irrig_2      = water_dose_2,
    CONFIG_Conduite.apport_irrig_3      = water_dose_3,
    CONFIG_Variete.date_TT_E1  		      = TDE1,
    CONFIG_Variete.date_TT_F1  			    = TDF1,
    CONFIG_Variete.date_TT_M0  			    = TDM0,
    CONFIG_Variete.date_TT_M3  			    = TDM3,
    CONFIG_Variete.TLN     			        = TLN,
    CONFIG_Variete.ext     			        = K,
    CONFIG_Variete.bSF   				        = LLH,
    CONFIG_Variete.cSF   				        = LLS,
    CONFIG_Variete.a_LE  				        = LE,
    CONFIG_Variete.a_TR  				        = TR,
    CONFIG_Variete.IRg   				        = HI,
    CONFIG_Variete.thp   				        = OC
  )
  
  # save vpz image
  # saveVpz(model, "model.vpz")
  
  # evaluate the model
  r <- results(run(model))
  
  return(r)
}



# Mise en forme des données brutes de sortie
#' @export shape
shape <- function(data, view="generic") {
  
	switch(view,
		
	       generic = {
	         # shorten colums names
	         names(data[[1]]) <- sub(".*\\.","", names(data[[1]]))
	         
	         # rename output variables and
	         # convert VLE time format (JDN cf. http://en.wikipedia.org/wiki/Julian_day) to date
	         # delta = julian(x = 01, d = 01, y = 1970, origin = c(month=11, day=24, year=-4713))
	         output <- data %>% .[[1]] %>%
	           select(-time) %>% 
	           rename(time=current_date) %>% 
	           mutate(time=as.Date(time, origin="1970-01-01") - 2440588) 
	       },

	       timed = {
	         names(data[[1]]) <- sub(".*\\.","", names(data[[1]]))
	        
	         output <- data %>% .[[1]] %>% 
	           select(
	             time=current_date, TTA2=TT_A2, PhenoStage=PhasePhenoPlante,
	             TN=Tmin, TM=Mean, TX=Tmax, GR=RAD, PET=ETP, RR=Pluie,
	             FTSW, FHTR, FHRUE, ETPET=ETRETM, FTRUE=FT, NAB=Nabs, NNI=INN, FNRUE=FNIRUE, 
	             LAI, RIE=Ei, RUE=Eb, TDM, GY=photo_RDT_aFinMATURATION, OC=photo_TH_aFinMATURATION
	           ) %>% 
	           mutate(time=as.Date(time, origin="1970-01-01") - 2440588)
	       },
	       
	       end = {
	         names(data[[1]]) <- sub(".*\\.","", names(data[[1]]))
	         
	         output <- data %>% .[[1]] %>% 
	           select(time=current_date, GY=photo_RDT_aFinMATURATION, OC=photo_TH_aFinMATURATION) %>% 
	           mutate(time=as.Date(time, origin="1970-01-01") - 2440588) 
	       },
	)
  return(as_tibble(output))
}


# summarise timed output variables 
#' @export indicate
indicate <- function(x, integration="crop", Tb=4.8) {
  
  # Définition des périodes d'intégration
  # semis - levee
  SE <- x$PhenoStage == 1
  # levée - fin maturité
  EH <- (x$PhenoStage > 1 & x$PhenoStage < 6)
  # levée - debut maturité
  EM <- (x$PhenoStage > 1 & x$PhenoStage < 5)
  # levée - floraison
  EF <- (x$PhenoStage == 2 | x$PhenoStage == 3)
  # initiation florale - début maturité
  FIM <- (x$PhenoStage == 3 | x$PhenoStage == 4)
  # floraison - début maturité
  FM <- x$PhenoStage == 4
  # floraison - fin maturité
  FH <- (x$PhenoStage == 4 | x$PhenoStage == 5)
  # début maturité - fin maturité
  MH <- x$PhenoStage == 5
  
  
  switch(integration,
    
    crop = {
      # Calcul des indicateurs
      o <- data.frame(

        # Phenologie
        D_SE = sum(SE),
        D_EF = sum(EF),
        D_FM = sum(FM),
        D_MH = sum(MH),
        TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
        
        # Ressources environnementales
        SGR = sum(x$GR[EH] * 0.48), # PAR
        SRR = sum(x$RR[EH]),
        SPET = sum(x$PET[EH]),
        SCWD = sum(x$RR[EH] - x$PET[EH]),
        
        # Contraintes hydriques
        ## basés sur FTSW
        SFTSW = sum(1 - x$FTSW[EH]),
        NET = sum(x$ETPET[EH] < 0.6),
        SFHTR = sum(1 - x$FHTR[EH]),
        SFHRUE = sum(1 - x$FHRUE[EH]), 
        
        # Contraintes azotées
        # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
        SNNI = sum(1 - x$NNI[EH & x$NNI <1]), #  déficit d'azote 
        SNAB = last(x$NAB[EH]),  # quantité totale d'azote absorbé 
        SFNRUE = sum(1 - x$FNRUE[EH]),
        
        # Contraintes thermiques
        SFTRUE = sum(1 - x$FTRUE[EH]),
        NHT = sum(x$TM[EH] > 28),
        NLT = sum(x$TM[EH] < 20),
        SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
        SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
        
        # Évolution de la surface foliaire
        LAI = max(x$LAI[EH]),
        LAD = sum(x$LAI[EH]), 
        
        # Rayonnement intercepté (PAR)
        SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
        
        # Photosynthèse
        MRUE = mean(x$RUE[EH]),
        
        # Biomasse accumulée
        STDM = max(x$TDM[EH]),
        
        # Performances
        GY = max(x$GY),
        OC = max(x$OC)
      ) 
    },
         
    phase = {       
       # Calcul des indicateurs
       o <- data.frame(
         
         # Phenologie
         D_SE = sum(SE),
         D_EF = sum(EF),
         D_FM = sum(FM),
         D_MH = sum(MH),
         
         # Ressources environnementales
         # Somme de rayonnement
         SGR = sum(x$GR[EH]),
         SGR_EF = sum(x$GR[EF]),
         SGR_FM = sum(x$GR[FM]),
         SGR_MH = sum(x$GR[MH]),
         
         # Cumul de précipitations
         SRR = sum(x$RR[EH]),
         SRR_EF = sum(x$RR[EF]),
         SRR_FM = sum(x$RR[FM]),
         SRR_MH = sum(x$RR[MH]),
         
         # Cumul d'évapotranspiration potentielle
         SPET = sum(x$PET[EH]),
         SPET_EF = sum(x$PET[EF]),
         SPET_FM = sum(x$PET[FM]),
         SPET_MH = sum(x$PET[MH]),
         
         # Déficit hydrique climatique : sum(P-PET)
         SCWD = sum((x$RR-x$PET)[EH]),
         SCWD_EF = sum((x$RR-x$PET)[EF]),
         SCWD_FM = sum((x$RR-x$PET)[FM]),
         SCWD_MH = sum((x$RR-x$PET)[MH]),
         
         # Déficit hydrique édaphique : mean(ET/PET)
         MET = mean(x$ETPET[EH]),
         MET_EF = mean(x$ETPET[EF]),
         MET_FM = mean(x$ETPET[FM]),
         MET_MH = mean(x$ETPET[MH]),
         
         # Déficit hydrique édaphique qualitatif : sum(ET/PET < 0.6)
         NET = sum(x$ETPET[EH] < 0.6),
         NET_EF = sum(x$ETPET[EF] < 0.6), 
         NET_FM = sum(x$ETPET[FM] < 0.6),
         NET_MH = sum(x$ETPET[MH] < 0.6),
         
         # Déficit hydrique édaphique quantitatif : sum(1-FTSW)
         SFTSW = sum(1 - x$FTSW[EH]),
         SFTSW_EF = sum(1 - x$FTSW[EF]), 
         SFTSW_FM = sum(1 - x$FTSW[FM]),
         SFTSW_MH = sum(1 - x$FTSW[MH]),
         
         # Effet de la contrainte hydrique sur la photosynthèse : sum(1-FHRUE)
         SFHRUE = sum(1 - x$FHRUE[EH]),
         SFHRUE_EF = sum(1 - x$FHRUE[EF]), 
         SFHRUE_FM = sum(1 - x$FHRUE[FM]),
         SFHRUE_MH = sum(1 - x$FHRUE[MH]),
         
         # Effet de la contrainte hydrique sur la transpiration : sum(1-FHTR)
         SFHTR = sum(1 - x$FHTR[EH]),
         SFHTR_EF = sum(1 - x$FHTR[EF]), 
         SFHTR_FM = sum(1 - x$FHTR[FM]),
         SFHTR_MH = sum(1 - x$FHTR[MH]),
         
         # Somme de température 
         TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
         TT_SE = sum((x$TM[SE] - Tb)[(x$TM[SE] - Tb) > 0]),
         TT_EF = sum((x$TM[EF] - Tb)[(x$TM[EF] - Tb) > 0]),
         TT_FM = sum((x$TM[FM] - Tb)[(x$TM[FM] - Tb) > 0]),
         TT_MH = sum((x$TM[MH] - Tb)[(x$TM[MH] - Tb) > 0]),       
         
         # Contraintes thermiques
         SFTRUE = sum(1 - x$FTRUE[EH]),
         SFTRUE_EF = sum(1 - x$FTRUE[EF]), 
         SFTRUE_FM = sum(1 - x$FTRUE[FM]),
         SFTRUE_MH = sum(1 - x$FTRUE[MH]),
         
         # heat stress
         NHT = sum(x$TM[EH] > 28),
         NHT_EF = sum(x$TM[EF] > 28),
         NHT_FM = sum(x$TM[FM] > 28),
         NHT_MH = sum(x$TM[MH] > 28),
         SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
         SHT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="high")),
         SHT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="high")),
         SHT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="high")),
         
         # cold stress
         NLT = sum(x$TM[EH] < 20),
         NLT_EF = sum(x$TM[EF] < 20),
         NLT_FM = sum(x$TM[FM] < 20),
         NLT_MH = sum(x$TM[MH] < 20),
         SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
         SLT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="low")),
         SLT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="low")),
         SLT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="low")),
         
         # nitrogen stress 
         # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
         # absorbed nitrogen
         SNAB = max(x$NAB[EH]),
         SNAB_EF = max(x$NAB[EF]),
         SNAB_FM = max(x$NAB[FM]),
         SNAB_EM = max(x$NAB[EM]),
         SNAB_MH = max(x$NAB[MH]),
         
         # nitrogen nutrition index 
         SNNI = sum(1 - x$NNI[EH & x$NNI <1]),
         SNNI_EF = sum(1 - x$NNI[EF & x$NNI <1]),
         SNNI_FM = sum(1 - x$NNI[FM & x$NNI <1]),
         SNNI_MH = sum(1 - x$NNI[MH & x$NNI <1]),
         
         # nitrogen impact on phytosynthesis
         SFNRUE = sum(1 - x$FNRUE[EH]),
         SFNRUE_EF = sum(1 - x$FNRUE[EF]),
         SFNRUE_FM = sum(1 - x$FNRUE[FM]),
         SFNRUE_MH = sum(1 - x$FNRUE[MH]),
      
         # NNI at flowering
         NNI_F = x$NNI[FM][1],
         
         # Nombre de jours INN < 0.8 jusqu'à M0
         NNNID_EM = sum(x$NNI[EM] < 0.8),
         NNNIE_EM = sum((x$NNI[EM] > 1.2) & (x$NNI[EM] < 2)),
         
         # Indice foliaire maximum
         LAI = max(x$LAI),
         
         # Durée de surface foliaire : sum(x$LAI)
         LAD = sum(x$LAI[EH]),
         
         # Rayonnement intercepté (PAR)
         SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
         SIR_EF = sum(x$RIE[EF] * x$GR[EF] * 0.48),
         SIR_FM = sum(x$RIE[FM] * x$GR[FM] * 0.48),
         SIR_MH = sum(x$RIE[MH] * x$GR[MH] * 0.48),
         
         # Photosynthèse
         MRUE = mean(x$RUE[EH]),
         
         # Biomasse
         STDM = max(x$TDM[EH]),
         STDM_F = x$TDM[FM][1],
         
         # Performance
         GY = max(x$GY),
         OC = max(x$OC)
       ) 
     }         
  )
  return(o)
}


# Visualize timed output variables
#' @export display
display <- function(data, view="timed") {
  switch(
    view,
    timed = {
      data %>% 
        gather(variable, value, -time, factor_key=TRUE) %>% 
        ggplot(aes(x=time, y=value)) +
        geom_line() +
        facet_wrap(~ variable, scales="free")     
    }
  )
}

# Define the sunflo model
sunflo_model <- function(design_row, climate_data) {
  # Extract inputs from the design row
  root_depth <- design_row$root_depth
  stone_content <- design_row$stone_content
  field_capacity_1 <- design_row$field_capacity_1
  wilting_point_1 <- design_row$wilting_point_1
  field_capacity_2 <- design_row$field_capacity_2
  wilting_point_2 <- design_row$wilting_point_2
  soil_density_1 <- design_row$soil_density_1
  soil_density_2 <- design_row$soil_density_2
  crop_sowing <- design_row$crop_sowing
  crop_harvest <- design_row$crop_harvest
  crop_density <- design_row$crop_density
  HI <- design_row$HI  # Harvest Index
  
  # Compute soil water capacity
  swc <- soil_water_capacity(
    root_depth = root_depth,
    stone_content = stone_content,
    field_capacity_1 = field_capacity_1,
    field_capacity_2 = field_capacity_2,
    wilting_point_1 = wilting_point_1,
    wilting_point_2 = wilting_point_2,
    soil_density_1 = soil_density_1,
    soil_density_2 = soil_density_2
  )
  
  # Compute thermal time
  thermal_sum <- thermal_time(
    climate = climate_data,
    id = design_row$id,
    start = crop_sowing,
    end = crop_harvest
  )
  
  # Simulate crop yield
  crop_biomass <- swc * thermal_sum * crop_density  # Simplified biomass calculation
  crop_yield <- crop_biomass * HI  # Yield based on harvest index
  
  # Return results
  return(list(
    soil_water_capacity = swc,
    thermal_time = thermal_sum,
    crop_biomass = crop_biomass,
    crop_yield = crop_yield
  ))
}