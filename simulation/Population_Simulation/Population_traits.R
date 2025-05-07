# trait expression 
wild_population_trait <- read.csv("Team_rotation/results/summary_stats_all_up_wild_terpenoid_counts.csv")
normalize <- function(value ,min_value, max_value){
  return((value - min_value) / (max_value - min_value))
}

terpinoid_level <- seq(, 3, by = 0.5)
for (level in 1:length(terpinoid_level)) {
  print(paste(terpinoid_level[level], normalize(terpinoid_level[level], min(terpinoid_level), max(terpinoid_level))))
}
  






wild_mean <- normalize(value = wild_population_trait$mean, 
                       min_value = wild_population_trait$min,
                       max_value = wild_population_trait$max)
wild_sd <- normalize(value = wild_population_trait$sd, 
                     min_value = wild_population_trait$min,
                     max_value = wild_population_trait$max)



# domesticated_population_trait <- read.csv("Team_rotation/results/summary_stats_all__up_domesticated_terpenoid_counts.csv")
# domesticated_mean <- normalize(value = domesticated_population_trait$mean, 
#                                min_value = domesticated_population_trait$min,
#                                max_value = domesticated_population_trait$max)
# 
# 
# 
# domesticated_sd <- normalize(value = domesticated_population_trait$sd, 
#                              min_value = domesticated_population_trait$min,
#                              max_value = domesticated_population_trait$max)
# 
# mean 199.140935294118
# median 13
# sd 505.76268925222
# min 0
# max 3240
# total_counts 37239.3549
# num_genes 17
# num_samples 11
# num_values 187



domesticated_mean <- normalize(value = 199.140935294118, 
                       min_value = 0,
                       max_value = 3240)



domesticated_sd <- normalize(value = 505.76268925222, 
                               min_value = 0,
                               max_value = 3240)
