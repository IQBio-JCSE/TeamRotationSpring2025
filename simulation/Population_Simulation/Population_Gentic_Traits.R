# min-max normalization
normalize <- function(value ,min_value, max_value){
  return((value - min_value) / (max_value - min_value))
}


# trait expression 
wild_population_trait <- read.csv("Team_rotation/results/summary_stats_all_up_wild_terpenoid_counts.csv")
wild_mean <- normalize(value = wild_population_trait$mean, 
                       min_value = wild_population_trait$min,
                       max_value = wild_population_trait$max)
wild_median <- normalize(value = wild_population_trait$median, 
                       min_value = wild_population_trait$min,
                       max_value = wild_population_trait$max)
wild_sd <- normalize(value = wild_population_trait$sd, 
                     min_value = wild_population_trait$min,
                     max_value = wild_population_trait$max)



domesticated_population_trait <- read.csv("Team_rotation/results/summary_stats_all_down_domesticated_terpenoid_counts.csv")
domesticated_mean <- normalize(value = domesticated_population_trait$mean,
                               min_value = domesticated_population_trait$min,
                               max_value = domesticated_population_trait$max)
domesticated_median <- normalize(value = domesticated_population_trait$median,
                               min_value = domesticated_population_trait$min,
                               max_value = domesticated_population_trait$max)
domesticated_sd <- normalize(value = domesticated_population_trait$sd,
                             min_value = domesticated_population_trait$min,
                             max_value = domesticated_population_trait$max)

# hacky way to use values Jenna used in test runs of sunflo
# terpinoid_level <- seq(, 3, by = 0.5)
# for (level in 1:length(terpinoid_level)) {
#   print(paste(terpinoid_level[level], normalize(terpinoid_level[level], min(terpinoid_level), max(terpinoid_level))))
# }