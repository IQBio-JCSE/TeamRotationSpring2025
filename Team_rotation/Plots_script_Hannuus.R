
#### Load libraries and data #### 
library(readr)       
library(dplyr)       
library(tibble)      
library(DESeq2)      
library(ggplot2)     
library(pheatmap) 
library(tidyverse)
library(tidyr)

# Set a working directory 
setwd("/scratch/Users/cava3224/team_rotation/github_repo/Team_rotation")

# Delete enviroment and load counts and tpm data
load("results/DESeq2_data.RData")
load("results/results_tpm.RData")
load("results/results_counts.RData")
load("results/data_files.RData")
load("results/summary_stats_files.RData")
load("results/terpenoid_files.RData")
load("results/counts_data.Rdata")

#### Upload terpenoid synthesis genes list ####
## Note this section contains the generation of the terpenoid data already loaded in the beginning ##
terpenoid_genes <- read_tsv("terpenoid_synthesis__genes_of_interest.tsv")


# Select genes from the significant list that match the terpenoid synthesis genes
terpenoid_genes_list <- filtered_genes %>%
  filter(gene_name %in% terpenoid_genes$ID)

# Using that list, set apart those counts and tpm values for plotting

tpm_signif_expr <- tpm_filtered[rownames(tpm_filtered) %in% filtered_genes$gene_name, ]

terpenoid_sig_counts <- filtered_results[filtered_results$gene_id %in% terpenoid_genes_list$gene_name, ]

tpm_sig_expr_with_genes <- rownames_to_column(tpm_signif_expr, var = "gene_ID")

terpenoid_sig_tpm <- tpm_sig_expr_with_genes[tpm_sig_expr_with_genes$gene_ID %in% terpenoid_genes_list$gene_name, ]

# Add a column to indicate up or down regulation to look for KEGG pathway later
P_VALUE_FILTER   <- 0.01
LOG2_FOLD_FILTER <- 1.5

terpenoid_sig_counts$regulation <- case_when(
  terpenoid_sig_counts$padj < P_VALUE_FILTER & terpenoid_sig_counts$log2FoldChange > LOG2_FOLD_FILTER  ~ "Upregulated",
  terpenoid_sig_counts$padj < P_VALUE_FILTER & terpenoid_sig_counts$log2FoldChange < -LOG2_FOLD_FILTER ~ "Downregulated",
  TRUE ~ "Not significant")

terpenoid_genes_list$regulation <- terpenoid_sig_counts$regulation[match(terpenoid_genes_list$gene_name, terpenoid_sig_counts$gene_id)]

# Delete not significant genes 
terpenoid_genes_list <- terpenoid_genes_list %>%
  filter(regulation != "Not significant")

# Save dataset as an .Rdata file to load it easily later
save(terpenoid_genes, terpenoid_genes_list, terpenoid_sig_counts, terpenoid_sig_tpm
     , file = "results/terpenoid_files.RData")

# Save this matrix as a .csv file for easier sharing 
write.csv(terpenoid_genes_list, "terpenoid_sig_genes.csv", row.names = FALSE)

# More complete dataset with all the KEGG IDs
terpenoid_KEGG <- terpenoid_genes_list %>%
  left_join(terpenoid_genes, by = c("gene_name" = "ID"))

# Save dataset as a .csv file 
write.csv(terpenoid_KEGG, "terpenoid_KEGG_genes.csv", row.names = FALSE)

# Reset plotting computer
dev.off()

#### PCA plot ####
# rlog transformation for normalization
rld <- rlog(dds, blind = TRUE)

# Extract PCA data
pcaData <- plotPCA(rld, intgroup = "ecology", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
pcaData$sample <- rownames(pcaData)

# PCA plot with no ellipses 
ggplot(pcaData, aes(x = PC1, y = PC2, color = ecology)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA of Sunflower Samples",
       x = paste0("PC1 (", percentVar[1], "% variance)"),
       y = paste0("PC2 (", percentVar[2], "% variance)")) +
  scale_color_manual(values = c("domesticated" = "dodgerblue3", "wild" = "firebrick3")) +
  theme_minimal()

ggsave("results/PCA_wild_v_dom.png", width = 8, height = 6, dpi = 300)

# PCA plot with ellipses 
ggplot(pcaData, aes(x = PC1, y = PC2, color = ecology)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = ecology), type = "t", linetype = "dashed", size = 0.8) +
  labs(title = "PCA of Sunflower Samples",
       x = paste0("PC1 (", percentVar[1], "% variance)"),
       y = paste0("PC2 (", percentVar[2], "% variance)")) +
  scale_color_manual(values = c("domesticated" = "dodgerblue3", "wild" = "firebrick3")) +
  theme_minimal()

ggsave("results/PCA_wild_v_dom_ellipses.png", width = 8, height = 6, dpi = 300)

#### Volcano plot all genes ####

# Setting more stringent values 
P_VALUE_FILTER   <- 0.01
LOG2_FOLD_FILTER <- 1.5

volcano_data <- results %>%
  mutate(status = case_when(
    padj < P_VALUE_FILTER & log2FoldChange > LOG2_FOLD_FILTER  ~ "Upregulated in Wild",
    padj < P_VALUE_FILTER & log2FoldChange < -LOG2_FOLD_FILTER ~ "Downregulated in Wild",
    TRUE ~ "Not significant"
  ))

ggplot(volcano_data, aes(x = log2FoldChange, y = -log10(padj))) +
  geom_point(aes(color = status), alpha = 0.8, size = 1.5) +
  scale_color_manual(values = c("Upregulated in Wild" = "steelblue", 
                                "Downregulated in Wild" = "firebrick", 
                                "Not significant" = "gray70")) +
  theme_minimal() +
  theme(
    text = element_text(size = 12)
  ) +
  labs(title = "Volcano Plot- Wild vs Domesticated Sunflowers",
       x = "log2(Fold Change)",
       y = "-log10(Adjusted p-value)",
       color = "Gene Regulation") +
  geom_vline(xintercept = c(-LOG2_FOLD_FILTER, LOG2_FOLD_FILTER), linetype = "dashed", color = "black") +
  geom_hline(yintercept = -log10(P_VALUE_FILTER), linetype = "dashed", color = "black")

ggsave("results/volcano_plot_sig_genes.png", width = 8, height = 6, dpi = 300)

####Heatmap for tepenoid significant genes ####
rownames(terpenoid_sig_tpm) <- terpenoid_sig_tpm$gene_ID
terpenoid_sig_tpm <- terpenoid_sig_tpm %>%
  select(-gene_ID)

log_tpm_matrix <- log2(terpenoid_sig_tpm + 1)
# Add gene_names and select names from all genes list (g2s)
log_tpm_matrix_gene_names <- log_tpm_matrix %>% rownames_to_column("gene_id") %>% merge(g2s)

# Extract the only column from filtered_genes as a vector for easier manipulation
filtered_genes_vector <- filtered_genes[[1]]

# Filter to significant genes object called sig_log_tpm_matrix
sig_log_tpm_matrix <- log_tpm_matrix_gene_names[log_tpm_matrix_gene_names$gene_name %in% filtered_genes_vector, ]

# Remove all observations with missing data (NA)
sig_log_tpm_matrix <- na.omit(sig_log_tpm_matrix)

# Remove non numeric data for plotting heatmap
sig_log_tpm_matrix_numeric <- sig_log_tpm_matrix %>% select(-gene_id, -gene_name)

# See if there is data 
dim(sig_log_tpm_matrix_numeric)

# Setting custom clustering and distance functions
custom_dist <- function(x) dist(x, method = "euclidian") 
custom_hclust <- function(x) hclust(x, method = "ward.D2") 

# Set the scale
breaks <- seq(-2, 2, length.out = 51)

# Plot heatmap for log transformed significant genes using custom clustering and distance methods, and breaks
pheatmap(sig_log_tpm_matrix_numeric, 
         cluster_rows = TRUE,  
         cluster_cols = TRUE,  
         scale = "row",        
         show_rownames = FALSE, 
         show_colnames = TRUE,  
         main = "Heatmap of Log2-Transformed TPM Values in Significant Genes in the Terpenoid Pathway",
         distfun = custom_dist,
         color = colorRampPalette(c("purple", "white", "yellow"))(50),
         hclustfun = custom_hclust,
         breaks = breaks,
        filename ="results/heatmap_terpenoid_sig_genes_log_tpm.png"
)

#### GHI Data plots ####
## Minnesota
# Read the data
Minnesota_GHI_1998 <- read.csv("climate_data/averaged_Minnesota_GHI_1998.csv")  # replace with your actual file name
Minnesota_GHI_2001 <- read.csv("climate_data/averaged_Minnesota_GHI_2001.csv") 
Minnesota_GHI_2004 <- read.csv("climate_data/averaged_Minnesota_GHI_2004.csv") 
Minnesota_GHI_2007 <- read.csv("climate_data/averaged_Minnesota_GHI_2007.csv") 
Minnesota_GHI_2010 <- read.csv("climate_data/averaged_Minnesota_GHI_2010.csv") 
Minnesota_GHI_2013 <- read.csv("climate_data/averaged_Minnesota_GHI_2013.csv") 
Minnesota_GHI_2016 <- read.csv("climate_data/averaged_Minnesota_GHI_2016.csv") 
Minnesota_GHI_2019 <- read.csv("climate_data/averaged_Minnesota_GHI_2019.csv") 
Minnesota_GHI_2023 <- read.csv("climate_data/averaged_Minnesota_GHI_2023.csv") 

Minnesota_all <- bind_rows(Minnesota_GHI_1998, Minnesota_GHI_2001, Minnesota_GHI_2004,
                           Minnesota_GHI_2007, Minnesota_GHI_2010, Minnesota_GHI_2013,
                           Minnesota_GHI_2016, Minnesota_GHI_2019, Minnesota_GHI_2023)
  
# Aggregate average GHI by year
yearly_ghi_Minnesota <- aggregate(avg_GHI ~ year, Minnesota_all, mean)


merged_df_Minnesota <- Minnesota_all %>%
  mutate(
    date = make_date(year, month, day),
    doy = yday(date)  # day of year (1 to 365/366)
  )

ggplot(merged_df_Minnesota, aes(x = doy, y = avg_GHI, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  labs(
    title = "Daily GHI Distribution per Year",
    x = "Day of Year",
    y = "Average GHI",
    fill = "Year"
  ) +
  theme_minimal()

ggplot(merged_df_Minnesota, aes(x = doy, y = avg_GHI)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 1) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Daily GHI Distribution by Year",
    x = "Day of Year",
    y = "Average GHI"
  ) +
  theme_minimal()

## Georgia
Georgia_GHI_1998 <- read.csv("climate_data/averaged_Georgia_GHI_1998.csv")  # replace with your actual file name
Georgia_GHI_2001 <- read.csv("climate_data/averaged_Georgia_GHI_2001.csv") 
Georgia_GHI_2004 <- read.csv("climate_data/averaged_Georgia_GHI_2004.csv") 
Georgia_GHI_2007 <- read.csv("climate_data/averaged_Georgia_GHI_2007.csv") 
Georgia_GHI_2010 <- read.csv("climate_data/averaged_Georgia_GHI_2010.csv") 
Georgia_GHI_2013 <- read.csv("climate_data/averaged_Georgia_GHI_2013.csv") 
Georgia_GHI_2016 <- read.csv("climate_data/averaged_Georgia_GHI_2016.csv") 
Georgia_GHI_2019 <- read.csv("climate_data/averaged_Georgia_GHI_2019.csv") 
Georgia_GHI_2023 <- read.csv("climate_data/averaged_Georgia_GHI_2023.csv") 

Georgia_all <- bind_rows(Georgia_GHI_1998, Georgia_GHI_2001, Georgia_GHI_2004,
                           Georgia_GHI_2007, Georgia_GHI_2010, Georgia_GHI_2013,
                           Georgia_GHI_2016, Georgia_GHI_2019, Georgia_GHI_2023)


# Aggregate average GHI by year
yearly_ghi_Georgia <- aggregate(avg_GHI ~ year, Georgia_all, mean)


merged_df_Georgia <- Georgia_all %>%
  mutate(
    date = make_date(year, month, day),
    doy = yday(date)  # day of year (1 to 365/366)
  )

ggplot(merged_df_Georgia, aes(x = doy, y = avg_GHI, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  labs(
    title = "Daily GHI Distribution per Year",
    x = "Day of Year",
    y = "Average GHI",
    fill = "Year"
  ) +
  theme_minimal()

ggplot(merged_df_Georgia, aes(x = doy, y = avg_GHI)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 1) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Daily GHI Distribution by Year",
    x = "Day of Year",
    y = "Average GHI"
  ) +
  theme_minimal()


#### Plotting summary statistics means ####
print(summary_stats_all_genes)

# Bar graph with mean for all subcategories of genes
ggplot(summary_stats_all_genes, aes(x = id, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean log2FC for gene subsets",
       y = "Mean log2FC",
       x = "Gene subset")

# Bar graph with mean for all terpenoid subcategories 
ggplot(summary_stats_terpenoid_genes, aes(x = id, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean log2FC for gene subsets",
       y = "Mean log2FC",
       x = "Gene subset")

# # Bar graph with the min and max values for each of the subcategories of genes 
# ggplot(summary_stats_terpenoid_genes, aes(x = id, y = mean)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
#   geom_text(aes(y = max, label = round(max,1)), vjust = -0.5, color = "red") +
#   geom_text(aes(y = min, label = round(min,1)), vjust = 1.5, color = "blue") +
#   theme_minimal() +
#   labs(title = "Mean log2FC with SD, min, and max",
#        y = "log2FC",
#        x = "Gene subset")

#### Histograms of counts distribution ####
# Get rid of samples that =0 in all significant genes 
wild_df_filtered <- wild_df_counts[rowSums(wild_df_counts[ , -1]) > 0, ]
domesticated_df_filtered <- domesticated_df_counts[rowSums(domesticated_df_counts[ , -1]) > 0, ]

# Calculate row sums 
domesticated_df_filtered$total_counts_sums <- rowSums(domesticated_df_filtered[ , -1])
wild_df_filtered$total_counts_sums <- rowSums(wild_df_filtered[ , -1])

# Only show overlapping genes between wild and domesticated species
overlapping_genes <- intersect(wild_df_filtered[[1]], domesticated_df_filtered[[1]])

# Select final data frames for plotting 
wild_df_final <- wild_df_filtered[wild_df_filtered[[1]] %in% overlapping_genes, ]
domesticated_df_final <- domesticated_df_filtered[domesticated_df_filtered[[1]] %in% overlapping_genes, ]

# Set output file path (adjust path and filename as needed)
png("results/gene_counts_histogram_wild.png", width = 800, height = 600)

# Plot row sums 
hist(log10(wild_df_final$total_counts_sums + 1),
     main = "Wild gene counts distribution (log10)",
     xlab = "log10(Total counts + 1)",
     col = "lightblue",
     border = "white",
     breaks = 50, 
     xlim = c(0, 7))

dev.off()

# Set output file path (adjust path and filename as needed)
png("results/gene_counts_histogram_domesticated.png", width = 800, height = 600)

hist(log10(domesticated_df_final$total_counts_sums + 1),
     main = "Domesticated gene counts distribution (log10)",
     xlab = "log10(Total counts + 1)",
     col = "salmon",
     border = "white",
     breaks = 50,
     xlim = c(0, 7))

dev.off()

# Check if it follows a normal distribution
qqnorm(log10(wild_df_final$total_counts_mean + 1))
qqline(log10(wild_df_final$total_counts_mean + 1), col = "red")

qqnorm(log10(domesticated_df_final$total_counts_mean + 1))
qqline(log10(domesticated_df_final$total_counts_mean + 1), col = "red")


# Generate graphs with normal distribution
# Wild 
png("results/gene_counts_histogram_wild_norm.png", width = 800, height = 600)

hist(log10(wild_df_final$total_counts_mean + 1), 
     main = "Wild Counts Normal Fit", 
     xlab = "log10(Total counts + 1)", 
     col = "lightblue", 
     border = "white", 
     breaks = 50, 
     freq = FALSE)  # Important: use density instead of counts

# Calculate mean and SD of your log-transformed data
mu <- mean(log10(wild_df_final$total_counts_mean + 1))
sigma <- sd(log10(wild_df_final$total_counts_mean + 1))

# Overlay normal density curve
curve(dnorm(x, mean = mu, sd = sigma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
dev.off()

# Doemsticated 
png("results/gene_counts_histogram_domesticated_norm.png", width = 800, height = 600)

hist(log10(domesticated_df_final$total_counts_sums + 1),
     main = "Domesticated Counts Normal Fit",
     xlab = "log10(Total counts + 1)",
     col = "salmon",
     border = "white",
     breaks = 50,
     freq = FALSE)

# Calculate mean and SD of your log-transformed data
mu <- mean(log10(domesticated_df_final$total_counts_mean + 1))
sigma <- sd(log10(domesticated_df_final$total_counts_mean + 1))

# Overlay normal density curve
curve(dnorm(x, mean = mu, sd = sigma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
dev.off()
# 
# # Separate counts for terpenoid pathway genes
# terpenoid_gene_col <- terpenoid_genes_list$gene_name
# 
# # Select final data frames for plotting
# wild_df_terpenoid <- wild_df_filtered[wild_df_filtered[[1]] %in% terpenoid_gene_col, ]
# domesticated_df_terpenoid <- domesticated_df_counts[domesticated_df_counts[[1]] %in% terpenoid_gene_col, ]
# 
# domesticated_df_terpenoid$total_counts_mean <- rowMeans(domesticated_df_terpenoid[ , -1])
# domesticated_df_terpenoid$total_counts_sums <- rowSums(domesticated_df_terpenoid[ , -1])
# 
# hist(wild_df_terpenoid$total_counts_sums,
#      main = "Wild terpenoid gene counts distribution (log10)",
#      xlab = "log10(Total counts + 1)",
#      col = "lightblue",
#      border = "white",
#      breaks = 50)

