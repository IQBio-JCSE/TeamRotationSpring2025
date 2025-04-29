
#### Load libraries and data #### 
library(readr)       
library(dplyr)       
library(tibble)      
library(DESeq2)      
library(ggplot2)     
library(pheatmap) 

# Set a working directory 
setwd("/scratch/Users/cava3224/team_rotation/github_repo/Team_rotation")

# Delete enviroment and load counts and tpm data
load("results/DESeq2_data.RData")
load("results/results_tpm.RData")
load("results/results_counts.RData")
load("results/data_files.RData")

#### Upload terpenoid synthesis genes list ####
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

# Save dataset as a .csv file 
write.csv(terpenoid_genes_list, "terpenoid_sig_genes.csv", row.names = FALSE)

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


