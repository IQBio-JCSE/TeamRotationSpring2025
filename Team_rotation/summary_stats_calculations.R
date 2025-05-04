
#### Load libraries and data ####
library(dplyr)

load("results/DESeq2_data.RData")
load("results/results_tpm.RData")
load("results/results_counts.RData")
load("results/data_files.RData")
load("results/terpenoid_files.RData")
# Terpenoid sig genes list
terpenoid_sig_genes <- read.csv("terpenoid_sig_genes.csv")

#### Function for summary statistics for genes ####
# Setting stringent thresholds
P_VALUE_FILTER   <- 0.01
LOG2_FOLD_FILTER <- 1.5

# Function to summarize log2FC for a vector of gene IDs
summarize_log2FC <- function(df, gene_ids) {
  df %>%
    filter(gene_id %in% gene_ids) %>%
    summarise(
      count = n(),
      mean = mean(log2FoldChange, na.rm = TRUE),
      median = median(log2FoldChange, na.rm = TRUE),
      sd = sd(log2FoldChange, na.rm = TRUE),
      min = min(log2FoldChange, na.rm = TRUE),
      max = max(log2FoldChange, na.rm = TRUE)
    )
}

#### Summary statistics calculations using function ####

## Create variables with all genes of interest
all_genes <- filtered_results$gene_id
all_up_genes <- filtered_results %>% filter(log2FoldChange > LOG2_FOLD_FILTER) %>% pull(gene_id)
all_down_genes <- filtered_results %>% filter(log2FoldChange < -LOG2_FOLD_FILTER) %>% pull(gene_id)

# Apply the function to the genes of interest results
summary_all_genes <- summarize_log2FC(filtered_results, all_genes)
summary_all_up <- summarize_log2FC(filtered_results, all_up_genes)
summary_all_down <- summarize_log2FC(filtered_results, all_down_genes)

# Combining all the summaries into one dataset to commit to github
summary_stats_all_genes <- bind_rows(list(
  "all" = summary_all_genes,
  "down" = summary_all_down,
  "up" = summary_all_up
),
.id = "id")

## All genes in the terpenoid pathway
terpenoid_all_genes <- terpenoid_genes$ID
terpenoid_all_sig_genes <- terpenoid_genes_list$gene_name
terpenoid_up_genes <- terpenoid_genes_list$gene_name[terpenoid_genes_list$regulation == "Upregulated"]
terpenoid_down_genes <- terpenoid_genes_list$gene_name[terpenoid_genes_list$regulation == "Downregulated"]

# Apply the function to the genes of interest results
summary_all_terpenoid_genes <- summarize_log2FC(results, terpenoid_all_genes)
summary_all_terpenoid_sig_genes <- summarize_log2FC(filtered_results, terpenoid_all_sig_genes)
summary_terpenoid_up <- summarize_log2FC(filtered_results, terpenoid_up_genes)
summary_terpenoid_down <- summarize_log2FC(filtered_results, terpenoid_down_genes)

# Genes that are in the beta-ocimene synthase
# First filter by the subset of genes present in the pathway
KEGG_ids_of_interest_ocimene <- c(
  "K12742",
  "K01662",
  "K00099",
  "K00991",
  "K00919",
  "K01770",
  "K03526",
  "K01823",
  "K03527"
)

filtered_KEGG_ids_of_interest_ocimene_df <- terpenoid_genes %>%
  mutate(ORTHOLOGY = sub(";.*", "", ORTHOLOGY)) %>%
  filter(ORTHOLOGY %in% KEGG_ids_of_interest_ocimene)

# Include genes of the pathway I want, in this case, beta-ocimene synthase
ocimene_pathway_genes <- as.array(filtered_KEGG_ids_of_interest_ocimene_df$ID)
summary_ocimene_pathway <- summarize_log2FC(results, ocimene_pathway_genes)

# Save all terpenoid related summary statistics in a dataframe 

summary_stats_terpenoid_genes <- bind_rows(list(
  "all_terpenoid" = summary_all_terpenoid_genes,
  "down_terpeniod" = summary_terpenoid_down,
  "up_terpenoid" = summary_terpenoid_up,
  "all_sig_terpenoid" = summary_all_terpenoid_sig_genes, 
  "ocimene_pathway_genes" = summary_ocimene_pathway
),
.id = "id")

# Save files for graphs 
save(summary_stats_all_genes,summary_stats_terpenoid_genes, file = "results/summary_stats_files.RData")

# Save files for github push
write.csv(summary_stats_all_genes, file='results/summary_stats_all_genes.csv', row.names=FALSE)
write.csv(summary_stats_terpenoid_genes, file='results/summary_stats_terpenoid_genes', row.names=FALSE)

#### Extra commands that mught be useful ####

# terpenoid_genes_in_filtered_genes <- terpenoid_all_genes_df %>% filter(terpenoid_all_genes_df$terpenoid_all_genes %in% filtered_genes$gene_name)
# 
# genes_present <- terpenoid_all_genes %in% results$gene_name
# 
# # Count how many are present
# sum(genes_present)
# 
# # View missing genes
# missing_genes <- terpenoid_all_genes[!genes_present]
# missing_genes

