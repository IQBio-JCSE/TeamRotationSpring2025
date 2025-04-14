from goatools.obo_parser import GODag
from goatools.associations import read_gaf
from goatools.go_enrichment import GOEnrichmentStudy
from collections import defaultdict

# Load GO ontology
go_obo = "go-basic.obo"
obodag = GODag(go_obo)

# Load gene2go associations from your GAF file
gaf_file = "GCF_002127325.2_HanXRQr2.0-SUNRISE_gene_ontology.gaf" #TODO: make this var we can pass in 
gene2go = read_gaf(gaf_file, prt=None)

# Load background genes
with open("gene_list_all.csv") as f:
    background_genes = set(line.strip() for line in f)

deg_file = "filtered_genes.csv" #TODO: make this var we can pass in 
# Load DEGs (your study genes)
with open(deg_file) as f:
    degs = set(line.strip().replace('"','') for line in f)

# Run GO enrichment
goea = GOEnrichmentStudy(
    background_genes,  # population (all genes tested)
    gene2go,           # gene2go associations
    obodag,            # GO DAG
    methods=["fdr_bh"],# multiple testing correction
    propagate_counts=True,
    alpha=0.25,
)

#results = goea.run_study(degs)

# Print significant results
# TODO: modify this to adjust p-val, write output to file
# for r in results:
#     if r.p_fdr_bh < 0.05:
#         print(f"{r.GO} ({r.name}) | p={r.p_fdr_bh:.4g} | {r.study_count}/{r.study_n} in study")

