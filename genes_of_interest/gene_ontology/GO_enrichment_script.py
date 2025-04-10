from goatools.obo_parser import GODag
from goatools.associations import read_gaf
from goatools.go_enrichment import GOEnrichmentStudy
from collections import defaultdict

# Load GO ontology
go_obo = "go.obo"
obodag = GODag(go_obo)

# Load gene2go associations from your GAF file
gaf_file = "HanXRQr2.0-SUNRISE.gaf"
gene2go = read_gaf(gaf_file, prt=None)

# Load background genes
with open("background_genes.txt") as f:
    background_genes = set(line.strip() for line in f)

# Load DEGs (your study genes)
with open("deg_list.txt") as f:
    deg_genes = set(line.strip() for line in f)

# Run GO enrichment
goea = GOEnrichmentStudy(
    background_genes,  # population (all genes tested)
    gene2go,           # gene2go associations
    obodag,            # GO DAG
    methods=["fdr_bh"],# multiple testing correction
    propagate_counts=True,
    alpha=0.05,
)

results = goea.run_study(deg_genes)

# Print significant results
for r in results:
    if r.p_fdr_bh < 0.05:
        print(f"{r.GO} ({r.name}) | p={r.p_fdr_bh:.4g} | {r.study_count}/{r.study_n} in study")

