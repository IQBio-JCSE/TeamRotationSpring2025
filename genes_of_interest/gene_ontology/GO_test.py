deg_file = "filtered_genes.csv"
go_file = "GCF_002127325.2_HanXRQr2.0-SUNRISE_gene_ontology.gaf"
output_file = "filtered_genes_GO_annotations.tsv"

# Load DEGs
with open(deg_file) as f:
    degs = set(line.strip().replace('"','') for line in f)
    print(degs)

# Parse GAF and extract relevant GO terms
with open(go_file) as fin, open(output_file, "w") as fout:
    fout.write("GeneID\tGO_ID\tAspect\tEvidence\tReference\n")
    for line in fin:
        if line.startswith("!"):  # skip header lines
            continue
        parts = line.strip().split("\t")
        if len(parts) < 15:
            continue
        gene_id = parts[2]
        go_id = parts[4]
        aspect = parts[8]
        evidence = parts[6]
        reference = parts[5]
        if gene_id in degs:
            fout.write(f"{gene_id}\t{go_id}\t{aspect}\t{evidence}\t{reference}\n")

