from goatools.obo_parser import GODag

# --- Step 1: Load GO DAG (Gene Ontology structure)
go_dag = GODag("go-basic.obo")  

print("GO DAG loaded successfully.")

# --- Step 2: Load lists from files
# Your 'wanted' GO terms (e.g., related to stress, defense, etc.)
with open("wanted_go_terms.txt") as f:
    wanted_terms = set(line.strip() for line in f if line.strip())

# Your list of GO terms from significantly expressed genes
with open("go_ids.txt") as f:
    significant_terms = set(line.strip() for line in f if line.strip())

# --- Step 3: Track whether each term is original or a child
go_origin = {}

for go_id in wanted_terms:
    if go_id in go_dag:
        term = go_dag[go_id]
        go_origin[go_id] = "original"
        for child_id in term.get_all_children():
            # Only label as child if not already marked as original
            if child_id not in go_origin:
                go_origin[child_id] = "child"

# --- Step 4: Filter significant GO terms
filtered_terms = significant_terms.intersection(go_origin.keys())

# --- Step 5: Save filtered terms with annotation to file
with open("filtered_significant_go_terms_labeled.txt", "w") as out:
    out.write("GO_ID\tCategory\tGO_Name\n")
    for go_id in sorted(filtered_terms):
        origin = go_origin.get(go_id, "unknown")
        name = go_dag[go_id].name if go_id in go_dag else "NA"
        out.write(f"{go_id}\t{origin}\t{name}\n")
print(f"{len(filtered_terms)} matching GO terms found.")
