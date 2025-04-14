#!/Users/jennastanislaw/miniconda/envs/jcse/bin/python

import pandas as pd

# Hard-coded input and output paths
infile="HanXRQr2.0-SUNRISE_cds_gene_info.txt"
outfile="HanXRQr2.0-SUNRISE_cds_gene_info.csv"

# Read in lines
lines = open(infile,"r").read().splitlines()

all_data=list()

for line in lines:
    # print(line)
    outdict = dict()

    # The first element in each line is not in brackets, so extract it
    # Also remove the ">lcl|" that is at the beginning of every line
    name_end_i = line.find("[")
    name = line[:name_end_i]
    remainder = line[name_end_i:]
    outdict["id"] = name.replace(">lcl|","").strip()

    # Split the rest of the lines. Can't just split on brackets or spaces
    # because some labels contain brackets and spaces
    labels=remainder.split("] [")
    #print(labels)
    for item in labels:
        # Cut off beginning and ending brackets if needed
        if item.startswith("["):
            item = item[1:]
        if item.endswith("]"):
            item = item[:-1]
        # print(item)

        # Save to dictinoary. One dict per entry
        category, val = tuple(item.split("="))
        outdict[category] = val
    
    all_data.append(outdict)

# Put all dicts for all lines into a df
df = pd.DataFrame(all_data)

# Add an extra column to get the NCBI GeneID alone from the db_xref column
df["NCBI_GeneID"] = df["db_xref"].str.split(":", expand=True)[1]

# print(df)
# assert False

df.to_csv(outfile)
