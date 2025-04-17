#!/Users/jennastanislaw/miniconda/envs/jcse/bin/python

import pandas as pd

# Hard-coded input and output paths
infile="HanXRQr2.0-SUNRISE_genomic_gtf.txt"
outfile="HanXRQr2.0-SUNRISE_genomic_gtf.csv"

# Read in lines
lines = open(infile,"r").read().splitlines()

all_data=list()

'''
seqname - name of the chromosome or scaffold; chromosome names can be given with or without the 'chr' prefix. Important note: the seqname must be one used within Ensembl, i.e. a standard chromosome name or an Ensembl identifier such as a scaffold ID, without any additional content such as species or assembly. See the example GFF output below.
source - name of the program that generated this feature, or the data source (database or project name)
feature - feature type name, e.g. Gene, Variation, Similarity
start - Start position* of the feature, with sequence numbering starting at 1.
end - End position* of the feature, with sequence numbering starting at 1.
score - A floating point value.
strand - defined as + (forward) or - (reverse).
frame - One of '0', '1' or '2'. '0' indicates that the first base of the feature is the first base of a codon, '1' that the second base is the first base of a codon, and so on..
attribute 

'''
gft_headers=["seqname","source","feature","start","end","score","strand","frame"]

for line in lines:
    # print(line)
    outdict = dict()

    gtf_info = line.split("\t")[:-1]
    assert len(gtf_info) == len(gft_headers)

    for i in range(len(gft_headers)):
         outdict[gft_headers[i]] = gtf_info[i]
    

    labels=line.split("\t")[-1].split(";")
    #print(labels)
    for item in labels:
        print(item)
        if len(item) <= 1: continue
        # Save to dictonary. One dict per entry
        category, val = tuple(item.split('"')[:2])

        # clean up:
        category = category.strip()
        val = val.strip()
        if val == "": continue
        outdict[category] = val
    
    all_data.append(outdict)

# Put all dicts for all lines into a df
df = pd.DataFrame(all_data)

# Add an extra column to get the NCBI GeneID alone from the db_xref column
df["NCBI_GeneID"] = df["db_xref"].str.split(":", expand=True)[1]

df.to_csv(outfile)
