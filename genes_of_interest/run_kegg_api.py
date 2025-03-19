#!/Users/jennastanislaw/miniconda/envs/REE_proj/bin/python
# Change the path above later 

# Jenna Stanislaw
# March 18 2024
# Script to query the KEGG REST API 

import requests
import pandas as pd
import math
import re
import time

# Remove BRITE?
COLUMNS=["AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME",
         "NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION"]

# Process our arguments 


# Load arguments into the string for the url
url_base = "https://rest.kegg.jp"
operation = "link"
target_database = "han"
search_term="path:han00900" # search term from another database. In this example, terpenoid backbone synthesis pathway in helianthus annuus

full_url = f"{url_base}/{operation}/{target_database}/{search_term}"
# print(full_url)
# full_url_test="https://rest.kegg.jp/link/han/path:han00900"

# Send the request
response = requests.get(full_url)

# Turn response into text and parse the output list (this will be the second/final
# column from the output)
response_data = response.text
entry_id_list = [ row.split('\t')[-1] for row in response_data.split("\n")]
# print(entry_id_list)

data = list()

start = 0
end = 10
# do search for entry (need to convert to only do 10 at a time)
for _ in range(math.ceil(len(entry_id_list)/10)):
    search_l = entry_id_list[start:end]
    entry_search_str = "+".join(search_l)
    get_url = f"https://rest.kegg.jp/get/{entry_search_str}"
    entry_info_all = requests.get(get_url).text

    all_entries=entry_info_all.split("///")[:-1] # last elem is always empty 

    for entry_text in all_entries:
        curr_col = ""
        entry_d = dict()
        for row in entry_text.split("\n"):
            # Split anything with more than one space between it
            # This keeps phrases together but splits everything else
            words = re.split(r"\s{2,}", row) 
            # print(words)
            # print("column", curr_col)
            if words[0] in COLUMNS:
                curr_col = words[0]
                if "SEQ" in curr_col:
                    entry_d[curr_col+"_length"] = words[1]
                    entry_d[curr_col] = ""
                else:
                    entry_d[curr_col] = ";".join(words[1:])
            elif curr_col == "":
                continue
            elif "SEQ" in curr_col:
                entry_d[curr_col] = entry_d[curr_col]+"".join(words)
            else:
                entry_d[curr_col] = entry_d[curr_col] +";".join(words)
        if len(entry_d) > 0:
            data.append(entry_d)

    start += 10
    end += 10
    time.sleep(0.35) #Can't do more that 3 requests per second 

df = pd.DataFrame(data)
# print(df)

name="terpenoid_sythesis"
outpath=f"./{name}__genes_of_interest.tsv"

# Save as tsv
df.to_csv(outpath,sep='\t')
