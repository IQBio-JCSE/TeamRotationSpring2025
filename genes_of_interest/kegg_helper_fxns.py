import requests
import pandas as pd
import math
import re
import time

COLUMNS=["AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME",
         "NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION"]

def search_KEGG_entry_by_geneID(entry_id_list):
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
            entry_d = process_entry_for_csv(entry_text)
           
            if len(entry_d) > 0:
                data.append(entry_d)

        start += 10
        end += 10
        time.sleep(0.35) #Can't do more that 3 requests per second 

    df = pd.DataFrame(data)

    return df
    # df.to_csv(outpath,sep='\t') # Save as tsv

# Loop over all od the text data for an entry page and split it into into a dict
# with keys matching the defined columns
def process_entry_for_csv(entry_text):
    # Initialize some variables
    curr_col = ""
    entry_d = dict()

    for row in entry_text.split("\n"):
        # Split anything with more than one space between it
        # This keeps phrases together but splits everything else
        words = re.split(r"\s{2,}", row) 
        if words[0] in COLUMNS:
            curr_col = words[0]
            if "SEQ" in curr_col:
                entry_d[curr_col+"_length"] = words[1]
                entry_d[curr_col] = ""
            else:
                if "ENTRY" in curr_col:
                    entry_d["ID"] = words[1]
                entry_d[curr_col] = ";".join(words[1:])
        elif curr_col == "":
            continue
        elif "SEQ" in curr_col:
            entry_d[curr_col] = entry_d[curr_col]+"".join(words)
        else:
            entry_d[curr_col] = entry_d[curr_col] +";".join(words)
    return entry_d