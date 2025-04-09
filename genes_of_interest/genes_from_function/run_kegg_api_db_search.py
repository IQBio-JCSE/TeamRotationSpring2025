#!/Users/jennastanislaw/miniconda/envs/kegg_search/bin/python
# Change the path above as needed to match the location of your python in your environment

# Jenna Stanislaw
# March 18 2025
# Script to query the KEGG REST API 

import requests
import pandas as pd
import math
import re
import time
import argparse

import sys
from pathlib import Path
# Should import from the directory above
sys.path.append(str(Path(__file__).resolve().parent.parent)) 
from kegg_helper_fxns import search_KEGG_entry_by_geneID

# Remove BRITE?
COLUMNS=["AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME",
         "NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION"]

def main(operation, target_database, search_term, outpath):

    # Load arguments into the string for the url
    url_base = "https://rest.kegg.jp"
    # operation = "link"
    # target_database = "han"
    # search_term="path:han00900" # search term from another database. In this example, terpenoid backbone synthesis pathway in helianthus annuus

    full_url = f"{url_base}/{operation}/{target_database}/{search_term}"
    # full_url_test="https://rest.kegg.jp/link/han/path:han00900"

    # Send the request
    response = requests.get(full_url)

    # Turn response into text and parse the output list (this will be the second/final
    # column from the output)
    response_data = response.text
    entry_id_list = [ row.split('\t')[-1] for row in response_data.split("\n")]
    # print(entry_id_list)

    df = search_KEGG_entry_by_geneID(entry_id_list)
    
    # Save as tsv
    df.to_csv(outpath,sep='\t')

def parseargs():
    parser=argparse.ArgumentParser(
        description="""Uses the KEGG API to perform two types of searches. First,
        a tagret database is searched using a specific term. Then, the output from this
        search is used to pull the genes of interest and save their information to a tsv."""
    )

    # parser.add_argument('--operation','-o', type=str,
    #                     default='link',
    #                     help='Type of API operation to perform. See https://www.kegg.jp/kegg/rest/keggapi.html for allowed options.')
    parser.add_argument('--target_database','-d', type=str,
                        default="han",
                        help='Database to search')
    parser.add_argument('--search_term','-s', type=str,
                        default="path:han00900",
                        help='Term to search for in the target database')
    parser.add_argument('--outpath','-o',type=str,
                        default='./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest.tsv', 
                        help='Path to which the tsv with search output should be saved')
    
    args = parser.parse_args()

    return args


if __name__ == "__main__":
    args=parseargs()
    
    #TODO: modify script to allow for other serach types, then change this 
    # Currently, only allows for "link" operation, which connects two databases
    args.operation = "link"

    # Main function
    main(args.operation, args.target_database, args.search_term, args.outpath)
