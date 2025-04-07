#!/Users/jennastanislaw/miniconda/envs/REE_proj/bin/python
# Change the path above later. Need python env with pandas and requests 

# Jenna Stanislaw
# March 18 2025
# Script to query the KEGG REST API 
# Modified from the original script to search a database and then use ids to search
# for each entry

import pandas as pd
import argparse

import sys
from pathlib import Path
# Should import from the directory above
sys.path.append(str(Path(__file__).resolve().parent.parent)) 
from kegg_helper_fxns import search_KEGG_entry_by_geneID

def main(ids, organism_code, input_type, col_index, outpath):

     # Process id type - accept a list directly, or a column in a file
    if input_type.lower() in ["csv", "tsv"]:
        if input_type == "csv":
            df = pd.read_csv(ids,index_col=0)
        elif input_type == "tsv":
            df = pd.read_csv(ids, sep='\t',index_col=0)
        entry_id_list = [str(i) for i in df.iloc[:, col_index].tolist()]
    elif input_type == "list":
        entry_id_list = [str(i) for i in ids.split()]
    else:
        raise ValueError(f'''Input type {input_type} is not allowed. Input must be formatted as csv, tsv, 
        or space-separated list. The -t option should reflect the input format. 
        See the information about the option for more details''')
    
    # Modify search list to add the organism code if needed 
    if not  entry_id_list[0].startswith(organism_code):
        for j in range(len(entry_id_list)):
            entry_id_list[j] = f"{organism_code}:{entry_id_list[j]}"
    
    df_out = search_KEGG_entry_by_geneID(entry_id_list)

    # Save as tsv
    df_out.to_csv(outpath,sep='\t')

def parseargs():
    parser=argparse.ArgumentParser(
        description="""Uses the KEGG API to perform searches for a list of geneIDs
        from a specific orgnaism. The information about each gene is extracted and
        saved to a tsv."""
    )

    parser.add_argument('--ids','-i', type=str, required=True,
                        help='''Input IDs to serach for in KEGG. Can be file (csv or tsv),
                        or space-separated list of NCBI Gene ID numbers''')
    parser.add_argument('--input_type','-t', type=str,
                        default='list',
                        help='''Input type. Allowed options are 'csv', 'tsv', or 'list', where
                        list is a space-separated string. This is the default.
                        If csv or tsv is provided, the -c option must be used to specify
                        which column contains the gene IDs.''')
    parser.add_argument('--col_index','-c', type=int, default=0,
                        help='Index (starting from 0) of column with sequences')
    parser.add_argument('--organism_code','-g', type=str,
                        default="han", help='Organism code to use in search for the gene')
    parser.add_argument('--outpath','-o',type=str,
                        default='output.tsv', 
                        help='Path to tsv where search output should be saved')
    
    args = parser.parse_args()

    return args


if __name__ == "__main__":
    args=parseargs()

    # Main function
    main(args.ids, args.organism_code, args.input_type, args.col_index, args.outpath)