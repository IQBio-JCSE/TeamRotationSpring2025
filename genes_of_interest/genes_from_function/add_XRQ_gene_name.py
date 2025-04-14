#!/Users/jennastanislaw/miniconda/envs/jcse/bin/python

import pandas as pd
import argparse

def main(infile, ref_infile, outfile="", overwrite=False):
    # Add col to reference name 
    df = pd.read_csv(infile, sep='\t',index_col=0)

    ref_df = pd.read_csv(ref_infile, dtype=str, sep=",", index_col=0)

    df["NCBI_GeneID"] = df["ID"].str.replace("LOC","")
    GOI_kegg_id_list = df["NCBI_GeneID"]

    GOIs_ref = ref_df[ref_df["NCBI_GeneID"].isin(GOI_kegg_id_list)].copy()
    # print(GOIs_ref["gene"].tolist())

    df = df.merge(
        GOIs_ref[['NCBI_GeneID', 'gene']],  # just take needed columns
        on='NCBI_GeneID',
        how='left'  
    )

    if overwrite:
        df.to_csv(infile,sep='\t')
    else:
        df.to_csv(outfile,sep='\t')
    

def parseargs():
    parser=argparse.ArgumentParser()

    parser.add_argument('--infile','-i', type=str,
                        default="./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest.tsv",
                        help='Path to input file. Should be a tsv')
    parser.add_argument('--ref_infile','-r', type=str,
                        default="../preprocessing_ref_genome/HanXRQr2.0-SUNRISE_cds_gene_info.csv",
                        help='Path to reference input file. Should be a csv and contain all genes in the input file')
    parser.add_argument('--outfile','-o', type=str, default="output.tsv",
                        help='Path to output file. Should be a tsv')
    parser.add_argument('--overwrite','-w', type=bool, default=False,
                        help='Overwrite input tsv? This should just append a column to the original file. Outfile option will be ignored')
    
    args = parser.parse_args()

    return args

if __name__ == "__main__":
    args=parseargs()

    # Main function
    main(args.infile, args.ref_infile, args.outfile, args.overwrite)