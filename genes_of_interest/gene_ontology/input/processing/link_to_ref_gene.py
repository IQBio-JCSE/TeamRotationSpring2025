#!/Users/jennastanislaw/miniconda/envs/jcse/bin/python

import pandas as pd
import argparse

def main(infile, ref_infile, outfile, shared_ref_col, output_ref_col):
    # Add col to reference name. Assumes input is a file with one column
    gene_list = [line.strip() for line in open(infile).readlines()]

    ref_df = pd.read_csv(ref_infile, dtype=str, sep=",", index_col=0)
    # print(shared_ref_col)

    GOIs_ref = ref_df.loc[ref_df[shared_ref_col].isin(gene_list)]
    output = GOIs_ref[output_ref_col].tolist()

    with open(outfile, "w") as file:
        for item in output:
            file.write(item + "\n")
    file.close()
    

def parseargs():
    parser=argparse.ArgumentParser()

    parser.add_argument('--infile','-i', type=str,
                        default="",
                        help='Path to input file. Should contain only one column with no header, and one entry per column')
    parser.add_argument('--ref_infile','-r', type=str,
                        default="../../../preprocessing_ref_genome/HanXRQr2.0-SUNRISE_cds_gene_info.csv",
                        help='Path to reference input file. Should be a csv and contain all genes in the input file')
    parser.add_argument('--outfile','-o', type=str, default="output.list",
                        help='Path to output file. Will contain one item per line')
    parser.add_argument('--shared_ref_col','-s', type=str, default="gene",
                        help='The column name in the ref_infile which matches the items in the infile')
    parser.add_argument('--output_ref_col','-c', type=str, default="locus_tag",
                        help='The column name in the ref_infile from which info should be drawn')
    
    args = parser.parse_args()

    return args

if __name__ == "__main__":
    args=parseargs()

    # Main function
    main(args.infile, args.ref_infile, args.outfile, args.shared_ref_col, args.output_ref_col)