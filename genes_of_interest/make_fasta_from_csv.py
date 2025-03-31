#!/Users/jennastanislaw/miniconda/envs/REE_proj/bin/python
# Change the path above later. Need python env with pandas

# Jenna Stanislaw
# March 31, 2025
# Script to extract gene sequences from csv to one fasta file

import pandas as pd
import argparse

def main(infile, name_i, col_i, outfile):
    df = pd.read_csv(infile, sep='\t',index_col=0)
    # print(df)
    seqs = df.iloc[:, col_i].str.upper()
    names = df.iloc[:, name_i]
    assert len(seqs) == len(names)
    f = open(outfile, "w")
    for i in range(len(names)):
        f.write(f">{names[i]}\n")
        f.write(seqs[i]+"\n")
    f.close()
    print(f"Fasta written to {outfile}")


def parseargs():
    parser=argparse.ArgumentParser()

    parser.add_argument('--infile','-i', type=str,
                        required=True,
                        help='Path to input file. Should be a tsv')
    parser.add_argument('--name_index','-n', type=int,default=0,
                        help='Index (starting from 0) of column with names to be used in fasta file')
    parser.add_argument('--col_index','-c', type=int, default=1,
                        help='Index (starting from 0) of column with sequences')
    parser.add_argument('--outfile','-o', type=str, default="output.fa",
                        help='Path to output file. Should be a fasta (.fa)')
    
    args = parser.parse_args()

    return args

if __name__ == "__main__":
    args=parseargs()

    # Main function
    main(args.infile, args.name_index, args.col_index, args.outfile)
