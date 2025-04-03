# Genes of interest from KEGG

## Retriving columns of interest from the data
Need to define which columns to keep from the entry page for each gene of interest
To do this, first copied the output of `https://rest.kegg.jp/get/han:110865220+han:110867580` to a file called `sample_raw_out.txt`. Then, used the below command to extract the "column" names.  \
` awk -F'   ' '{print $1}' sample_raw_out.txt| sort |uniq | tr '\n' '_' | sed 's:_:\",\":g'`

Raw output looks like this:
`","AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME","NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION","%`  

Above raw output is cleaned up and copied into a list, stored as a global var in the `run_kegg_api.py` script.

## Using API to extract genes of interest from KEGG
Running the script as is will do a search for Helianthus annuus genes which are associated with the 00900 pathways (terpenoid backbone synthesis) and write the information about these genes to the output file `./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest_full.tsv`. The script has been modified to accept command-line arguments to allow for different searches, but this has not been extensively tested.

To run the script, the user will need to have a python environment with pandas and requests. Then, can run with the following command:
`python run_kegg_api.py`

Note that if the user wants to run without specifiy which python to use, the first line of the file will need to be changed to the correcy python path

## Preparing sequences for BLAST
Following creation of the csv, if further steps need to be taken with the sequences saved in the csv, sequences can be extracted in fasta format using the `make_fasta_from_csv.py` script. The user can specify which column has sequence information, as well as which column should be used to extract identifiers (names) for the sequences. 
For processing of terpenoid synthesis genes from the tsv above:
`make_fasta_from_csv.py  -i ./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest.tsv -c 12 -n 1  -o ./output_kegg_search/terpenoid_synthesis_genes/fasta_terpenoid_synthesis_genes.fa`

This fasta file will become the inpout for the next steps, which are running a BLAST to align the genes of interest to an annotated reference genome.