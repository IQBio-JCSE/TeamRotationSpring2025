# Genes of interest from KEGG
Goal: extract information about genes of interest from KEGG
In the specific test case that is detailed in this example, we are looking for genes from _Helianthus annuus_ that are associates with terpenoid backbone synthesis. However, the scripts can be modified to search for genes associated with other pathways of interest (and likely for other organisms, although this has not been tested.)

## Retriving columns of interest from the data
Before writing the search script, had to define which columns to keep from the entry page for each gene of interest. In the browser, each entry page for a gene has a table with information, which is converted into a full text format when a search for that gene (using its KEGG ID) is performed with the KEGG API. The rows in the table are the same for each gene, so we can look at the output from a sample search to determine which rows of the table are important to keep.
To do this, first copied the output of `https://rest.kegg.jp/get/han:110865220+han:110867580` to a file called `sample_raw_out.txt`. Then, used the below command to extract the "column" names.  \
` awk -F'   ' '{print $1}' sample_raw_out.txt| sort |uniq | tr '\n' '_' | sed 's:_:\",\":g'`

Raw output looks like this:
`","AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME","NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION","%`  

Above raw output is cleaned up and copied into a list, stored as a global var in the `run_kegg_api.py` script.

## Using API to extract genes of interest from KEGG
Running the script as is will do a search for Helianthus annuus genes which are associated with the 00900 pathways (terpenoid backbone synthesis) and write the information about these genes to the output file `./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest_full.tsv`. The script has been modified to accept command-line arguments to allow for different searches, but this has not been extensively tested.

To run the script, the user will need to have a python environment with pandas and requests. Then, can run with the following command:
`python run_kegg_api.py`

Note that if the user wants to run without specifiy which python to use, the first line of the file will need to be changed to the correcy python path.

### Update: April 4, 2025
Script was modified to add an additional column to the tsv, titled "ID". This column contains the first value in the "ENTRY" column, and should correspond to the KEGG entry number. This number also appears to be the same as the NCBI-GeneID.  

## Preparing sequences for BLAST
Following creation of the csv, if further steps need to be taken with the sequences saved in the csv, sequences can be extracted in fasta format using the `make_fasta_from_csv.py` script. The user can specify which column has sequence information, as well as which column should be used to extract identifiers (names) for the sequences. 
For processing of terpenoid synthesis genes from the tsv above:
`make_fasta_from_csv.py  -i ./output_kegg_search/terpenoid_synthesis_genes/terpenoid_synthesis__genes_of_interest.tsv -c 13 -n 2  -o ./output_kegg_search/terpenoid_synthesis_genes/fasta_terpenoid_synthesis_genes.fa`

This fasta file will become the inpout for the next steps, which are running a BLAST to align the genes of interest to an annotated reference genome.