# Genes of interest from KEGG

## Retriving columns of interest from the data
Need to define which columns to keep from the entry page for each gene of interest
To do this, first copied the output of `https://rest.kegg.jp/get/han:110865220+han:110867580` to a file called `sample_raw_out.txt`. Then, used the below command to extract the "column" names.  \
` awk -F'   ' '{print $1}' sample_raw_out.txt| sort |uniq | tr '\n' '_' | sed 's:_:\",\":g'`

Raw output looks like this:
`","AASEQ","BRITE","DBLINKS","ENTRY","MODULE","MOTIF","NAME","NTSEQ","ORGANISM","ORTHOLOGY","PATHWAY","POSITION","%`  

Above raw output is cleaned up and copied into a list, stored as a global var in the `run_kegg_api.py` script.

## Using API to extract genes of interest from KEGG
Currently, all of the options are hard-coded into the `run_kegg_api.py` script, so running the script as is will do a search for Helianthus annuus genes which are associated with the 00900 pathways (terpenoid backbone synthesis) and write the information about these genes to the output file `terpenoid_synthesis__genes_of_interest_full.tsv`. To expand the list of candidate genes and investigate more pathways, the script should be modified to accept command-line arguments in a modular fashion.

To run the script, the user will need to have a python environment with pandas and requests. Then, can run with the following command:
`python run_kegg_api.py`

Note that if the user wants to run without specifiy which python to use, the first line of the file will need to be changed to the correcy python path

## TODO
* Modify python script to accept command line arguments instead of hard-coded paths and search terms 