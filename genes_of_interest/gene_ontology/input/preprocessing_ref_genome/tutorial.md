# Extract information from the reference genome fasta

First, pulled out all header lines from the fasta. These are all lines that start with ">". These were saved to a temporary text file called ` HanXRQr2.0-SUNRISE_cds_gene_info.txt` (just for later processing).

`grep ">" GCF_002127325.2_HanXRQr2.0-SUNRISE_cds_from_genomic.fna >> HanXRQr2.0-SUNRISE_cds_gene_info.txt`

Then, this text file is used as input into a python script called `split_fasta_header.py`. The script will split info from the header and save all parsed header info to a csv. For example, a line that looks like: 
`>lcl|NC_035433.2_cds_XP_022038937.1_1 [gene=LOC110941587] [db_xref=GeneID:110941587] [protein=WEB family protein At2g38370] [protein_id=XP_022038937.1] [location=join(118257..118409,118852..120219)] [gbkey=CDS]`

Will be parsed into a dictionary that looks like this:
```
{'id': 'NC_035433.2_cds_XP_022038937.1_1', 
 'gene': 'LOC110941587', 
 'db_xref': 'GeneID:110941587', 
 'protein': 'WEB family protein At2g38370', 
 'protein_id': 'XP_022038937.1', 
 'location': 'join(118257..118409,118852..120219)', 
 'gbkey': 'CDS'}
```

The db_ref column is parsed further, adding an additional column called NCBI_GeneID, which contains just the numerical ID. Any missing values are filled with NaN. The parsed headers are saved to `HanXRQr2.0-SUNRISE_cds_gene_info.csv`.
