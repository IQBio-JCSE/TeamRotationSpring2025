* `preprocessing_ref_genome` : - directory for processing info about coding sequences from reference genome
        * NOT IN GITHUB REPO: `GCF_002127325.2_HanXRQr2.0-SUNRISE_cds_from_genomic.fna` - coding sequence fastas from the refernce genome. The individual sequence descriptions contain information about each gene, which will be parsed. Downloaded from online `HanXRQ2.0_SUNRISE` wuth `wget https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/002/127/325/GCF_002127325.2_HanXRQr2.0-SUNRISE/GCF_002127325.2_HanXRQr2.0-SUNRISE_cds_from_genomic.fna.gz`
        * `HanXRQr2.0-SUNRISE_cds_gene_info.txt` - just the descriptor lines from the cds fasta (lines beginning with > )
        * `HanXRQr2.0-SUNRISE_cds_gene_info.csv` - Output from processing of fasta
