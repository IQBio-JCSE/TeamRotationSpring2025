# BLAST genes of interest against reference genome
Goal: Identifty which genes in the reference genome are the genes of interest, based on the sequence of the genes of interest
To connect genes of interest to the reference genome, need to run a BLAST of the gene sequences against the coding sequences of the genmoe. This is because the reference genome is not annotated with the names or other information associated with the KEGG-identified genes. All of the BLAST was done on the ruderalis server and then copied over locally

## Generation of BLAST database
Downloaded the coding seqeuences from the Han XRQ-SUNRISE v2 genome to make the BLAST database. This file is used as the inoput with the following command:

`makeblastdb -in ../../GCF_002127325.2_HanXRQr2.0-SUNRISE_cds_from_genomic.fna -out HanXRQr2.0-SUNRISE_cds -dbtype nucl`

Three files are produced: `HanXRQr2.0-SUNRISE_cds.nhr, HanXRQr2.0-SUNRISE_cds.nin, HanXRQr2.0-SUNRISE_cds.nsq`

## BLAST 
The input from this step is a fasta file containing the sequences of the genes of interest. In the example shown here, the fasta files are generated from the KEGG search for terpenoid synthesis genes in _H. annuus_. There are many different ways to output, so three different file types were generated with three different commands.
* Pairwise aligment, as seen in the NCBI browser: `blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output`
* TSV: `blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output_tabular.tsv -outfmt 6`
* Sequence alignemnt map: `blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output_alignment.sam -outfmt 17`

## Reorgnaization
Generated files were copied to local machine and reorganized. Database files were relocated to `ref_genome_XRQ` and output files from the BLAST search were moved to `output_blastn/terpenoid_synthesis_genes`
