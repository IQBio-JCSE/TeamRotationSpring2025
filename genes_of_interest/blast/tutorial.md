makeblastdb -in ../../GCF_002127325.2_HanXRQr2.0-SUNRISE_cds_from_genomic.fna -out HanXRQr2.0-SUNRISE_cds -dbtype nucl

blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output
blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output_tabular.tsv -outfmt 6
blastn -query fasta_terpenoid_synthesis_genes.fa -db HanXRQr2.0-SUNRISE_cds -out blast_terpenoid_output_alignment.sam -outfmt 17
