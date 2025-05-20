source activate vContact2

vcontact --raw-proteins viral_genomes.faa --rel-mode ‘Diamond’ --proteins-fp viral_genomes_g2g.csv --db 'ProkaryoticViralRefSeq94-Merged' --pcs-mode MCL --vcs-mode ClusterONE --c1-bin /anaconda3/envs/vContact2/bin  --output-dir vConTACT2_Results
