

parallel -j 8    --link   --xapply  --plus   --regexp    'megahit  -1   clean/{}_*_paired_1.fastq  -2    clean/{}_*_paired_2.fastq     -m 0.5 -t 12   -o  megahit_out_batch/{} '   ::::  list.txt  
parallel -j 5 --xapply 'metabat2   -m 1500 -t 16 -i  contigs/{}.R1_final.contigs.fa  -a  depth_files/{}.depth.txt -o {}_bin -v  '   ::::  list.txt

phylophlan -i  bins/   -d database/phylophlan  --diversity  low -t a  -f supermatrix_aa.cfg   --nproc 12  --genome_extension fa

