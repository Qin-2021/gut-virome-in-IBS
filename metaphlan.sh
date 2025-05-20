
parallel -j 8 --link --xapply --plus  --regexp   ' metaphlan  --input_type fastq  --bowtie2db   /database/metaphlan3    clean_data/{}*_paired_1.fastq  -o  output/{}.metaphlan.txt'   :::: list.txt 
 
