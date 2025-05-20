
parallel -j 6 'wrapper_phage_contigs_sorter_iPlant.pl -f {} --db 1 --wdir virsorter/{/.} --ncpu 4 --data-dir virsorter-data '  ::: contigs/*.fa


parallel -j 10 --link --xapply 'prodigal -a prodigal/{}.pep -d prodigal/{}.cds -f gff -g 11 -o prodigal/{}.gff -p meta -s prodigal/{}.stat -i contigs/{}_final.fa ' :::: list.txt


parallel -j 6 'hmmsearch -o ./hmm_output/{/.}.txt --tblout ./hmm_output/{/.}.tbl -E 1e-5 hmm_db/pVOG_ref.hmm prodigal/{}.pep '  :::: list.txt

parallel -j 6 'blastn -query {}  -db  NCBI_nt/nt -out  ./{/.}_blastn.out -outfmt 6 -num_alignments 1 -evalue 1e-5  '  ::: /*.fasta


#############


parallel -j 10 'bwa aln all_viral_contigs.fa {} -f sai_file/{/.}.1.sai' ::: *.R1.fastq.gz


parallel -j 10 --link --xapply --plus  --regexp  'bwa samse all_viral_contigs.fa sai_file/{}.1.sai /{}.R1_kneaddata_paired_1.fastq > sam_file/{}.se.sam '  :::: list.txt  #2 running 

for file in $(ls ./*.sam); do perl calculate_abundance_from_sam.pl ${file} ${file}.counts.txt; done

perl fastalength.pl all_viral_contigs.fa > all_viral_contigs_length.txt 

less all_viral_contigs_length.txt | grep "k141" | awk -F " " '{print$1" "$2}' > all_viral_contigs_length.clean.txt