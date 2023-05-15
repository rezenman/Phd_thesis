#!/bin/bash

##create an environment to contain all relevant packages (multiqc, fastp, cutadapt)
#load packages - specific for working with the Weizmann institute hpc cluster
module load bcl2fastq/2.20.0.422
module load fastqc/0.11.9
module load BBMap/38.45
module load R/3.6.0  

##load miniconda first, change 'analysis' to whatever your conda environment name is
## packages to install on the conda environment - multiqc, fastp, cutadapt, if not working on weizmann hpc also: bcl2fastq, fastqc, BBmap, R 
source activate analysis 

#to run use: ./Prepare_samples_from_fastq.sh "path_to_folder_where_R1_and_R2" "path_to_internal_index_file"

folder=$1
path_to_int_index=$2
cd $1
echo -e "\n-----FASTQC-----\n" >> log.txt

##1 - fastqc
read1=$(ls|grep R1)
read2=$(ls|grep R2)
fastqc $read1 $read2 &>>log.txt

#check that for each sample two fastqc files were created 
comp1=$(cat log.txt | grep -c -E "Analysis complete") 
if (( $comp1 == 2 ))
then 
  echo "Successfully finished creating quality reports" >> log.txt
else 
  echo "ERROR - Number of quality reports does not match number of samples"  >> log.txt && exit 
fi

##2 - trimming uneccessary basepairs  
echo -e "\n-----TRIMMING_BASEPAIRS_USING_FASTP-----\n" >> log.txt  

R1=$(ls *R1_001.fastq.gz)
R2=$(ls *R2_001.fastq.gz)

fastp -i $R1 -I $R2 -o $folder"_only_barcode.fastq.gz" -O $folder"_index_umi.fastq.gz" -f 5 -F 5 -T 3 -w 10 -h $folder"_fastp_trim_report.html" &>>log.txt

comp=$(find . -name "*_fastp_trim_report.html" | wc -l)
if (( $comp == 1))
then 
  echo "Successfully finished trimming uneccessary base-pairs" >> log.txt
else 
  echo "ERROR - Something went wrong with the fastp trim script, please check log files" >> log.txt && exit 
fi

##3 - Quality filtering for R1 and R2
echo -e "\n-----QUALITY_FILTERING-----\n" >> log.txt

R1=$(ls *barcode.fastq.gz)
R2=$(ls *index_umi.fastq.gz)

fastp -i $R1 -o $folder"_R1_filtered.fastq.gz" -q 25 -u 9 -n 0 -l 24 -h $folder"_fastp_R1_quality_report.html" &>>log.txt &
fastp -i $R2 -o $folder"_R2_filtered.fastq.gz" -n 2 -l 18 -h $folder"_fastp_R2_quality_report.html" &>>log.txt &
wait

comp_1=$(find . -name "*fastp_R1_quality_report.html" | wc -l)
comp_2=$(find . -name "*fastp_R2_quality_report.html" | wc -l)

if (( $comp_1 == 1 && $comp_2 == 1))
then 
  echo "Successfully filtered R1 and R2 reads" >> log.txt
else 
  echo "ERROR - Something went wrong with reads quality filtering" >> log.txt && exit
fi

##4 - Repairing reads
echo -e "\n-----REPAIRING_READS-----\n" >> log.txt

R1=$(ls *R1_filtered.fastq.gz)
R2=$(ls *R2_filtered.fastq.gz)

repair.sh in1=$R1 in2=$R2 out1=$folder"_R1_repaired.fastq.gz" out2=$folder"_R2_repaired.fastq.gz" outs=$folder"_singles.fastq.gz" repair &>>log.txt


comp=$(grep -c -E "^Time:.*seconds\.$" log.txt)
if (( $comp == 1))
then 
  echo "Successfully finished repairing reads" >> log.txt
else 
  echo "ERROR - Something went wrong with repairing the reads, please check log files" >> log.txt && exit
fi

##5 - seperating R2 reads to umi reads and internal indices reads
echo -e "\n-----SEPERATING_R2_TO_UMI_AND_INDEX-----\n" >> log.txt

R2=$(ls *R2_repaired.fastq.gz)
cutadapt -u 8 -o $folder"_only_umi.fastq.gz" $R2 -j 0  &>>log.txt 
cutadapt -u -10 -o $folder"_only_index.fastq.gz" $R2 -j 0 &>>log.txt 


comp=$(grep -c -E "=== Summary ===" log.txt)
if (( $comp == 2))
then 
  echo "Successfully seperated umi and index reads" >> log.txt
else 
  echo "ERROR - Something went wrong with the umi/index seperation, please check log files" >> log.txt && exit
fi

##6 - creating report
echo -e "\n-----REPORT-----\n" >> log.txt

echo "sample_name" "barcode" "index" "umi" >> log.txt
index=$(zcat *only_index.fastq.gz | grep -E -c "^@") &
read1=$(zcat *R1_repaired.fastq.gz |grep -E -c "^@") &
umi=$(zcat *only_umi.fastq.gz |grep -E -c "^@") &
wait
echo $folder $read1 $index $umi >> log.txt
echo -e "\nfinished creating report" >> log.txt


##7 - convert csv standard sample sheet to cutadapt ready sample sheet
echo -e "\n-----CONVERTING_INTERNAL_INDICES-----\n" >> log.txt

sed 's/ /_/g' $path_to_int_index | sed 's/,/_/' | sed 's/,/\n^/' | sed 's/Gr/\>Gr/' | grep -A 1 $folder"_" > "int_index_"$folder 
cat "int_index_"$folder >>log.txt

echo -e "\n-----FINISHED_CONVERTING_INTERNAL_INDICES-----\n" >> log.txt

##8 - demultiplexing according to internal primers
echo -e "\n-----DEMULTIPLEXING_INTERNAL_INDICES-----\n" >> log.txt

barcode=$(ls int_index*)
index=$(ls *only_index.fastq.gz)
read1=$(ls *R1_repaired.fastq.gz)

cutadapt -e 0.15 --no-indels -g file:$barcode -o int-{name}.2.fastq -p int-{name}.1.fastq $index $read1 &>>log.txt

comp=$(grep -E -c "=== Summary ===" log.txt)

if (( $comp == 3))
then 
  echo "Successfully demultiplexed internally" >> log.txt
else 
  echo "ERROR - Something went wrong with the intenral de-multiplexing, please check log files" >> log.txt && exit
fi

##9 - finding matching umis for each sample
echo -e "\n-----CREATING_SEPERATE_UMI_FILE_FOR_EACH_SAMPLE-----\n" >> log.txt

group=$(ls Gr*only_umi.fastq.gz)

for file in $(ls int-*.1.fastq | awk '{print $1}')
  do  
    echo -e $file"::\n" >> log.txt
    repair.sh in1=$group in2=$file out1=ni.fastq.gz out2=$file"_umis.fastq.gz" repair overwrite=true &>> log.txt
    if [ $? -eq 0 ] ; then echo "finished finding umis for "$file >> log.txt && continue ; else echo "bad job" >>log.txt && exit  ; fi
  done
  rm ni.fastq.gz