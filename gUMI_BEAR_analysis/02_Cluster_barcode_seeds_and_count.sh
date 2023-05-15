#!/bin/bash

#READ FIRST
#move all relevant files to a directory including barcodes fastqs and umi fastqs
# to run use: cluster_and_count.sh "path_to_dir_where_all_files_are" "path_to_config"

##create an environment to contain all relevant packages (multiqc, fastp, cutadapt)
#load packages - specific for working with the Weizmann institute hpc cluster
module load bcl2fastq/2.20.0.422
module load fastqc/0.11.9
module load BBMap/38.45
module load R/4.0.0

##load miniconda first, change 'analysis' to whatever your conda environment name is
## packages to install on the conda environment - multiqc, fastp, cutadapt, if not working on weizmann hpc also: bcl2fastq, fastqc, BBmap, R 
source activate analysis 

#sourcing the config file to get all variables from it
source $2
cd $1

##01 - creating files for seed clustering
echo -e "\n-----CREATING_FILES_FOR_SEEDS_CLUSTERING-----\n" >> cluster_and_count_log.txt  

#creating a fastq file to contatin all relevant files
lines=$(cat $initial_Seeds)
echo -n cat > conc_initial_Seeds
echo -n cat > conc_umis_Seeds

for i in $lines
do
  file=$(ls | grep "$i.*fastq$")
  echo -n " "$file >> conc_initial_Seeds 
done
echo -n " > "$exp_name"_initial_Seeds.fastq" >> conc_initial_Seeds 
source conc_initial_Seeds

#creating a fastq.gz file to contatin all relevant umi files

for i in $lines
do
  file=$(ls | grep "$i.*_umis.fastq.gz$")
  echo -n " "$file >> conc_umis_Seeds 
done
echo -n " > "$exp_name"_umis_Seeds.fastq.gz" >> conc_umis_Seeds 
source conc_umis_Seeds

comp1=$(ls | grep -E -c $exp_name"_initial_Seeds.fastq") ; comp2=$(ls | grep -E -c $exp_name"_umis_Seeds.fastq.gz")
if (( $comp1 == 1 && $comp2 == 1))
then 
  echo "Successfully Created fastqs for seed clustering" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with preperation of fastqs for seeds clustering" >> cluster_and_count_log.txt && exit
fi 

##02 - seed clustering
echo -e "\n-----SEEDS_CLUSTERING-----\n" >> cluster_and_count_log.txt  

cd-hit-est -i $exp_name"_initial_Seeds.fastq" -o $exp_name"_initial_seeds.clst" -c 0.95 -n 5 -d 0 -M 0 -r 0 &>> cluster_and_count_log.txt

comp=$(grep -E -c "program completed" cluster_and_count_log.txt)
if (( $comp == 1))
then 
  echo "Successfully clustered seeds" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with seeds clustering" >> cluster_and_count_log.txt && exit
fi 



##03 - cluster_2d for each sample
echo -e "\n-----CLUSTERING_EACH_SAMPLE_AGAINST_SEEDS-----\n" >> cluster_and_count_log.txt 

samples_to_cluster=$(ls *fastq | grep -v Seeds)
for i in $samples_to_cluster
  do
    echo -e "\n_____"$i"_____\n" > $i"_cluster_2d_log.txt"
    cd-hit-est-2d -i $exp_name"_initial_seeds.clst" -i2 $i -o $i"_VSall.clst" -c 0.95 -n 5 -d 0 -M 0 -r 0 -T 0 &>> $i"_cluster_2d_log.txt" &
done
wait

cat *2d_log.txt > all_cluster2d_log.txt
rm *_2d_log.txt

comp=$(grep -E -c "program completed" all_cluster2d_log.txt)

if (( $comp == $samples ))
then 
  echo "Successfully clustered all samples against seeds" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with samples clustering against seeds" >> cluster_and_count_log.txt && exit
fi 

##04 - reformatting the cluster2d tables to be easily redable as a text file for further analysis
echo -e "\n-----REFORAMTTING_CLUSTER2D_TABLES-----\n" >> cluster_and_count_log.txt 

for file in $(ls | grep VSall.*clstr$)
do
  clstr2txt.pl $file > $file"_df.txt" &
done
wait

comp=$(ls | grep -E -c  "*df.txt")
if (( $comp == $samples ))
then 
  echo "Successfully reformatted clustering tables" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with cluster tables reformatting" >> cluster_and_count_log.txt && exit
fi 

##05 - umis clustering using a custom r code
echo -e "\n-----CLUSTERING_UMIS-----\n" >> cluster_and_count_log.txt 

for file in $(ls *clstr_df.txt | awk '{print $1}')
  do 
    echo -e "\n_____"$file"_____\n" > $file"_r_code_log.txt"
    Rscript $r_code $file $exp_name &>> $file"_r_code_log.txt" &
  done
wait

cat *_r_code_log.txt > r_code_all_log.txt
rm *_r_code_log.txt

comp=$(ls | grep -E -c  "*.Rdata$")
if (( $comp == $samples ))
then 
  echo "Successfully clustered umis and created r workspace for each sample" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with umis clustering" >> cluster_and_count_log.txt && exit
fi 

##06 - merge all tables to output one csv file
echo -e "\n-----MERGING_TABLES_AND_SAVING_AS_CSV-----\n" >> cluster_and_count_log.txt 
Rscript $post_code &>> cluster_and_count_log.txt 

comp=$(cat cluster_and_count_log.txt | grep -E -c "@@@@@@@@@@@@@@ finished analyzing @@@@@@@@@@@@@@")
if (( $comp == 1 ))
then 
  echo "Successfully merged table, check final_df.csv and final_df_freq.csv" >> cluster_and_count_log.txt 
else 
  echo "ERROR - Something went wrong with tables merge" >> cluster_and_count_log.txt && exit
fi




