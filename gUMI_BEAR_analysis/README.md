# gumi_bear_code
This repo contatins all scripts and files required to perform analysis for the gUMI-BEAR method as described in the paper:
"gUMI-BEAR, a modular, unsupervised population barcoding method to track variants and evolution at high resolution"

## First script to run 
Following demultiplexing, for each folder contatining read1 and read2 files use the following script:
```
./01_Prepare_samples_from_fastq.sh "path_to_folder_where_R1_and_R2" "path_to_internal_index_file"
```
internal_index_file - should contain internal index sequences inserted in the sequcning fragement - look at the file "aux_internal_indexes.csv" for example

This will result in two files for each sample:
1) a file containing all releveant barcodes sequences
2) a file containing all relevant umis sequences

## Second script to run 
First of all trasnfer all relevant files and their respective umi files to a folder
Fill out the "aux_01_config_file" - for examples on how each file should be formatted see example files
Run the following script:
```
./02_Cluster_barcode_seeds_and_count.sh "path_to_dir_where_all_files_are" "path_to_config"
```

The second script should result in a final data frame containing counts and frequencies for each sample
