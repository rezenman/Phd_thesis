print("Reading all r workspaces and merging all tables to a final count and frequency table")
count_to_freq = function(data){
  freqs_data = as.data.frame(data[,1])
  data_sub = data[,-1]
  for (col in 1:ncol(data_sub)){
    freqs = data_sub[,col] / sum(data_sub[,col])
    freqs_data = cbind(freqs_data, freqs)
    colnames(freqs_data)[1 + col] = colnames(data_sub)[col] 
  }
  return(freqs_data)
}  


all_data_frames = list.files(pattern = "\\.Rdata$")
# reading data frames and arranging them ----------------------------------

###read all Rdata files and load final df for each sample to a list
dfs_list = NULL
for(j in all_data_frames){
    current_df = load(j)
    dfs_list[[j]] = final_df
    print(paste0(j, " added to data frames list"))
}
names(dfs_list)
lapply(dfs_list, head)
  
  
##adding to each data frame in the list acoulumn that contatins  sample number 
for (i in 1:length(dfs_list)){
  dfs_list[[i]]$sample = names(dfs_list)[i]
}
  
##subsetting all data frame to contain only read sequence and abundance and saving in a list by sample names
sub_dfs_list = NULL
for(k in dfs_list){
  sub_df = k[,c(10,9)]
  sample_name = k$sample[1]
  colnames(sub_df)[2] = sample_name
  sub_dfs_list[[sample_name]] = sub_df
}
  
##merging all data frame by lineage while keeping all NA's(lineages that didn't appear in all sample)
all_dfs = Reduce(function(x,y) merge(x = x, y = y, by = "read_1", all = T), sub_dfs_list)
head(all_dfs)
  
##changing all Na's to zeros and saving file as csv
all_dfs[is.na(all_dfs)] = 0
write.csv(all_dfs, file = "final_df.csv")
  
##creating a frequency table and saving it
all_dfs_freq = count_to_freq(all_dfs)
write.csv(all_dfs_freq, file = "final_df_freq.csv")
            
print("@@@@@@@@@@@@@@ finished analyzing @@@@@@@@@@@@@@")









