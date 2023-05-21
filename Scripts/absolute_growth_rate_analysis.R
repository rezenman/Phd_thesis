setwd("~/all_experiments/Absolute_doubling_times/Kfir_Wencheng_Real_data_Calculations/with_missing_lineages/non_smooth_data/")
rm(list = ls())
theme_set(theme_bw())

# loading libraries -------------------------------------------------------
getwd()
library(scales)
library(tidyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(dplyr)

# reading all growth rates data frame and adding rank to the growht rate-------------------------------------
ls = list.files(pattern = "replica.*csv$", full.names = T)
all = NULL
for(i in 1:length(ls)){
  rep = strsplit(ls[i], "replica_|\\.csv")[[1]][2]
  
  df = read.csv(ls[i], check.names = F)
  df = replace(df, df == "-Inf" | df == "Inf" | is.na(df) , 0)
  
  start = colnames(df)[2]
  end = colnames(df)[ncol(df)]
  
  df_long = gather(df, key = "time_hr", value = "growth_rate", start:end)
  df_long$replicate = rep
  df_long$time_hr = round(as.numeric(df_long$time_hr), 1)
  all[[i]] = df_long
}
lapply(all, head)
all = do.call("rbind", all) #binding all replicas top one data frame
head(all)

##removing all lineages who reach zero and calculating ranks based on growth rate
zeros_gone = NULL
for(j in unique(all$replicate)){
  
  df = all %>% filter(replicate == j)
  df_wide = spread(df, key = "time_hr", value = "growth_rate")
  
  for(i in 3:ncol(df_wide)){
    
    index_zero = which(df_wide[,i] == 0)
    df_wide[index_zero,i:ncol(df_wide)] = 0
    
  }
  start = colnames(df_wide)[3]
  end = colnames(df_wide)[ncol(df_wide)]
  
  new_df = gather(df_wide, key = "time_hr", value = 'growth_rate', start:end)
  zeros_gone[[j]] = new_df
}
lapply(zeros_gone, head)
all_no_zeros = do.call("rbind", zeros_gone)
head(all_no_zeros)

all_ranked = all_no_zeros %>% group_by(replicate, time_hr) %>% dplyr::mutate(rank_growth_rate = rank(-growth_rate, ties.method = "first")) %>% 
  ungroup() %>% group_by(replicate, lineage) %>% dplyr::mutate(sd_ranks_growth_rate = sd(rank_growth_rate)) %>% ungroup()
head(all_ranked)

# reading frequency tables and ading ranks to each lineage -------------------
freqs = read.csv("~/all_experiments/sample_to_time/all_replicates_trajectories.csv") %>% select(-X)
freqs$time_hr = round(freqs$Duration, 1)
head(freqs)
freqs_all_ranked = freqs %>% group_by(replicate, time_hr) %>% dplyr::mutate(rank_freq = rank(-frequency, ties.method = "first")) %>% 
  ungroup() %>% group_by(replicate, lineage) %>% dplyr::mutate(sd_ranks_freq = sd(rank_freq)) %>% ungroup()
head(freqs_all_ranked)

# Merging growth rates and frequency  -------------------------------------
head(all_ranked)
head(freqs_all_ranked)

merged = merge(all_ranked, freqs_all_ranked %>% select(-Duration), by = c("replicate", "time_hr", "lineage"))
merged$time_hr = as.numeric(merged$time_hr)
head(merged)

##removing data points where turbidosttats measures are not accurate anymore, towards the end of the experiments
merged_f = merged %>% filter(replicate == "A" & time_hr < 400 |
                               replicate == "B" & time_hr < 350 |
                               replicate == "C" & time_hr < 400 |
                               replicate == "D" & time_hr < 400 |
                               replicate %in% c("E", "F", "G") & time_hr < 850 |
                               replicate == "H" & time_hr < 750 |
                               replicate %in% c("I", "J", "K") & time_hr < 850 |
                               replicate  == "L" & time_hr < 1100 |
                               replicate == "M" & time_hr < 1000)

merged_f %>% distinct(replicate, time_hr) %>% arrange(replicate, as.numeric(time_hr))

# reading tempratures in each time for each replciate ---------------------
sample_to_time_dict = read.csv("~/all_experiments/sample_to_time/Sample_to_time_dictionary.csv") %>% select(-c(X, Timestamp))
sample_to_time_dict$Duration = round(sample_to_time_dict$Duration, 1) 
sample_to_time_dict$Temp = round(sample_to_time_dict$Temp, 1) 
colnames(sample_to_time_dict)[2] = "time_hr"

merged_f = merge(merged_f, sample_to_time_dict, by = c("replicate", "time_hr"))
head(merged_f)
distinct_temps = merged_f %>% distinct(replicate, time_hr, Temp)

# plotting ridgeplots -----------------------------------------------------

for(j in unique(merged_f$replicate)){
  ##looking for the winning lineage by frequency and creating a data frame for it
  winning_lineage = merged_f %>% filter(replicate  == j) %>% filter(time_hr == max(time_hr) & rank_freq == 1) %>% 
    arrange(time_hr) %>% .$lineage
  df_lineage = merged_f %>% filter(replicate == j & lineage == winning_lineage) %>% 
    arrange(time_hr) 
  
  ridge_weighted = ggplot(merged_f %>% filter(replicate == j & growth_rate != 0), 
                          aes(x = growth_rate, y = as.factor(time_hr), fill = time_hr)) +
    geom_density_ridges(aes(height=..density.., weight=frequency, scale= 1.5, stat="density", color = "black", alpha = .5, fill = "#ff2f0a") + 
    geom_density_ridges(aes(x = growth_rate, weight=NULL), scale= 1.5, color = "black", alpha = .6, fill = NA) +
    theme(legend.position="none", panel.spacing = unit(1, "in"), strip.text.x = element_text(size = 8)) +
    xlab("Growth rates") + 
    ylab("Time (hr)") + 
    theme(legend.position = "none") +
    scale_fill_gsea() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_cartesian(clip = "off") + 
    ggtitle(paste0("Replicate: ", j, ", growth rates density by time, adjusted by frequency")) + 
    geom_point(data = df_lineage, aes(x = growth_rate, y = as.factor(time_hr)), color = "black",  size = 3) + 
    geom_label(data = distinct_temps %>% filter(replicate == j), aes(x = .25, y = as.factor(time_hr), label = Temp),
               nudge_y= .3, alpha = .5, size = 5, color = "black", fill = "#ff2f0a", alpha = .5) + 
    geom_label(data = df_lineage, aes(x = 0.6, y = as.factor(time_hr), label = rank_freq),
               nudge_y= .45, nudge_x = -0.01, size = 5, color = "black", fill = NA) + 
    theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
          axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18))
  
  ggsave(paste0("ridgeplot_non_smooth_lineage", j, ".pdf"), ridge_weighted, units = "in", width = 6, height = 8)
}

## looking at the change in growth rates of top lineages with scaled data --------------------------

#keeping only relevant coulumns from the big data frame
merged_sub = merged_f %>% select(replicate, lineage, time_hr, growth_rate, frequency, Temp, rank_freq_end)

#calculating ranks accordin to frequency and growth rates for each lineage in each replciate for each time-point
new_merged = merged_sub %>% filter(growth_rate != 0 & replicate != "M") %>%  group_by(replicate, time_hr) %>% dplyr::mutate(ranks_frequency = rank(-frequency, ties.method = "first"),
                                                                                                                            ranks_growth_rate = rank(-growth_rate, ties.method = "first"))
#scaling the growth rate data to compare the changes in growth rates between replicates and experiments
new_merged = new_merged %>% group_by(replicate, time_hr) %>% dplyr::mutate(scaled_gr = scale(growth_rate))

#ploting histograms of scaled data to make sure the scaling process worked as intended
ggplot(new_merged, aes(x = scaled_gr, fill = replicate)) + geom_histogram() + facet_wrap(~replicate)


#Keeping only the best 10 lineages by the end of the experiment from each replicate to compare their hage in growth rates throughout the experiment
winners = new_merged %>% filter(rank_freq_end < 11) %>% 
  dplyr::mutate(exp = ifelse(replicate %in% c("A", "B", "C", "D"), "exp30", 
                             ifelse(replicate %in% c("E", "F", "G", "H"), "exp39", "gradual"))) %>% #this line adds the experiment type according to the repicate name
  arrange(replicate, time_hr) 

#subseting the sample to time data to contain only relevanbt coulumns
time_dict = sample_to_time_dict %>% select(time_hr, Generations, replicate, Sample)

head(time_dict)
head(winners)

#merging sample to time witht the data frame containing the scaled growtrh rate data
win_with_sample_to_time = merge(winners, time_dict, by = c("replicate", "time_hr"))

#calculating mean time per sample
win_with_mean_time_per_sample= win_with_sample_to_time %>% group_by(replicate, Sample) %>% dplyr::mutate(mean_time_per_sample = mean(time_hr)) %>% 
  ungroup()

#arranging the factor to fit the order of experiments
win_with_mean_time_per_sample$exp = factor(win_with_mean_time_per_sample$exp, levels = c("exp30", "gradual", "exp39"))

#adding a marking to the winning lineages presented in the ridgeplot by black
win_with_mean_time_per_sample= win_with_mean_time_per_sample %>% dplyr::mutate(mark = ifelse(lineage == "ACTACTTTACTATTTGTTTGTATG" & replicate == "K" | 
                                                lineage == "GGCTGGCACACGCGTAAACGGGGA" & replicate == "B" |
                                                lineage == "GCTCGAGGCACAGGTTTCGAGTGC" & replicate == "E", "black","white"))

win_with_mean_time_per_sample = win_with_mean_time_per_sample %>% dplyr::mutate(un_lin = paste0(replicate, "_", lineage))


my_pal = c("#72c9dd", "#33b39f",  "#eb705e")
sacled_traj = ggplot(win_with_mean_time_per_sample%>% filter(time_hr < 850), 
                     aes(x = time_hr, y = scaled_gr, color = exp, group = un_lin, size = mark)) + geom_line() + 
  facet_wrap(~exp, nrow = 3, scales  = "free_x") + 
  scale_size_manual(values = c(3, 1)) +
  scale_color_manual(values = my_pal)

ggsave(filename = "Scaled_gr_trajectories_new.pdf", sacled_traj, units = "in", width = 16, height = 11)



#calculating the sd of the growth rates across the experiment to measure the change in growth rates throughoput the experiment fro each lineage
sd_scaled_gr = win_with_mean_time_per_sample %>% group_by(replicate, lineage)  %>%  dplyr::summarize(sd_scaled_gr = sd(scaled_gr[,1])) %>% 
  dplyr::mutate(exp = ifelse(replicate %in% c("A", "B", "C", "D"), "exp30", 
                             ifelse(replicate %in% c("E", "F", "G", "H"), "exp39", "gradual")))

sd_scaled_gr$exp = factor(sd_scaled_gr$exp, levels = c("exp30", "gradual", "exp39"))

sd_scaled_gr_plot = ggplot(sd_scaled_gr, aes(y = sd_scaled_gr, x = exp,  color = exp)) + geom_boxplot(size = 1) + 
  scale_color_manual(values = my_pal)

ggsave(filename  = "SD_scaled_growth_rate_top10.pdf", sd_scaled_gr, units = "in", width = 8, height = 6)


##computing statistics between distributions
head(sd_scaled_gr)
ggplot(sd_scaled_gr, aes(x = sd_scaled_gr)) + geom_histogram(bins = 15) + facet_wrap(~exp)


t.test(sd_scaled_gr %>% filter(exp == "exp39") %>% .$sd_scaled_gr, 
       sd_scaled_gr %>% filter(exp == "gradual") %>% .$sd_scaled_gr, exact = F)

t.test(sd_scaled_gr %>% filter(exp == "exp39") %>% .$sd_scaled_gr, 
       sd_scaled_gr %>% filter(exp == "exp30") %>% .$sd_scaled_gr, exact = F)

t.test(sd_scaled_gr %>% filter(exp == "gradual") %>% .$sd_scaled_gr, 
       sd_scaled_gr %>% filter(exp == "exp30") %>% .$sd_scaled_gr, exact = F)



##creating a temperature graph to illustrate the conditions for all three experiments
temp_graph = ggplot(time_dict, aes(x = Duration, y = Temp, color = exp)) + geom_line(size = 2) + theme_bw()  +
  scale_y_continuous(breaks = c(seq(30, 39, 1))) + xlim(c(0, 1000)) + labs(x = "Time (hr)", y = "Temperature (°C)") + 
  theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
        axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18), 
        strip.text.x = element_text(size = 16))


ggsave(filename = "temp_graph.pdf", plot = temp_graph, units = "in", width = 12, height = 6)





