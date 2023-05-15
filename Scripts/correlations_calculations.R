setwd("~/all_experiments/Calculating_IGE_for_all/")
rm(list = ls())
theme_set(theme_bw())

perc.rank <- function(x) trunc(rank(x))/length(x)

df = read.csv("~/all_experiments/sample_to_time/all_replicates_trajectories.csv") %>% select(-X)

##rounding the time (hr)
df$Duration = round(df$Duration, 1) 

##removing data points where turbidosttats measures are not accurate anymore, towards the end of the experiments
df = df %>% filter(replicate == "A" & Duration < 400 |
                     replicate == "B" & Duration < 350 |
                     replicate == "C" & Duration < 400 |
                     replicate == "D" & Duration < 400 |
                     replicate %in% c("E", "F", "G") & Duration < 850 |
                     replicate == "H" & Duration < 750 |
                     replicate %in% c("I", "J", "K") & Duration < 850 |
                     replicate  == "L" & Duration < 1100 |
                     replicate == "M" & Duration < 1000)



#loading a data frame that contains the conversion between time, sample, and mean generations
time_dict = read.csv("~/all_experiments/sample_to_time/Sample_to_time_dictionary.csv") %>% select(-c(X, Timestamp, exp))
time_dict$Duration = round(time_dict$Duration, 1)
head(time_dict)
head(df)


#calculating the correlation in lineages frequency for every replicate and every tim point compared to the first time point
cors_for_ige_all_tp1 = NULL

for(j in unique(df$replicate)){

  loop_df = all_with_sample %>% filter(replicate == j) %>% select(lineage, Duration, frequency) %>% spread(key = "Duration", value = "frequency")
  colnames(loop_df)[-1] = paste0("a", colnames(loop_df)[-1])
  
  first_tp = colnames(loop_df)[2]
  per_replicate_table = NULL
  
  for(i in 2:ncol(loop_df)){
    second_tp = colnames(loop_df)[i]
    
    both_tps_df = loop_df[loop_df[,2] > 0 & loop_df[,i] > 0, c(2, i)]
    
    percentile_ranks = as.data.frame(apply(both_tps_df, MARGIN = 2, perc.rank))
    cors = cor.test(percentile_ranks[,1], percentile_ranks[,2])
    new_df = data.frame(replicate = j, Duration = second_tp, stat = cors$estimate, p_value = cors$p.value)
    per_replicate_table[[i - 1]] = new_df
    
  }
  per_rep_df = do.call("rbind", per_replicate_table)
  
  cors_for_ige_all_tp1[[j]] = per_rep_df
}


#binding everything to one data frame and adding experiment name
cors_for_all_df_tp1 = do.call("rbind", cors_for_ige_all_tp1) %>%  mutate(exp = ifelse(replicate %in% c("A", "B", "C", "D"), "exp_30",
                                                                                      ifelse(replicate %in% c("E", "F", "G", "H"), "abrupt_39", "gradual_39"))) 

#rearranging the time variable
cors_for_all_df_tp1$Duration = sapply(strsplit(cors_for_all_df_tp1$Duration, "a"), `[`, 2) 
cors_for_all_df_tp1$p_value = round(cors_for_all_df_tp1$p_value, 4)

head(cors_for_all_df_tp1)
head(time_dict)

#merging the correlations table with the time to samples table to get sample number per time
merged = merge(cors_for_all_df_tp1, time_dict, by = c("Duration", "replicate"))
merged$Duration = as.numeric(merged$Duration)
head(merged)

#calculating mean correlations per sample per experiment
all_cors_means = merged %>% group_by(exp, Sample) %>% 
  dplyr::summarize(mean_cor = mean(stat), sd_cor = sd(stat), std_error = std.error(stat), sample_size = n(), mean_time = mean(Duration),
                   mean_gen = mean(Generations))
head(all_cors_means)

#getting the time where temperature changed to 39
x_intercept_for_gradual= merged %>% filter(Temp > 39 & exp == "gradual_39") %>% filter(Sample == min(Sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x
x_intercept_for_abrupt = merged %>% filter(Temp > 39 & exp == "abrupt_39") %>% filter(Sample == min(Sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x


#creating a color palette to fit my experiment colors
my_pal = c("#72c9dd", "#33b39f",  "#eb705e")
show_col(my_pal)
##changing the factor levels order to fit
all_cors_means$exp = factor(all_cors_means$exp, levels = c("exp_30", "gradual_39", "abrupt_39"))

#plotting
all_vs_tp1 = ggplot(all_cors_means %>% filter(sample_size > 2), aes(x = mean_time, y = mean_cor, color = exp)) +
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = mean_cor - std_error, ymax = mean_cor + std_error), width = 10) +
  scale_color_manual(values = my_pal, labels = c("30 °C", "Gradual 39 °C", "Abrupt 39 °C")) + 
  labs(x = "Time (hr)", y = "Pearson's r", title = "Correlation to the beginnig of the experiment", color = "Experiment") + 
  theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
        axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18), 
        strip.background=element_rect(fill="white"), 
        strip.text.x = element_text(size = 16)) +
  geom_vline(xintercept = x_intercept_for_gradual, linetype="dotted", color = "#33b39f", size=1.5) + 
  geom_vline(xintercept = x_intercept_for_abrupt, linetype="dotted", color = "#eb705e", size=1.5)
  

ggsave(filename = "cors_vs_tp1.jpg", all_vs_tp1, units = "in", width = 14, height = 8)
