setwd("~/all_experiments/Distribution_distance_measurments/Bag_of_words/")
rm(list = ls())

library(ggplot2)
library(dplyr)
library(lsa)
library(scales)
library(plotrix)

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




time_dict = read.csv("~/all_experiments/sample_to_time/Sample_to_time_dictionary.csv") %>% select(-c(X, Timestamp, exp))
time_dict$Duration = round(time_dict$Duration, 1)
head(time_dict)
head(df)


all_with_sample = merge(df, time_dict, by = c("replicate", "Duration")) %>% 
  dplyr::mutate(exp = ifelse(replicate %in% c("A", "B", "C", "D"), "exp_30", 
                      ifelse(replicate %in% c("E", "F", "G", "H"), "abrupt_39", "gradual_39")))

cosine_per_experiment = NULL
for(i in unique(all_with_sample$exp)){
  loop_df = all_with_sample %>% filter(exp == i) 
  unique_samples = unique(loop_df$Sample) 

  cosine_dfs = NULL
  for(j in unique_samples){
    print(j)
    per_sample_df = loop_df %>%  filter(Sample == j) %>% select(replicate, lineage, frequency)
    to_wide = per_sample_df %>% spread(key = "replicate", value = "frequency")
    to_wide[is.na(to_wide)] = 0
    
    if(ncol(to_wide) > 2){
    cosine = cosine(as.matrix(to_wide[,-1]))
    cosine_df = data.frame(row=rownames(cosine)[row(cosine)[upper.tri(cosine)]], 
                col=colnames(cosine)[col(cosine)[upper.tri(cosine)]], 
                cos=cosine[upper.tri(cosine)])
    cosine_df$sample = j
    cosine_dfs[[j]] = cosine_df
  } else { next }

  all_samples_cosine_df = do.call("rbind", cosine_dfs)
  all_samples_cosine_df$exp = i
  cosine_per_experiment[[i]] = all_samples_cosine_df
 }

}
all_cosines = do.call("rbind", cosine_per_experiment)

samp_to_time = samp_to_time %>% dplyr::mutate(exp = ifelse(exp  == "exp_39", "abrupt_39", ifelse(exp == "exp_30", "exp_30", "gradual_39")))
mean_time_per_exp = samp_to_time %>% group_by(sample, exp) %>% dplyr::summarize(mean_time = mean(Duration), mean_gen = mean(Generations))


##calculating mean and std of each time-ppoinst for all experiments
head(all_cosines)

mean_cosine_sim = all_cosines %>% group_by(sample, exp) %>%
  dplyr::summarize(mean = mean(cos), std = sd(cos), num_samples = n(), std_error = std.error(cos))

##Merging 

merged1 = merge(mean_cosine_sim, mean_time_per_exp, by = c("sample", "exp"))
head(merged1)

x_intercept_for_gradual= samp_to_time %>% filter(Temp > 39 & exp == "gradual_39") %>% filter(sample == min(sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x
x_intercept_for_abrupt= samp_to_time %>% filter(Temp > 39 & exp == "abrupt_39") %>% filter(sample == min(sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x


my_pal = c("#72c9dd", "#33b39f",  "#eb705e")
show_col(my_pal)
##changing the factor levels order to fit
merged1$exp = factor(merged1$exp, levels = c("exp_30", "gradual_39", "abrupt_39"))


##plotting cosine similarity by sample number
cos_time = ggplot(data = merged1, aes(x = mean_time, y = mean, color = exp)) +
            geom_line(size = 1) + 
            geom_point(size = 2) +
            theme_bw() + 
            geom_errorbar(aes(ymin = mean - std_error, ymax = mean + std_error), width = 10) +
            labs(x = "Sample", y ="Mean Cosine similarity", title = "Cosine similarity by time", color = "Experiment") +
            scale_color_manual(values = my_pal, labels = c("30 °C", "Gradual 39 °C", "Abrupt 39 °C")) +
            theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
                  axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18), 
                  strip.background=element_rect(fill="white"), 
                  strip.text.x = element_text(size = 16)) +
            geom_vline(xintercept = x_intercept_for_gradual, linetype="dotted", color = "#33b39f", size=1.5) + 
            geom_vline(xintercept = x_intercept_for_abrupt, linetype="dotted", color = "#eb705e", size=1.5)


ggsave(filename = "Cosine_similarity_all_experiments_time.pdf", plot = cos_time, units = "in", width = 12, height = 5)
