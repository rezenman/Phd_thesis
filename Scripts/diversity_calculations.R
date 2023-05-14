setwd("~/all_experiments/data_for_final/")
theme_set(theme_bw())

library(ggplot2)
library(tidyr)
library(dplyr)


##reading the data frame contatining all lineages frequency for all experiments
df = read.csv("~/all_experiments/sample_to_time/all_replicates_trajectories.csv")

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


##creating a function to calculate diversity, given a vector of frequencies and a Q - the order
measure_div = function(vector, q){
  div = NULL  
  name = paste0("q_", q)
  vector = vector[vector > 0]
    if(q != 1){
      div = sum(vector^q)
      diversity = div^(1/(1-q))
    } else {
      sub_div = -1*(sum(vector * log(vector)))
      diversity = exp(sub_div)
  }
  return(diversity)
}

##calculating diversity for order 0 and 1 for each replicate and each sample
diversity_df = df %>% group_by(replicate, Duration) %>% dplyr::summarize(diversity_0 = round(measure_div(frequency, 0)),
                                                                         diversity_1 = round(measure_div(frequency, 1)))

head(diversity_df)
##reordering the fdata frame from wide to long, and adding another coulumn with the Experiment variable 
diversity_df = diversity_df %>% gather(key = "Qindex", value = "Effective_lin", "diversity_0":"diversity_1") %>% 
  dplyr::mutate(exp = ifelse(replicate %in% c("A", "B", "C", "D"), "exp_30", 
                             ifelse(replicate %in% c("E", "F", "G", "H"), "abrupt_39", "gradual_39")))


##reading the time to sample dictionary
time_dict = read.csv("~/all_experiments/sample_to_time/Sample_to_time_dictionary.csv") %>% select(-c(X, Timestamp, exp))
time_dict$Duration = round(time_dict$Duration, 1)
head(time_dict)

#merging to add sample number to the diversity df
head(diversity_df)
all_divs = merge(diversity_df, time_dict, by = c("replicate", "Duration"))
head(all_divs)

#summarizing the mean effective number per sample for each experiment and other parameters
all_divs_means = all_divs %>% group_by(exp, Sample, Qindex) %>% 
  dplyr::summarize(mean_div = mean(Effective_lin), sd_div = sd(Effective_lin), sample_size = n(), mean_time = mean(Duration),
                                                                        mean_gen = mean(Generations))
head(all_divs)
head(all_divs_means)


#adding the time where transtion to 39 degrees occur
x_intercept_for_gradual= all_divs %>% filter(Temp > 39 & exp == "gradual_39") %>% filter(Sample == min(Sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x
x_intercept_for_abrupt= all_divs %>% filter(Temp > 39 & exp == "abrupt_39") %>% filter(Sample == min(Sample)) %>% dplyr::summarize(x = mean(Duration)) %>% .$x

##averaging the diversity per sample for all replicates and adding label to show the effective number at the enbd of the experiment
mean_div_2 = all_divs_means %>%  group_by(exp, Qindex)  %>%  filter(sample_size > 2) %>% 
  dplyr::mutate(labl = ifelse(mean_time == max(mean_time), round(mean_div, 0) ,NA))


##creating a color palette
my_pal = c("#72c9dd", "#33b39f",  "#eb705e")

##changing the factor levels order to fit
mean_div_2$exp = factor(mean_div_2$exp, levels = c("exp_30", "gradual_39", "abrupt_39"))

ggplot(mean_div_2, aes(x = mean_time, y = mean_div, color = exp, label = labl)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Qindex, nrow = 2) + 
  scale_color_manual(values = my_pal, labels = c("30 °C", "Gradual 39 °C", "Abrupt 39 °C"), ) +
  theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
        axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18), 
        strip.background=element_rect(fill="white"), 
        strip.text.x = element_text(size = 16)) +
  geom_ribbon(alpha = .15, aes(ymin = mean_div - sd_div, ymax = mean_div + sd_div), linetype = "dashed") +
  labs(x = "Time (hr)", y = "Effective number of lineages", title = "Diversity by time", color = "Experiment") + geom_label() +
  geom_vline(xintercept = x_intercept_for_gradual, linetype="dotted", color = "#33b39f", size=1.5) + 
  geom_vline(xintercept = x_intercept_for_abrupt, linetype="dotted", color = "#eb705e", size=1.5)


ggsave(filename = "diversity_plot_time_normal.pdf", divs_plot, units = "in", width = 16, height = 13)
