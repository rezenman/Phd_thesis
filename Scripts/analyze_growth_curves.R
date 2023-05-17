setwd("~/all_experiments/fitness_assays/new_exp_30_39/new_39_plate/")

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(growthcurver)
library(ggpubr)
library(ggrepel)
library(ggsci)

##reading the data frame with the OD data
ls = list.files(pattern = "experiment2.csv")

##index of well to sample
well_to_sample = read.csv("well_to_sample_39.csv")
head(well_to_sample)


##read plates
plate1 = read.csv(ls[1], check.names = F)
head(plate1)
plate1$Time = seq(20, nrow(plate1) * 20, 20) #changing time to appear in minutes from the beginning
colnames(plate1)[2] = 'Temp'

df = plate1
index = well_to_sample

##starting from a concatenated table of the two plates
arrange_growth_curve_table_2 = function(file, index){
  
  df = df %>% select(c(Time, Temp, index$Well))
  
  start = colnames(df)[3]
  end = colnames(df)[ncol(df)]
  
  df_long = gather(df, key = "Well", value = "OD", start:end)
  df_long = merge(df_long, index)
  df_long$OD = as.numeric(df_long$OD)
  
  df_stats = df_long %>% group_by(Sample, Time) %>% dplyr::summarize(mean_od = mean(OD), sd_od = sd(OD))

  ##Calculating for each sample
  max_gr = NULL
  av_gr = NULL
  for(i in unique(df_stats$Sample)){
    sample = i
    
    per_sample = df_stats %>% filter(Sample == sample)
    gc_fit = SummarizeGrowth(per_sample$Time, per_sample$mean_od)
    gr = gc_fit$vals$t_gen
    av = gc_fit$vals$t_mid 
    max_gr[[sample]] = gr
    av_gr[[sample]] = av
  }
  
  max_gr_df = data.frame(Sample = unique(df_stats$Sample), max_gr = as.vector(unlist(max_gr)), av_gr = as.vector(unlist(max_gr)))
  
  df_stats = merge(df_stats, max_gr_df)
  plot = ggplot(df_stats, aes(x = Time, y = mean_od, color = Sample)) + geom_line(size = 1) +
    geom_point(size = 2) +
    theme_bw() + 
    geom_errorbar(aes(ymin=mean_od-sd_od, ymax=mean_od+sd_od), width=.3) + 
    geom_label(data = df_stats %>% group_by(Sample) %>% slice_max(Time, n = 1), size = 6, aes(label = round(max_gr, 1)))
  
  
  return(list(stats = df_stats, data = df_long, plot = plot))
} ##starting from a concatenated table of the two plates


exp_39 = arrange_growth_curve_table_2(file = plate1, index = index)

exp_39$stats$replicate = sapply(strsplit(exp_39$stats$Sample, ''), `[`, 1) ## changing replciate names
exp_39$stats = exp_39$stats %>% dplyr::mutate(replicate = ifelse(Sample == "Blank", "Blank", replicate)) 
exp_39$stats$step = sapply(strsplit(exp_39$stats$Sample, ""), `[`, 2)


all_growth_curves =  ggplot(exp_39$stats, aes(x = Time, y = mean_od, group = Sample)) + geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() + 
  geom_label_repel(data = exp_39$stats %>% group_by(Sample) %>% slice_max(Time, n = 1), size = 6, aes(label = round(max_gr, 1)), nudge_y = -0.5) +
  facet_wrap(~replicate, scales = "free_x")

ggsave(filename = "all_39_growth_curves.jpg", all_growth_curves, units = "in", width = 14, height = 8)
ggsave(filename = "all_39_growth_curves.pdf", all_growth_curves, units = "in", width = 14, height = 8)



# fitting polynominal to all acute and all gradual ------------------------

#aggregating all acute and all gradual

acute = exp_39$data %>% filter(Sample %in% c("E", "F", "G", "H")) %>% select(-rep)
acute$stress = "Acute"
gradual = exp_39$data %>% filter(Sample %in% c("I", "J", "K", "L", "M")) %>% select(-rep)
gradual$stress = "Gradual"
acute_grad = rbind(acute, gradual)

lm_acute = lm(OD ~ poly(Time, 3), data = acute)
summary(lm_acute)
acute_graph = ggplot(acute, aes(x = Time, y = OD)) + geom_point(size = 1, aes(color = Sample)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 2) + 
  theme_bw() + 
  geom_line(aes(group = Sample, color = Sample), size = 2) + 
  annotate(label = "R-squared = 0.32", geom = 'label', x = 1000, y = 2, color = 'darkblue')  + 
  scale_color_npg() + 
  theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
        axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18))

lm_gradual = lm(OD ~ poly(Time, 3), data = gradual)
summary(lm_gradual)
gradual_graph = ggplot(gradual %>% filter(Sample != "M"), aes(x = Time, y = OD)) + geom_point(size = 1, aes(color = Sample)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 2) + 
  theme_bw() + 
  geom_line(aes(group = Sample, color = Sample), size = 2) + 
  annotate(label = "R-squared = 0.85", geom = 'label', x = 1000, y = 2, color = 'darkblue') +
  scale_color_npg() + 
  theme(axis.text.x= element_text(size = 18), axis.title.x = element_text(size = 18), 
        axis.text.y= element_text(size = 18), axis.title.y = element_text(size = 18))

ggsave(filename = "Acute_growth_curves_with_fit.pdf", acute_graph, units = "in", width = 16, height = 8)
ggsave(filename = "Gradual_growth_curves_with_fit.pdf", gradual_graph, units = "in", width = 16, height = 8)
