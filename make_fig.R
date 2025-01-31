library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv(text = "ID,Freq,Threshold,Ear
1,125,5,L
1,250,10,L
1,500,5,L
1,1000,15,L
1,125,5,R
1,250,10,R
1,500,5,R
1,1000,15,R
2,125,15,L
2,250,11,L
2,500,15,L
2,1000,20,L
2,125,15,R
2,250,15,R
2,500,15,R
2,1000,20,R")

# Filter for Ear == "L"
data_L <- data %>% filter(Ear == "L")

# Compute mean and standard deviation per frequency
summary_stats <- data_L %>%
  group_by(Freq) %>%
  summarize(mean_threshold = mean(Threshold),
            sd_threshold = sd(Threshold), .groups = 'drop')

# Create the plot
ggplot(data_L, aes(x = factor(Freq), y = Threshold, group = ID, color = factor(ID))) +
  geom_line(aes(linetype = "Individual"), size = 1) +  
  geom_point(size = 2) +
  geom_line(data = summary_stats, aes(x = factor(Freq), y = mean_threshold, group = 1, color = "Mean", linetype = "Mean"), size = 1.5) +
  #geom_errorbar(data = summary_stats, aes(x = factor(Freq), ymin = mean_threshold - sd_threshold, ymax = mean_threshold + sd_threshold, color = "Mean"), width = 0.2) +
  scale_color_manual(values = c(setNames(rainbow(length(unique(data_L$ID))), unique(data_L$ID)), "Mean" = "black"), name = "Legend") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Line Type") +
  labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Thresholds by Frequency for Left Ear") +
  theme_minimal()
