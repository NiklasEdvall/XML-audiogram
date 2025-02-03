library(dplyr)
library(ggplot2)
library(ggpubr)

#Load gathered audiogram data
load("data/dat.Rda")

# Filter Ears
data_L <- dat %>% filter(EarSide == "Left")
data_R <- dat %>% filter(EarSide == "Right")

# Compute mean and standard deviation per frequency
summary_stats_L <- data_L %>%
  group_by(Frequency) %>%
  summarize(mean_threshold = mean(IntensityUT),
            sd_threshold = sd(IntensityUT), .groups = 'drop')

summary_stats_R <- data_R %>%
  group_by(Frequency) %>%
  summarize(mean_threshold = mean(IntensityUT),
            sd_threshold = sd(IntensityUT), .groups = 'drop')

# Create plots
L.aud <- ggplot(data_L, aes(x = factor(Frequency), y = IntensityUT, group = FirstName)) +
          geom_vline(xintercept = which(levels(factor(data_L$Frequency)) == "8000") + 0.5, color = "grey", linetype = "dashed") +
          geom_line(aes(linetype = "Individual"), linewidth = 0.5) +  
          geom_point(size = 1) +
          geom_line(data = summary_stats_L, aes(x = factor(Frequency), y = mean_threshold, group = 1, color = "Mean", linetype = "Mean"), linewidth = 1.5) +
          scale_color_manual(values = c(setNames(rainbow(length(unique(data_L$FirstName))), unique(data_L$FirstName)), "Mean" = "#0448b5"), name = "Legend") +
          scale_linetype_manual(values = c("solid", "solid"), name = "Line Type") +
          labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Left ear thresholds") +
          scale_y_reverse(limits = c(80,-10)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1), legend.position = "none", plot.title = element_text(hjust = 0.5))

R.aud <- ggplot(data_R, aes(x = factor(Frequency), y = IntensityUT, group = FirstName)) +
          geom_vline(xintercept = which(levels(factor(data_R$Frequency)) == "8000") + 0.5, color = "grey", linetype = "dashed") +
          geom_line(aes(linetype = "Individual"), linewidth = 0.5) +  
          geom_point(size = 1) +
          geom_line(data = summary_stats_R, aes(x = factor(Frequency), y = mean_threshold, group = 1, color = "Mean", linetype = "Mean"), linewidth = 1.5) +
          scale_color_manual(values = c(setNames(rainbow(length(unique(data_L$FirstName))), unique(data_L$FirstName)), "Mean" = "#9e0505"), name = "Legend") +
          scale_linetype_manual(values = c("solid", "solid"), name = "Line Type") +
          labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Right ear thresholds") +
          scale_y_reverse(limits = c(80,-10)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1), legend.position = "none", plot.title = element_text(hjust = 0.5))

# Arrange plots
ggarrange(L.aud, R.aud, nrow = 1, ncol = 2)
