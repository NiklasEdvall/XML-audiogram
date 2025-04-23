library(dplyr)
library(ggplot2)
library(ggpubr)

#Load gathered audiogram data
load("data/dat.Rda")

#Remove 125 Hz
dat <- dat[dat$Frequency != 125, ]

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

# Compute log2 frequencies (NOT factors)
data_L$log2_freq <- log2(data_L$Frequency)
summary_stats_L$log2_freq <- log2(summary_stats_L$Frequency)

data_R$log2_freq <- log2(data_R$Frequency)
summary_stats_R$log2_freq <- log2(summary_stats_R$Frequency)

# Define the frequencies you want to show as axis ticks
tick_freqs <- c(250, 500, 1000, 2000, 3000, 4000, 6000, 8000, 10000, 12500, 16000)

log_min <- log2(min(tick_freqs)) - 0.1  # Add padding below the minimum frequency
log_max <- log2(max(tick_freqs)) + 0.1    # Add padding above the maximum frequency

# Plot
L.aud <- ggplot(data_L, aes(x = log2_freq, y = IntensityUT, group = sub_id)) +
          geom_vline(xintercept = log2(8000), color = "grey", linetype = "dashed") +
          geom_line(aes(linetype = "Individual"), linewidth = 0.5, alpha = 0.25) +  
          geom_point(shape = 16, size = 1, position = position_jitter(width = 0.05), alpha = 0.25) +
          geom_line(data = summary_stats_L, aes(x = log2_freq, y = mean_threshold, group = 1, color = "Mean", linetype = "Mean"), linewidth = 1.5) +
          scale_color_manual(values = c(setNames(rainbow(length(unique(data_L$sub_id))), unique(data_L$sub_id)), "Mean" = "#0448b5"), name = "Legend") +
          scale_linetype_manual(values = c("solid", "solid"), name = "Line Type") +
          scale_x_continuous(
            breaks = log2(tick_freqs),
            labels = tick_freqs,
            limits = c(log_min, log_max)  # Set limits with padding
          ) +
          labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Left ear thresholds") +
          scale_y_reverse(limits = c(100, -15)) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)
          )

R.aud <- ggplot(data_R, aes(x = log2_freq, y = IntensityUT, group = sub_id)) +
  geom_vline(xintercept = log2(8000), color = "grey", linetype = "dashed") +
  geom_line(aes(linetype = "Individual"), linewidth = 0.5, alpha = 0.25) +  
  geom_point(shape = 16, size = 1, position = position_jitter(width = 0.05), alpha = 0.25) +
  geom_line(data = summary_stats_L, aes(x = log2_freq, y = mean_threshold, group = 1, color = "Mean", linetype = "Mean"), linewidth = 1.5) +
  scale_color_manual(values = c(setNames(rainbow(length(unique(data_R$sub_id))), unique(data_R$sub_id)), "Mean" = "#9e0505"), name = "Legend") +
  scale_linetype_manual(values = c("solid", "solid"), name = "Line Type") +
  scale_x_continuous(
    breaks = log2(tick_freqs),
    labels = tick_freqs,
    limits = c(log_min, log_max)  # Set limits with padding
  ) +
  labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Right ear thresholds") +
  scale_y_reverse(limits = c(100, -15)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Arrange plots
ggarrange(L.aud, R.aud, nrow = 1, ncol = 2)
