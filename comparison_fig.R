library(tidyr)
library(dplyr)
library(readxl)
library(arsenal)

# Read excel sheet
comp_dat <- read_excel("data/comparison_aud.xlsx", sheet = 1)

# Make "studie" factor var
comp_dat$Studie <- factor(comp_dat$Studie)

data_long <- comp_dat %>%
  pivot_longer(
    cols = -Studie,                     # all columns except 'Studie'
    names_to = "Frequency",             # new column for the frequency (as character)
    values_to = "Threshold"             # new column for the values
  ) %>%
  mutate(Frequency = as.numeric(Frequency))  # optional: convert frequency to numeric

# Log frequencies
data_long$log2_freq <- log2(data_long$Frequency)

# Define the frequencies you want to show as axis ticks
tick_freqs <- c(250, 500, 1000, 2000, 3000, 4000, 6000, 8000, 10000, 12500, 16000)

log_min <- log2(min(tick_freqs)) - 0.1  # Add padding below the minimum frequency
log_max <- log2(max(tick_freqs)) + 0.1    # Add padding above the maximum frequency

# Define custom colors
custom_colors <- c(
  "Niklas"    = "#1b9e77",
  "Picklas"   = "#d95f02",
  "Robert"    = "#7570b3",
  "Tobias"    = "#e7298a",
  "Lugengrus" = "#2629de"
)

#Make plot
ggplot(data_long, aes(x = log2_freq, y = Threshold, group = Studie, color = Studie, shape = Studie)) +
  geom_vline(xintercept = log2(8000), color = "grey", linetype = "dashed") +
  geom_line(linewidth = 0.5, alpha = 0.75, na.rm = TRUE) +
  geom_point(size = 1.5, position = position_jitter(width = 0, height = 0), alpha = 0.75) +
  scale_color_manual(values = custom_colors) +  # <- custom color mapping
  scale_x_continuous(
    breaks = log2(tick_freqs),
    labels = tick_freqs,
    limits = c(log_min, log_max)
  ) +
  labs(x = "Frequency (Hz)", y = "Threshold (dB)", title = "Jämförelse audiogram") +
  scale_y_reverse(limits = c(100, -20), breaks = seq(100, -20, by = -10)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1),
    legend.position = c(0.05, 0.05),
    legend.justification = c("left", "bottom"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = "black"),
    legend.box.background = element_rect(color = "black", size = 0.5),
    plot.title = element_text(hjust = 0.5)
  )
