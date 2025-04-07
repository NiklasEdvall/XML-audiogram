
library(readxl)
library(arsenal)

# Read excel sheet
dat <- read_excel("data/Exempel_FFdata.xlsx", sheet = 1)

# Specify factor variables and categoires
dat$Sex <- factor(dat$Sex, levels = c(1,2,3,4), labels = c("M", "F", "Other", "No response"))
dat$Hearing <- factor(dat$Hearing, levels = c(1,2,3), labels = c("No problem", "Sometimes", "General problem"))
dat$Tinnitus <- factor(dat$Tinnitus, levels = c(1,2,3), labels = c("No", "Yes, Sometimes", "Yes, Always"))
dat$Noise <- factor(dat$Noise, levels = c(1,2,3), labels = c("Low", "Medium", "High"))

# Make table
tab1 <- tableby(data = dat, Sex ~ Age + Hearing + Tinnitus + Noise,
                test = FALSE, # Include statistical test?
                total = FALSE, # Include column for total?
                digits = 1) # Digits to round to

# Print table
summary(tab1, text = TRUE)

# Save table in word document
write2word(tab1, "data/table1.docx")
