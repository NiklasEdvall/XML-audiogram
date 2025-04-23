
library(readxl)
library(arsenal)

# Read excel sheet
dat <- read_excel("data/dat.xlsx", sheet = 1)

# Specify factor variables and categoires
dat$Sex <- factor(dat$Sex, levels = c(1,2,3,4), labels = c("Man", "Kvinna", "Annat", "Inget svar"))
dat$Hearing <- factor(dat$Hearing, levels = c(1,2,3,4), labels = c("Mycket bra", "Bra", "Dåligt", "Mycket dåligt"))
dat$Tinnitus <- factor(dat$Tinnitus, levels = c(1,2,3), labels = c("Nej", "Ja, ibland", "Ja, alltid"))
dat$Noise <- factor(dat$Noise, levels = c(1,2,3), labels = c("Nej", "Ja, i viss grad", "Ja, i hög grad"))

# Create table with custom labels
tab1 <- tableby(data = dat, 
                Sex ~ Age + Hearing + Tinnitus + Noise,
                test = FALSE,       # Exclude statistical tests
                total = FALSE,      # Exclude total column
                digits = 1)         # Round to 1 digit

# Change labels in table
labels(tab1) <- c(Sex = "Kön", 
                   Age = "Ålder", 
                   Hearing = "Självrapporterad hörsel", 
                   Tinnitus = "Förekomst av tinnitus",
                   Noise = "Bullerexponering")

# Print table
summary(tab1, text = TRUE)

# Save table in word document
write2word(tab1, "table1.docx")
