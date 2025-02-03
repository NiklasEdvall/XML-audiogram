# Audiogram from XML

Read audiogram data from XML, gather and plot threshold and mean value. Example data (n = 2) exported from Equinox Suite included for testing.

* **gather_aud.R** - Loop through all XML-files in /data, gather specific parameters in data frame and save as /data/dat.Rda
* **make_fig.R** - Load prepared data frame /data/dat.Rda and plot all subjects (black) and average thresholds (blue/red) for right and left ears separately. Vertical grey line denote standard and high frequency audiometric range.

<img width="1088" alt="aud_example" src="https://github.com/user-attachments/assets/73d4b7da-a381-46a4-8c7e-66fa1ab63a18" />
