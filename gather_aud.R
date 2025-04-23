library(xml2)
library(dplyr)

# Get a list of all XML files in /data
xml_files <- list.files("data", pattern = "\\.xml$", full.names = TRUE)

# Initialize an empty list to store results
all_data <- list()

# Loop through each XML file
for (file in xml_files) {
  # Load the XML file
  doc <- read_xml(file)
  
  # Get namespace
  ns <- xml_ns(doc)
  
  # Extract FirstName from filename (before the first "_")
  filename_base <- basename(file)
  first_name <- sub("_.*", "", filename_base)
  
  # Extract creation date from XML
  create_date <- xml_text(xml_find_first(doc, "//d1:SaData/d1:Session/d1:Created", ns))
  
  # Extract TonePoint data
  tone_nodes <- xml_find_all(doc, "//d1:SaData/d1:Session/d1:Test/d1:Data/d1:RecordedData/d1:Measured/d1:Tone", ns)
  
  data_list <- lapply(tone_nodes, function(tone_node) {
    ear_side <- xml_text(xml_find_first(tone_node, "d1:Earside", ns))  # Extract ear side
    
    tonepoints <- xml_find_all(tone_node, "d1:TonePoint", ns)
    
    data.frame(
      Frequency = as.numeric(xml_text(xml_find_all(tonepoints, "d1:Frequency", ns))),
      IntensityUT = as.numeric(xml_text(xml_find_all(tonepoints, "d1:IntensityUT", ns))),
      StatusUT = xml_text(xml_find_all(tonepoints, "d1:StatusUT", ns)),
      Transducer = xml_text(xml_find_all(tonepoints, "d1:Transducer", ns)),
      EarSide = ear_side,
      Filename = filename_base,
      sub_id = first_name,
      CreateDate = create_date,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all TonePoint data for this file
  if (length(data_list) > 0) {
    all_data[[file]] <- bind_rows(data_list)
  }
}

# Combine all data into a single data frame
dat <- bind_rows(all_data)

save(dat, file = "data/dat.Rda")
