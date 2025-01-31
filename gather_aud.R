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
  
  # Extract ClientInfo details
  first_name <- xml_text(xml_find_first(doc, "//d1:SaData/d1:ClientInfo/d1:FirstName", ns))
  create_date <- xml_text(xml_find_first(doc, "//d1:SaData/d1:Session/d1:Created", ns))
  
  # Extract TonePoint data
  tone_nodes <- xml_find_all(doc, "//d1:SaData/d1:Session/d1:Test/d1:Data/d1:RecordedData/d1:Measured/d1:Tone", ns)
  
  data_list <- lapply(tone_nodes, function(tone_node) {
    ear_side <- xml_text(xml_find_first(tone_node, "d1:Earside", ns))  # Extract ear side from element
    
    tonepoints <- xml_find_all(tone_node, "d1:TonePoint", ns)  # Corrected case
    
    data.frame(
      Frequency = as.numeric(xml_text(xml_find_all(tonepoints, "d1:Frequency", ns))),
      IntensityUT = as.numeric(xml_text(xml_find_all(tonepoints, "d1:IntensityUT", ns))),
      StatusUT = xml_text(xml_find_all(tonepoints, "d1:StatusUT", ns)),
      Transducer = xml_text(xml_find_all(tonepoints, "d1:Transducer", ns)),
      EarSide = ear_side,
      Filename = basename(file),  # Add filename
      FirstName = first_name,
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
final_data <- bind_rows(all_data)

