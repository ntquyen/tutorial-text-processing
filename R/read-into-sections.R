source("./R/process_inputs.R")

input_dir <- "./data/docs"
filenames <- list.files(input_dir, pattern = "\\.htm$")

for (i in 16:length(filenames)) {
  
  input_filename <- paste(input_dir, "/", filenames[i], sep = "")
  
  output_dir <- paste("./data/drafts/", filenames[i], sep = "")
  dir.create(output_dir, showWarnings = FALSE)
  
  resolution_filename <- paste("./data/resolution_", filenames[i], ".csv", sep = "")
  
  process(input_filename, output_dir, resolution_filename)
}