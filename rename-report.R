output_dir <- "output"

# Get the current timestamp in YYYY-MM-DD format
timestamp <- format(Sys.time(), "%Y%m%d")

# Newline separated list of all output files rendered (passed only to post-render).
# See: https://quarto.org/docs/projects/scripts.html
files <- unlist(strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n"))


for (file in files) {
  
  # Check if the output directory exists and if it doesn't, make it
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  } 
  
  original_filename <- file
  
  # Construct the new filename
  new_filename <- sub("\\.html$", original_filename, paste0(timestamp, "_", ".html"))

  # Construct the output path
  output_path <- file.path(output_dir, new_filename)
  
  # Rename the file
  file.copy(original_filename, output_path, overwrite = TRUE)
  

}
