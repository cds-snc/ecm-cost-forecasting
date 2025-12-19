# This script copies all of the data and the report output to a OneDrive
# folder for easy sharing. It is meant to be run post-rendering.

folders_to_copy <- c("output", "data")

path_to_onedrive <- file.path(Sys.getenv("OneDrive"), "analysis")
project_slug <- basename(getwd())


for (folder in folders_to_copy) {
  
  output_dir <- file.path(path_to_onedrive, project_slug, folder)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  } 
  
  file.copy(
    from = folder, 
    to = file.path(path_to_onedrive, project_slug),
    overwrite = TRUE,
    recursive = TRUE
  )
  
}