##Save p files as csvs

# Step 1: Create output folder if it doesn't exist
output_dir <- "./EDI_2025/EEMs_pfiles_2021_2025"

# Step 2: Find all p_ files recursively
p_files <- list.files("./Processed_Data/pfiles_forEDI_2021_2025_non_csv", pattern = "^p_", recursive = TRUE, full.names = TRUE)

# Step 3: Define processing function
process_pfile <- function(file_path) {
  # Read lines
  lines <- readLines(file_path)
  
  # Split on space or tab
  split_lines <- strsplit(lines, "[ \t]+")
  
  # Pad and bind
  df <- do.call(rbind, lapply(split_lines, function(x) {
    length(x) <- 45
    x
  }))
  
  # Clean up
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df |> select(2:44)
  colnames(df) <- NULL
  
  # Extract just the filename (no path)
  file_name <- basename(file_path)
  
  # Save to centralized folder
  output_path <- file.path(output_dir, paste0(file_name, ".csv"))
  write.csv(df, output_path, row.names = FALSE)
}

# Step 4: Apply to all files
lapply(p_files, process_pfile)


