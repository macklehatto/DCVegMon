
# confirm that all the releves are in a folder called "releve" or something

list <- str_extract_all(folder_list, pattern = "/[A-Z]{4}/.{9}") %>% 
  unique() %>% 
  as_tibble()

# This confirms that there are 6 subfolders called "Releves" that we need to
# loop through

target_string <- str_detect(list, pattern = "Releves")

sum(target_string)

# build a function that loops through all the folders in Archived Veg Data
# and reads in just the releve datasheets

# copy folders and xlsx$ from N:\PARKS\Natural Resources\Monitoring\Transects, Releves, and Photo Monitoring\Archived Monitoring Data 
# in folders called "Raw Data" to "data/raw"

library(fs)
library(purrr)
library(stringr)

copy_raw_xlsx_files <- function(source_root, dest_root) {
  
  # Create destination folder if needed
  dir_create(dest_root)
  
  # 1️⃣ Find all "Raw Data" directories
  raw_dirs <- dir_ls(
    path = source_root,
    recurse = TRUE,
    type = "directory"
  ) |>
    keep(~ str_detect (path_file(.x), regex("raw data", ignore_case = TRUE)))
  
  if (length(raw_dirs) == 0) {
    message("No 'Raw Data' folders found.")
    return(invisible(NULL))
  }
  
  # 2️⃣ Find all .xlsx files inside them
  xlsx_files <- map(raw_dirs, ~ dir_ls(.x, glob = "*.xlsx")) |>
    flatten_chr()
  
  if (length(xlsx_files) == 0) {
    message("No .xlsx files found.")
    return(invisible(NULL))
  }
  
  # 3️⃣ Copy with renamed filenames to avoid overwriting
  walk(xlsx_files, function(file_path) {
    
    parent_name <- path_file(path_dir(file_path))
    
    new_name <- paste0(parent_name, "_", path_file(file_path))
    
    dest_path <- path(dest_root, new_name)
    
    file_copy(file_path, dest_path, overwrite = FALSE)
    
    message("Copied: ", file_path)
  })
  
  message("Done.")
}

copy_raw_xlsx_files(
  source_root = "N:\\PARKS\\Natural Resources\\Monitoring\\Transects, Releves, and Photo Monitoring\\Archived Monitoring Data\\SLPR",
  dest_root   = "data/raw"
)
