
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