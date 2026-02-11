
# setup -------------------------------------------------------------------

library(tidyverse)

# Read in the data:

temp <-
  readxl::read_excel(
    "Copy of LHTS 1 2016.xlsx",
    sheet = "Veg Data Sheet",
    skip = 5
  ) %>% 
  janitor::clean_names() 

# vegetation level data ---------------------------------------------------

# Species and cover columns 1:

temp_grouped <- 
  temp %>% 
  select(
    cs = cs_1,
    species_name = species_name_2
  ) %>% 
  
  # Bind with species and cover columns 2:
  
  bind_rows(
    temp %>% 
      select(
        cs = cs_6,
        species_name = species_name_7
      )
  ) %>% 
  
  # Remove blank rows:
  
  drop_na(
    matches("species")
  ) %>% 
  
  # Detect species classified into groups
  
  mutate(
    group_id = 
      species_name %>% 
      str_detect("[A-Z][0-9]") %>% 
      cumsum()
  )

readxl::read_excel(
  "Copy of LHTS 1 2016.xlsx",
  sheet = "Veg Data Sheet",
  range = "A1:A100"
) %>% 
  janitor::clean_names() %>% 
  slice_head(n = 4)

#?readxl::read_excel

# Make a tidy data frame:

species_observations_tidy <-
  temp_grouped %>% 
  
  # Extract a unique vector of group ids:
  
  pull(group_id) %>% 
  unique() %>% 
  
  # Iterate across each id:
  
  map_df(
    function(id) {
      temp_grouped %>% 
        
        # Subset to the target of the iteration:
        
        filter(group_id == id) %>%
        
        # Add the group name column:
        
        mutate(
          group = pull(., species_name)[[1]]
        ) %>% 
        
        # Remove the first row (which was the group):
        
        slice(-1) %>% 
        
        # Reorder and remove columns:
        
        select(
          group, 
          species_name, 
          cs
        )
    }
  ) %>% 
  mutate(
    site_visit = "boy_howdy",
    .before = group
  )

# visit level information -------------------------------------------------

# Bring in visit-level information:

site_visits <-
  readxl::read_excel(
    "Copy of LHTS 1 2016.xlsx",
    sheet = "Veg Data Sheet",
    range = "A1:B100"
  ) %>% 
  janitor::clean_names() %>% 
  slice_head(n = 4) %>% 
  mutate(
    
    # Make our future column names:
    
    var = 
      c(
        "site",
        "observer",
        "county",
        "date"
      ),
    
    # Make a values column:
    
    value = 
      coalesce(x1, x2) %>% 
      str_remove(".*:") %>% 
      str_trim(),
    .keep = "none"
  ) %>% 
  
  # Reshape:
  
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>% 
  
  # Fix site name:
  
  mutate(
    method = "Releve",
    site = 
      site %>% 
      str_remove_all("Releve ") %>%
      str_extract("[A-Z]{3,5} ?[0-9]?-?[0-9]?")
  ) %>% 
  
  # Add primary key:
  
  mutate(
    key = "boy_howdy",
    .before = site
  ) %>% 
  
  # How to add site date pairs as a primary key
  
  mutate(
    date = mdy(date),
    key = 
      str_c(
        key, 
        as.integer(date),
        sep = "-"
      )
  )

# To Do: ------------------------------------------------------------------


# get the GIS GlobalIDs for releve points and map to all the site codes we've
# ever used
