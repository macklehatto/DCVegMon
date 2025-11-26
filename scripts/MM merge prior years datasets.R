##install packages
#install.packages('skimr', repos = "http://cran.us.r-project.org")
#install.packages('kableExtra', repos = "http://cran.us.r-project.org")
#install.packages('janitor', repos = "https://cran.r-project.org/package=janitor")
#install.packages('pivottabler', repos = "http://cran.us.r-project.org")
#install.packages('tidyverse')
#install.packages('vegan')
#install.packages('here')
#install.packages("ggplot2")

## attach libraries
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(here)
library(ggplot2)
#library(skimr)  # not used so far
#library(kableExtra)  #not used so far

### This is the code to read in the 2020 and later data ---------------------------------

##to do:
## refactor function - low priority

##Find all releve xlsx within directory
##This is a COPY of the data that is manually moved to github (or wherever you're working out of)

file.list <- list.files(path = here::here("data", "2022 data"), full.names = TRUE, recursive = TRUE)

## Extract data from woody, forbs and grasses worksheets

extract_data <- function(x)
{
  woody <- read_xlsx(x, sheet = "Woody", skip = 8)%>%
    clean_names() %>%
    remove_empty(c("rows"))%>%
    mutate(
      rarity_status = as.character(rarity_status), 
      cc = as.numeric(cc)
    ) %>%
    dplyr::select("scientific_name", "common_name", "x3" , "physiognamy", "mn_nativity", "mnwi", "rarity_status" , "id" , "cc" , "cc_range", "x12" , "midpoint_cc", "p", "p_c")
  
  forbs <- read_xlsx(x, sheet = "Forbs", skip = 8)%>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows"))%>%
    mutate(
      rarity_status = as.character(rarity_status), 
      cc = as.numeric(cc)
    ) %>%
    dplyr::select("scientific_name", "common_name", "x3" , "physiognamy", "mn_nativity", "mnwi", "rarity_status" , "id" , "cc" , "cc_range", "x12" , "midpoint_cc", "p", "p_c")
  
  grasses <- read_xlsx(x, sheet = "Grasses", skip = 8)%>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows"))%>%
    mutate(
      rarity_status = as.character(rarity_status), 
      cc = as.numeric(cc)
    ) %>%
    dplyr::select("scientific_name", "common_name", "x3" , "physiognamy", "mn_nativity", "mnwi", "rarity_status" , "id" , "cc" , "cc_range", "x12" , "midpoint_cc", "p", "p_c")
  
  relevecode <- read_xlsx(x, sheet = "Woody", range = "B2:B2", col_names = FALSE) #grabs the relevecode from C2 of Woody sheet
  
  data_frame <- (bind_rows(woody, forbs, grasses)) #combine the woody, forbs, and grasses data rows
  
  data_frame['releveCode'] <- list(relevecode[[1,1]]) #assign value from woody cell C2 to new column in combined dataframe (indicates releve code (=location code + YYYY))
  
  
  ##getting the year out of the releve code
  
  relevecode_str <- as.character(relevecode[[1,1]])
  
  data_frame$releveyear <- substr(relevecode_str,
                                  nchar(relevecode_str) - 3,
                                  nchar(relevecode_str))
  
  data_frame$releveSite <- substr(relevecode_str,
                                  1,
                                  nchar(relevecode_str) - 4)
  
  return(data_frame)
}

##create empty dataframe with all the required columns

all_df <- data.frame(
  "releveSite"=character(), 
  "releveDate"=as.Date(character()), 
  "releveCode" = character(), 
  "releveyear" = numeric(), 
  "scientific_name"=character(), 
  "common_name"=character(), 
  "x3" =numeric(), 
  "physiognamy"=character(), 
  "mn_nativity" = character(), 
  "mnwi"= character(), 
  "rarity_status" = character(), 
  "id" = character(), 
  "cc" = numeric(), 
  "cc_range" = character(), 
  "x12" = numeric(), 
  "midpoint_cc"= numeric(), 
  "p" = numeric(), 
  "p_c" = numeric(), 
  stringsAsFactors = FALSE
  ) 

##loop through all files in the folder 

for (x in file.list){
  print(x) #this helps troubleshoot if the code gets hung up somewhere
  single_df <- extract_data(x)
  all_df <- rbind(all_df, single_df)
}

##removes all rows with greater than 10 "NA" cells 

hi <- apply(all_df, 1, function(x) sum(is.na(x)))
Releve_Observations_2022 <- all_df[hi<10,]

write.csv(Releve_Observations_2022, file = "data/2022 Releve Observations.csv", row.names = TRUE)

