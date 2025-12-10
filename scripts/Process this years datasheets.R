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


## This script takes your individual releve datasheets and merges them into a single .csv file.
## IMPORTANT: 
## (1) Your datasheets must be prepped. The releve code (releve number+YYYY) must be in cell B2 of the "woody" tab. 
## (2) Copies of all the .xlsx files must be in Data > This Years Releve Datasheets
## (3) Answer this question:

ThisYear <- 1999 #What year is it? 

SavingFileIn <- paste("data/Processed Releve Data/", ThisYear, ".csv", sep = "")


##Finds all releve xlsx within directory
##This is a COPY of the data that is manually moved to github (or wherever you're working out of)

file.list <- list.files(path = here::here("data", "This Years Releve Datasheets"), full.names = TRUE, recursive = TRUE)

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
Releve_Observations <- all_df[hi<10,]

## Creates the .csv

write.csv(Releve_Observations, file = SavingFileIn, col.names = TRUE, row.names= false)

## Creates a new folder called "<ThisYear> data" and copies the contents of This Years Releve Datasheets into it

newfolder <- paste(ThisYear, "data")

file.copy(from = "data\This Years Releve Datasheets", to = newfolder, recursive = TRUE)

## Cleans up the environment

rm(single_df)
rm(hi)
rm(x)
