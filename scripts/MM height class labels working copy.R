y <- "C:\\Users\\MMAB3\\Documents\\RStudio Projects\\DCVegMon\\data\\GRG Reference\\List-3-Central-Mesic-Hardwood-Forest-Eastern.xlsx"
refdata2 <- read_xlsx(y, sheet= 1, skip = 1, col_names = TRUE) %>%
  janitor::remove_empty(c("rows"))

## find the row numbers where new sections start  
refdata2$row_num <- seq.int(nrow(refdata2))

subcanopy <- which(grepl("Understory Trees", refdata2$Genus)) 

shrubs <- which(grepl("Shrubs", refdata2$Genus)) 
shrubs <- shrubs[[1]]

low_shrubs <- which(grepl("Low Shrubs", refdata2$Genus, fixed = TRUE)) 

vines <- which(grepl("Vines", refdata2$Genus)) 

forbs <- which(grepl("Forbs", refdata2$Genus)) 

grasses <- which(grepl("Grasses", refdata2$Genus)) 

ferns <- which(grepl("Ferns and Fern Allies", refdata2$Genus))

stop <- which(grepl("Exotic Invasive Species - Do Not Plant", refdata2$Genus))

canopy1 <- refdata2 %>%
  slice(3:subcanopy-1) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "canopy")
subcanopy1 <- refdata2 %>%
  slice(subcanopy:shrubs) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "subcanopy") %>%
  slice(2:(n()-1)) 
shrubs1 <- refdata2 %>%
  slice(shrubs:low_shrubs) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "shrubs") %>%
  slice(2:(n()-1)) 
low_shrubs1 <- refdata2 %>%
  slice(low_shrubs:vines) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "low shrubs") %>%
  slice(2:(n()-1)) 
vines1 <- refdata2 %>%
  slice(vines:forbs) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "vines") %>%
  slice(2:(n()-1)) 
forbs1 <- refdata2 %>%
  slice(forbs:grasses) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "forbs") %>%
  slice(2:(n()-1)) 
grasses1 <- refdata2 %>%
  slice(grasses:ferns) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "graminoids") %>%
  slice(2:(n()-1)) 
ferns1 <- refdata2 %>%
  slice(grasses:stop) %>%
  dplyr::select(-row_num) %>%
  add_column(HeightClass = "ferns") %>%
  slice(2:(n()-1)) 


  refcode2 <- read_xlsx(y, sheet = 1, range = "A1:A1", col_names = FALSE)
refcode2 <- list(refcode2[[1,1]])

df<- bind_cols(refdata2, refcode)