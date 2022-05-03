# Set the file name
file_name <-"biodiversity-data.tar.gz"

# List all the files contained in the file
untar(file_name, list = TRUE)

# Extract data
untar(file_name)

# Import file with occurance data
library(data.table)
occurence <- fread("biodiversity-data/occurence.csv",nrow=100)
#View(occurence)

# The original file contains 37 columns
names(occurence)

library(dplyr)
# Select only desired columns
occurence <- occurence %>% select("occurrenceID",
       "vernacularName",
       "scientificName",
       "individualCount",
       "longitudeDecimal",
       "latitudeDecimal",
       "continent",
       "country",
       "countryCode",
       "stateProvince",
       "eventDate",
       "references",
       "eventTime")

## Import WHOLE FILE
occurence <- fread("biodiversity-data/occurence.csv",encoding='UTF-8')

head(occurence)

## Filter DF to keep only records from 'Poland' and kingdom 'Animalia'
occurencePL <- occurence %>% filter(countryCode=="PL" & kingdom=="Animalia") %>%
        arrange(scientificName) %>% group_by(scientificName) %>%
        mutate(totalCount = sum(individualCount)) %>%
        mutate(month = format(eventDate, "%m"), year = format(eventDate, "%Y"))

# Import bird taxonomy data
birdTaxonomy <- fread("biodiversity-data/eBird_Taxonomy_v2021.csv", encoding = "UTF-8")
#View(birdTaxonomy)

# Recover family name using regex - upper case
birdTaxonomy$FAMILY <- toupper(gsub(" \\(.*","",birdTaxonomy$FAMILY))

# Remove '_' character from 'family' column in 'occurencePL'
occurencePL$family <- gsub("_"," ",occurencePL$family)

# Select only last word from 'family' colmun in 'occurencePL'
library(stringr)
occurencePL$family <- toupper(word(occurencePL$family,-1))

# Subset data that contains only birds
occurencePL <- subset(occurencePL, family %in% birdTaxonomy$FAMILY)

# Import MULTIMEDIA FILE
multimedia <- fread("biodiversity-data/multimedia.csv",encoding='UTF-8')

# Select only desired columns
multimedia <- multimedia %>% select(CoreId, accessURI, creator)

# Left join 'occurence' and 'multimedia' by 'id'='CoreId'
occurencePL <- left_join(occurencePL, multimedia, by=c("id"="CoreId"))

# Remove V1 columns
occurencePL <- select(occurencePL, -V1)

# Image scrapper 
occurencePL <- imageScrapper(occurencePL)
#source("functions/image_scrapper.R")

# Export data
write.csv(occurencePL,"biodiversity-data/occurencePL.csv")

