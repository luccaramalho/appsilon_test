library(rvest)
library(tidyverse)

# Check percentage of NA values inside 'accessURI' column
sum(is.na(occurencePL$accessURI)) / length(occurencePL$accessURI)


## STOPPED AT 680
# Image link recovery from scrapping
for (i in 1:nrow(occurencePL)) {
  
  print(i)
  
  occurencePL$accessURI2[i] <- ""
  
  if(is.na(occurencePL$accessURI[i])==TRUE) {

    print("Missing image link. Trying 'occurenceID' html scrapping to recover link.")
    ##############################################################
    
    # Download html from 'occurenceID' values
    observation_html <- read_html(occurencePL$occurrenceID[i])
    
    # Reads observation html and perform regex
    observation_img <- observation_html %>% html_elements("body") %>%
      html_elements("img.app-ratio-box-image") %>% 
      { gsub('<img src="','',.[1]) } %>%
      { gsub("\\?w=200&amp;h=150\" class=\"app-ratio-box-image\" alt=\"photo for observation\">","",.) }
    
    if (length(observation_img)==0) {
      
      print(paste("Image ",i,"not found."))
      
    } else {
      
      print("Image found.")
      
      occurencePL$accessURI[i] <- paste("https://observation.org",observation_img,sep="")
      
      Sys.sleep(time = 1)
      
    }
    ##############################################################
    
    } else {

      print(paste("Valor",i,"já existente"))
    
    }
  
}




# Download html from 'occurenceID' values
observation_html <- read_html(occurencePL$occurrenceID[12])

# Reads observation html and perform regex
observation_img <- observation_html %>% html_elements("body") %>%
  html_elements("img.app-ratio-box-image") %>% 
  { gsub('<img src="','',.[1]) } %>%
  { gsub("\\?w=200&amp;h=150\" class=\"app-ratio-box-image\" alt=\"photo for observation\">","",.) }

