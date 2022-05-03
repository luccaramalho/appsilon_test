# Create empty data frame to display timeline from birds observation
birdTimelineTotal <- data.frame(
  year=c(min(occurencePL$year):c(max(occurencePL$year))),
  yearSum=0)

# Filterd by user
userInput <- subset(occurencePL, vernacularName %in% occurencePL$vernacularName[100])


# Create column 'yearSum' with sum of values by year
birdTimelineUser <- userInput %>% select("year","individualCount") %>% group_by(year) %>%
  mutate(yearSum = sum(individualCount)) %>% select(-"individualCount") %>% unique() %>% arrange(year)


timeline <- left_join(birdTimelineTotal, birdTimelineUser, by="year")


for (i in 1:nrow(birdTimelineTotal)) {
  
  if (is.na(timeline$yearSum.y[i])==TRUE) {
    
    timeline$yearSum[i] <- timeline$yearSum.x[i]
    
  } else {
    
    timeline$yearSum[i] <- timeline$yearSum.y[i]
    
  }
  
}

timeline <- select(timeline, year, yearSum)

library(esquisse)
#esquisser(timeline)

library(ggplot2)

ggplotly(ggplot(timeline) +
 aes(x = year, y = yearSum) +
 geom_line(size = 0.5, colour = "#112446") +
 theme_minimal())
