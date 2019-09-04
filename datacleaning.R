##Data Processing

library(tidyverse)
library(tidytext)
library(sentimentr)
library(gender)
library(leaflet)



data <- read_csv("AB_NYC_2019.csv")
babynames <- read_csv("NationalNames.csv")


#NA's for reviews_per_month, last_review. Convert to zero
summary(data)
data$reviews_per_month[is.na(data$reviews_per_month)] <- 0




##Text Analysis
names <- as.data.frame(data$name)
colnames(names) <- 'text'
names$text <- as.character(names$text)

singleword_frequency <- names %>% unnest_tokens(word,text) %>% anti_join(stop_words) 
singleword_frequency <- singleword_frequency %>% count(word,sort = TRUE) 


data <- data %>% mutate(
  cozy = ifelse(str_detect(name,'cozy')|str_detect(name,'Cozy'),1,0),
  private = ifelse(str_detect(name,'private')|str_detect(name,'Private'),1,0),
  spacious = ifelse(str_detect(name,'spacious')|str_detect(name,'Spacious'),1,0),
  park = ifelse(str_detect(name,'park')|str_detect(name,'Park'),1,0),
  beautiful = ifelse(str_detect(name,'beautiful')|str_detect(name,'Beautiful'),1,0),
  home = ifelse(str_detect(name,'home')|str_detect(name,'Home'),1,0),
  modern = ifelse(str_detect(name,'modern')|str_detect(name,'Modern'),1,0)
)


##Sentiment
sentiment <- sentiment(data$name)
sentiment <- sentiment %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment), word_count = sum(word_count))
data$sentiment <- sentiment$sentiment
data$word_count <- sentiment$word_count


##Gender #### CLEAN HOST NAMES FOR FIRST NAME IDENTIFICATION
males <- babynames %>% filter(Gender == 'M')
males <- males$Name
females <- babynames %>% filter(Gender == 'F')
females <- females$Name



#saveRDS(data,"data.RDS")


#######Neighborhood Maps
neighborhood <- data %>% group_by(neighbourhood) %>% summarise(
  lat = mean(latitude),
  long = mean(longitude),
  price = mean(price),
  reviews = mean(number_of_reviews),
  total = n(),
  host_listings = mean(calculated_host_listings_count),
  private_rooms = length(which(room_type == "Private room"))/n()
)


#saveRDS(neighborhood,file = "neighborhood.rds")
