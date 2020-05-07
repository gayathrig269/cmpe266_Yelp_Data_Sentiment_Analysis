#```{r,message=FALSE,warning=FALSE}

library(tidyverse) #  data manipulation and graphs
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library('wordcloud') #  wordcloud
library(tidytext) # tidy implementation of NLP methods
library(DT)       # table format display of data
library(leaflet) # maps
library(igraph) #  graphs
library(ggraph) #  graphs
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(textcat)
library(textdata)
library(shiny) #package for interactive web apps
install.packages("")
library()
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

##Read the data

#```{r,message=FALSE,warning=FALSE}

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

reviews <- read_csv('C:/Users/16692/Desktop/CMPE266-Big Data/yelp_review.csv')
business <- read_csv('C:/Users/16692/Desktop/CMPE266-Big Data/yelp_business.csv')

#Business data

datatable(head(business), 
          style="bootstrap", 
          class="table-condensed", 
          options = list(dom = 'tp',scrollX = TRUE))


#A glimpse of the reviews data

glimpse(reviews)
glimpse(business)

#Detecting the language of the reviews

textcat(reviews[1:10,]$text)


#Most Popular Categories

print("The most popular categories of business are plotted in the bar plot")        


#```{r,message=FALSE,warning=FALSE}

categories = str_split(business$categories,";")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories of Business') +
  coord_flip() + 
  theme_bw()

#Top Ten Cities with the most Business parties mentioned in Yelp

print("We show the Top Ten Cities which has the most Business parties mentioned in Yelp")

#```{r,message=FALSE,warning=FALSE}

business %>%
  group_by(city) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(City = reorder(city,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = City,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = City, y = 1, label = paste0("(",round(Count/1e3)," K )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'City', y = 'Count of Reviews', 
       title = 'Top Ten Cities with the most Business parties in Yelp') +
  coord_flip() + 
  theme_bw()
############################################

#Map of the business parties in Las vegas
LasvegasCoords = business %>% filter(city == "Las Vegas")

center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)

leaflet(LasvegasCoords) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
  
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 13)

#Business with most Five Star Reviews from Users

most5StarsReviews = reviews %>%
  filter(stars == 5) %>%
  group_by(business_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(BusinessID = reorder(business_id,Count)) %>%
  head(10)

most5StarsReviews = inner_join(most5StarsReviews,business)

most5StarsReviews %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Business', 
       y = 'Count', 
       title = 'Name of the Business and Count') +
  coord_flip() +
  theme_bw()

#From Wikipedia

print("The Las Vegas Strip is a stretch 
      of South Las Vegas Boulevard in Clark County, Nevada that is known for its concentration of resort hotels and casinos. The Strip is approximately 4.2 miles (6.8 km) in length,[1] located immediately south of the Las Vegas city limits in the unincorporated towns of Paradise and Winchester.")


#```{r,message=FALSE,warning=FALSE}

LasvegasCoords = business %>% filter(city == "Las Vegas")

center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)

leaflet(LasvegasCoords) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
  
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 13)


#######################

#**"Mon Ami Gabi"**

print("The location and category of the most liked business **Mon Ami Gabi** is shown below")


mon_ami_gabi = business %>% filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(mon_ami_gabi), 
          style="bootstrap", 
          class="table-condensed", 
          options = list(dom = 'tp',
                         scrollX = TRUE))

##############################

##Useful,funny,cool reviews

#The following plot describes the number of 
#**Useful, Funny and Cool** reviews.
#Most of the reviews are NOT useful , funny or cool.      
mon_ami_gabi_reviews = reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw")

mon_ami_gabi_reviews %>%
  group_by(useful) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(useful = reorder(useful,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = useful,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = useful, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Useful Reviews', 
       y = 'Count', 
       title = 'Useful Reviews and Count') +
  coord_flip() +
  theme_bw()

########################
mon_ami_gabi_reviews %>%
  group_by(funny) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(funny = reorder(funny,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = funny,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = funny, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Funny Reviews', 
       y = 'Count', 
       title = 'Funny Reviews and Count') +
  coord_flip() +
  theme_bw()

#########################

mon_ami_gabi_reviews %>%
  group_by(cool) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(cool = reorder(cool,Count)) %>%
  head(10) %>%

  ggplot(aes(x = cool,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = cool, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Cool Reviews', 
       y = 'Count', 
       title = 'Cool Reviews and Count') +
  coord_flip() +
  theme_bw()


################################
createWordCloud = function(train)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(reviews %>%
                  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))

######################################
mon_ami_gabi_reviews %>%
  group_by(cool) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(cool = reorder(cool,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = cool,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = cool, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Cool Reviews', 
       y = 'Count', 
       title = 'Cool Reviews and Count') +
  coord_flip() +
  theme_bw()


###########################

createWordCloud = function(train)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(3, "RdBu")))
}

createWordCloud(reviews %>%
                  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))

#########################################
reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

###########################33
positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(reviews %>%
                        filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))


############################
######5/5/2020#############
#Bacchanal Buffet#
bacchanal = business %>% filter(business_id == "RESDUcs7fIiihp38-d6_6g") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(bacchanal), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


#######WORD_COUNT#############
bacchanal = reviews %>% filter(business_id == "RESDUcs7fIiihp38-d6_6g")

createWordCloud(bacchanal)

################
#############SENTIMENT_ANALYSIS################
positiveWordsBarGraph(bacchanal)

#############CALCULATION_OF_SENTIMENTS_FOR_REVIEWS##########

sentiment_lines = calculate_sentiment(bacchanal)

head(sentiment_lines)

##############
#######DISPLAY_NEGATIVE_REVIEWS#########
display_neg_sentiments(sentiment_lines,bacchanal)

########DISPLAY_POSITIVE_REVIEWS########################
display_pos_sentiments(sentiment_lines,bacchanal)

########RELATIONSHIP_AMONG_WORDS###############
bigrams_bacchanal <- bacchanal %>%
  count_bigrams()

bigrams_bacchanal %>%
  filter(n > 100) %>%
  visualize_bigrams()

##########BIGRAMS
bigramsMonAmiGabi %>%
  filter(word1 == "crab" | word2 == "crab" ) %>%
  visualize_bigrams()

############

bacchanal %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()


####################
#TOP_TEN_BuSINESS_IN_TORONTO
##################
toronto_biz = business %>%
  filter(city == "Toronto") %>%
  arrange(desc(review_count,stars)) %>%
  select(name,neighborhood,address,review_count,stars) %>%
  head(10)

datatable(toronto_biz, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#################
createWordCloud(reviews %>%
                  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg"))
##################

#Ten most common words used in reviews of business Pai Northern Thai Kitchen
reviews %>%
  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

####################
positiveWordsBarGraph(reviews %>%
                        filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg"))

###############
# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}
#############
create_LDA_topics <- function(business_text,custom_stop_words)
{
  # create a document term matrix to clean
  reviewsCorpus <- Corpus(VectorSource(business_text$text)) 
  reviewsDTM <- DocumentTermMatrix(reviewsCorpus)
  
  # convert the document term matrix to a tidytext corpus
  reviewsDTM_tidy <- tidy(reviewsDTM)
  
  # remove stopwords
  reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
    anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
    anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords
  
  top_terms_by_topic_LDA(reviewsDTM_tidy_cleaned$term, number_of_topics = 4)
  
}

monamigabi = reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw")

custom_stop_words <- tibble(word = c("mon","ami","gabi","restaurant","food","vegas"))

create_LDA_topics(monamigabi,custom_stop_words)
##################
custom_stop_words <- tibble(word = c("restaurant","food"))

create_LDA_topics(bacchanal,custom_stop_words)

#####PHOENIX_CITY_ANALYSIS
city_biz = business %>%
  filter(city == "Phoenix") %>%
  arrange(desc(review_count,stars)) %>%
  select(name,neighborhood,address,review_count,stars) %>%
  head(10)

datatable(city_biz, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#######################TOPIC_MODELING_RESTAURANT_TEN_THOUSANDS_WORDS##########
CityCoords = business %>%
  filter(city == "Phoenix")

city_words = inner_join(CityCoords,reviews) %>% select(date,text,review_id) %>% sample_n(10000)

custom_stop_words <- tibble(word = c("restaurant","food"))

create_LDA_topics(city_words,custom_stop_words)
###################
createWordCloud(city_words)

###################
city_words %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()
#############
positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(value))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    labs(title = 'Positive and Negative') +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(city_words)

####################
calculate_sentiment <- function(review_text)
{
  sentiment_lines  =  review_text %>%
    filter(textcat(text) == "english") %>%  # considering only English text
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(review_id) %>%
    summarize(sentiment = round(mean(value),2),words = n()) %>%
    ungroup() %>%
    filter(words >= 5) 
  
  return(sentiment_lines)
  
}

sentiment_lines = calculate_sentiment(city_words)

head(sentiment_lines)

###################
