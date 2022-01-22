
###############################################################
######A3: Business Insights Report - Marius Heje Mæhle #######
######Querying Twitter for bitcoins###########################

#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")
#install.packages("NLP")
#install.packages("rtweet")

library(rtweet)
library(twitteR)
library(tm)
library(NLP)

#consumerKey, consumerSecret, access token and access secret from my twitter
consumer_key <- 'osmot3QLGT5ithOP2A3CgHGHh'
consumer_secret <- '2hP8tl70oJHeBLW0AZOaNfbZQj4hc51HeBFfkO310IV6CJleO6'
access_token <- '1204793374638837761-HXz9ePnCBLE93FJFk2mdet0rxQj2wc'
access_secret <- 'DpTIPGkY0H9PGHFVW3GSSrZYkOdC3oV3pBuPNyNVoxVBz'
name_of_app <- "Incerto Coding"

twitter_token <- create_token(
  app = name_of_app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

rt <- search_tweets("#Bitcoin", n = 10000, lang = "en", include_rts = FALSE)
#I receive a Warning messages: 
#1:In class(object) <- "environment" :Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object

#put dataset into a dataframe
library(dplyr)
mydf <- data.frame(line=1:9037, text=rt$text)
print(mydf)

###remove? and only use stop words
token_list <- mydf %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(token_list)

#remove stopwords
library(stringr)

data(stop_words)
frequencies_tokens_stop <- mydf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_stop)

### Sentiment analysis######
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

frequencies_tokens_stop %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

frequencies_tokens_stop %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

afinn <- frequencies_tokens_stop %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  frequencies_tokens_stop%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  frequencies_tokens_stop %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


####### Most common positive and negative words #########

bing_counts <- frequencies_tokens_stop %>%
  inner_join(get_sentiments("bing")) %>%
  arrange(desc(n))

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#################
#library(tidytext)
#library(tidyverse)

######create histogram ### not used
######### piping ####NOT USED #########

#style 3. piping method
#combine boths 1 and 2 - best practice
piped_df <- rt %>%
  filter(carb>2) %>% #takes the two first lines as input
  group_by(cyl) %>% #tells r that you want to create groups
  summarise(avg_mpg = mean(mpg)) %>%
  filter(avg_mpg > 15)


#setup twitter - give twitter autorization
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

USA <- twitteR::searchTwitter('#USA + #Bitcoin', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)
write.csv(d, file='USA')

EU <- twitteR::searchTwitter('#EU + #Bitcoin', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU) 
write.csv(e, file='xxxxx')

Asia <- twitteR::searchTwitter('#Asia + #Bitcoin', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(Asia) 









###### N-grams and tokenizing ###############
library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)

rt_bigrams_new <- rt %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

rt_bigrams_new #We want to see the bigrams (words that appear together, "pairs")

rt_bigrams_new %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- rt_bigrams_new %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#remove stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

###### VISUALISING THE BIGRAM NETWORK ##########
#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>100) %>% #check the size, and edit proport
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                                                     arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "gold", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
