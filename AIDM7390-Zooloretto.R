install.packages('rtweet')
library('rtweet')


app <- 'Zooloretto'
consumer_key = 'BpRSq1TOabc2RsBOlRV3HQraB'
consumer_secret= 'NdSZNKMgcSS23lvXV4zHPWaqmpO4DptBLh777bK1c7wbVTDPul'
access_token = '1327201654018490368-IuKsF3YAZmMCInO87m5iqVdu4MnjdM'
access_secret_token = '7hA7xB6N49fcBmv8ux2ui0aQQiLQZteikrjZoS1PWVfkt'

token <- create_token(app = app,
                      consumer_key = consumer_key,
                      consumer_secret = consumer_secret,
                      access_token = access_token,
                      access_secret = access_secret_token,
                      set_renv = TRUE)

Code<- search_tweets('#100DaysOfCode',n=10000,include_rts = FALSE,lang='en')
Code

#####################################################
# Information of official account of #100DaysOfCode #
#####################################################

daysoc <- lookup_users('_100DaysOfCode')
daysoc$name
daysoc$description
daysoc$followers_count

################
# search tweet #
################

data.frame(Code)
colnames(Code)
Code[100,]$text
Code[100,]$screen_name
Code[100,]$created_at
Code[100,]$retweet_count

#######################
# Twitter rate limits #
#######################

LCode <- search_tweets('#100DaysOfCode', n = 1000000, retryonratelimit = TRUE)
LCode

#################################################################
# Frequency of #100DaysOfCode Twitter statuses from past 4 days #
#################################################################

install.packages('ggplot2')
library('ggplot2')

ts_plot(Code,'3 hours')+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title = ggplot2::element_text(face = 'bold'))+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #100DaysOfCode Twitter statuses from past 4 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#########################
# Get 5 users timelines #
#########################

Codeuser <- get_timelines(c("_100DaysOfCode","ka11away","amanhimself","ossia","freeCodeCamp"),n = 3200)

Codeuser %>%
  dplyr::filter(created_at > "2020-11-1") %>%
  dplyr::group_by(screen_name)%>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point()+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face='bold'))+
  ggplot2::labs(
    x=NULL, y=NULL,
    title="Frequency of Twitter statuses posted by 100DaysOfCode and following",
    subtitle="Twitters status (tweet) count aggregated by day from Nov 2020",
    caption="\nSource: Data collected fron Twitter's REST API via rtweet"
  )

##############################  
# Text mining and word cloud #
##############################

install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')
install.packages('RColorBrewer')

library('tm')
library('SnowballC')
library('wordcloud')
library('RColorBrewer')

Codecode.v <- VectorSource(Code$text)
Codecode.c <- SimpleCorpus(Codecode.v)

inspect(Codecode.c)

Codecode.c.p <- tm_map(Codecode.c, content_transformer(tolower))
Codecode.c.p <- tm_map(Codecode.c.p, removeNumbers)
Codecode.c.p <- tm_map(Codecode.c.p, removeWords, stopwords('english'))
Codecode.c.p <- tm_map(Codecode.c.p,removeWords,c("day"))
Codecode.c.p <- tm_map(Codecode.c.p, removePunctuation)
Codecode.c.p <- tm_map(Codecode.c.p, stripWhitespace)

inspect(Codecode.c.p)

dtm <-TermDocumentMatrix(Codecode.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d,10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE,random.color = TRUE, rot.per = 0.25,
          colors = brewer.pal(13,"Paired"))

################
# word cloud 2 #
################

install.packages("wordcloud2")
library("wordcloud2")
wordcloud2(d,size = 1, color = "random-light",shape = "triangle-forward")

#########
# count #
#########

top10 <- head(d, 10)
top10
barplot(freq ~ word, data = top10, width =2,border = NA,las=2,main = "Top 10 most frequent words",cex.main=1,col = terrain.colors(10))

###########
# network #
###########

install.packages("topicmodels")
install.packages("lubridate")
install.packages("SentimentAnalysis")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("quanteda")
install.packages('textdata')
library("topicmodels")
library("lubridate")
library("SentimentAnalysis")
library("ggpubr")
library("dplyr")
library("tidytext")
library("quanteda")
library('textdata')

text_code <- Codecode.c.p
text_df <- data.frame(text_clean = get("content",text_code),stringsAsFactors = FALSE)
Code$text <- text_df$text_clean
toks <- tokens(Code$text)

set.seed(30)
fcmat <- fcm(toks, context = "document", tri = FALSE)
feat <- names(topfeatures(fcmat, 30))
fcm_select(fcmat, pattern = feat) %>%
textplot_network(min_freq = 0.5)

######################
# Sentiment analysis #
######################

install.packages('ROAuth')
install.packages('syuzhet')
install.packages('RSentiment')
install.packages('kableExtra')
install.packages('knitr')
install.packages('RColorBrewer')
library('ROAuth')
library('syuzhet')
library('RSentiment')
library('kableExtra')
library('knitr')
library('RColorBrewer')

mysentiment_code <- get_nrc_sentiment((Code$text))
Sentimentscores_code <- data.frame(colSums(mysentiment_code[,]))
names(Sentimentscores_code) <- 'Score'
Sentimentscores_code <- cbind('sentiment'=rownames(Sentimentscores_code),Sentimentscores_code)
rownames(Sentimentscores_code) <- NULL

ggplot(data = Sentimentscores_code,aes(x=sentiment,y=Score)) + geom_bar(aes(fill=sentiment),stat = 'identity',width=0.7) +
  theme(legend.position = 'right')+
  xlab('Sentiments')+ylab('Socres')+ggtitle('Sentiments of people behind the tweets on #100DaysofCode')

############################
# Topic modelling with LDA #
############################

set.seed(100)
samp <- sample(nrow(Code),3500)

corpus_sub <- Corpus(VectorSource(Code$text[samp]))
dtm_sub = DocumentTermMatrix(corpus_sub)
doc.length = apply(dtm_sub, 1, sum)
dtm_sub = dtm_sub[doc.length > 0,]

k <- 3
DO <- LDA(dtm_sub, k, method = "Gibbs", control = list(nstart = 5, seed = list(2003,5,63,100001,765), best = TRUE, burnin = 4000, iter = 2000, thin = 400))

topics <- as.matrix(topics(DO))
terms <- as.matrix(terms(DO, 10))
topics_prob <- as.matrix(DO@gamma)

topics_beta <- tidy(DO, matrix = "beta")

top_terms_b <- topics_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

theme_set(theme_classic())
top_terms_b %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + labs(x = 'words', y = NULL) + geom_col(show.legend = TRUE) + facet_wrap(~ topic, scales = "free") + theme(axis.text = element_text(angle = 30, vjust = 0.5, size = 8)) + coord_flip()

#######
# Map #
#######

install.packages('ggmap')
install.packages('maps')
install.packages('mapdata')
install.packages('igraph')
install.packages('gganimate')
install.packages('ggraph')
install.packages('ggalt')
install.packages('ggthemes')
library('ggmap')
library('maps')
library('mapdata')
library('igraph')
library('gganimate')
library('ggraph')
library('ggalt')
library('ggthemes')

CodeLoc <- rtweet::lat_lng(Code)
CodeLoc %>% names() %>% tail(2)
CodeLoc %>% dplyr::distinct(lng) %>% base::nrow()
CodeLoc %>% dplyr::distinct(lat) %>% base::nrow()
CodeLoc <- CodeLoc %>% dplyr::rename(long = lng)

World <- ggplot2::map_data('world')
World %>% glimpse(1000)

ggWorldMap <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = World,
                        aes(x = long,
                            y = lat,
                            group = group),
                        fill = "grey82",
                        color = "white",
                        alpha = 0.6)

gg_Code_title <- "#100DaysOfCode tweets worldwide"
gg_Code_cap <- "Tweets collected with tweet the hashtags #100DaysOfCode"

gg_Code_map <- ggWorldMap +
  ggplot2::coord_quickmap() +
  ggplot2::geom_point(data = CodeLoc,
                      aes(x = long, y = lat),
                      size = 0.7, # reduce size of points
                      color = "firebrick") + ggplot2::labs(title = gg_Code_title,caption = gg_Code_cap)

gg_Code_map
