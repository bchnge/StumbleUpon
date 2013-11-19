setwd('~/Projects/StumbleUpon/')

require(ggplot2)
require(plyr)

dfTrain <- read.table(file = 'data/train.tsv', sep = '\t', header=T,stringsAsFactors=F)

require(tm)
require(Snowball)
require(wordcloud)

### WORD CLOUDS

corpus2dtm <- function(df){
  require(tm)
  require(Snowball)
  corpus <- Corpus(VectorSource(dfTrain$boilerplate))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  stopWords <- c(stopwords('SMART'),'title','url')
  corpus <- tm_map(corpus, removeWords, stopWords)
  corpus <- tm_map(corpus, stemDocument)  
  dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 1))
}

cloudComparer<- function(topic){
  dtm_eg <- corpus2dtm(dfTrain[dfTrain$alchemy_category == topic & dfTrain$label == 1, ])
  dtm_neg <- corpus2dtm(dfTrain[dfTrain$alchemy_category == topic & dfTrain$label == 0, ])
  
  eg = colSums(as.matrix(dtm_eg))  
  neg = colSums(as.matrix(dtm_neg))
  
  dfEG = data.frame(word = names(eg), EG = eg) 
  dfNEG = data.frame(word = names(neg), NEG = neg) 
  merged = merge(dfEG, dfNEG, by = 'word', all.x = T, all.y = T)
  merged[is.na(merged)] <- 1
  merged$Evergreen <- with(merged, EG/NEG)
  merged$Ephem <- with(merged, NEG/EG)  
  merged
}

require(RColorBrewer)
pal <- brewer.pal(9,'Greens')  

totalCloud<- function(){
  
  dtm_eg <- corpus2dtm(dfTrain[dfTrain$label == 1, ])
  dtm_neg <- corpus2dtm(dfTrain[dfTrain$label == 0, ])
  
  eg = colSums(as.matrix(dtm_eg))  
  neg = colSums(as.matrix(dtm_neg))
  
  dfEG = data.frame(word = names(eg), EG = eg) 
  dfNEG = data.frame(word = names(neg), NEG = neg) 
  merged = merge(dfEG, dfNEG, by = 'word', all.x = T, all.y = T)
  merged[is.na(merged)] <- 1
  merged$Evergreen <- with(merged, EG/NEG)
  merged$Ephem <- with(merged, NEG/EG)  
  merged
}

evergreenPlotter <- function(topic, maxWords = 30){
  pal <- brewer.pal(9,'Greens')  
  a <- cloudComparer(topic)
  wordcloud(words = a$word, freq = a$Evergreen, max.words = maxWords, colors = pal)  
  wordcloud(words = a$word, freq = a$Ephem, max.words = maxWords, colors = pal)  
  
  }

ephemeralPlotter <- function(topic, maxWords = 30){
  pal <- revbrewer.pal(9,'Greys')  
  a <- cloudComparer(topic)
  wordcloud(words = a$word, freq = a$Ephem, max.words = maxWords, colors = pal)  
}


pal_total1 <- brewer.pal(9, 'Greys')
pal_total2 <- brewer.pal(9, 'Greens')

total <- totalCloud()

png('total.png',height = 1200, width = 1600)
par(mfrow = c(1,2))
wordcloud(words = total$word, freq = total$Evergreen, max.words = 20, colors = pal_total2)
wordcloud(words = total$word, freq = total$Ephem, max.words = 20, colors = pal_total1)
dev.off()


allPlotter <- function(topic, maxWords = 30){
  pal <- brewer.pal(9,'Greens')  
  pal2 <- brewer.pal(9,'Greys')
  a <- cloudComparer(topic)
  wordcloud(words = a$word, freq = a$Evergreen, max.words = maxWords, colors = pal)  
  wordcloud(words = a$word, freq = a$Ephem, max.words = maxWords, colors = pal2)  
}

png('arts_entertainment_wcloud.png',height = 800, width = 600)
par(mfrow = c(1,2))
allPlotter('arts_entertainment')
dev.off()

png('business_wcloud.png',height = 800, width = 600)
par(mfrow = c(1,2))
allPlotter('business')
dev.off()

png('unlabeled.png',height = 600, width = 600)
par(mfrow = c(1,2))
allPlotter('?')
dev.off()

png('recreation.png',height = 800, width = 600)
par(mfrow = c(1,2))
allPlotter('recreation')
dev.off()





#### summary bars ####
require(plyr)
catSet <- ddply(.data = dfTrain, .variables=c('alchemy_category'), summarize,
                numWebsites = NROW(label),
                avgUrlWords = mean(numwords_in_url),
                Evergreen = 100*mean(label)
                )

require(ggplot2)
require(scales)
require(ggthemes)


ggplot(catSet, aes(x = reorder(alchemy_category, numWebsites), y = numWebsites, fill = Evergreen)) + 
  geom_bar(stat = 'identity') +  scale_fill_gradient2(low='white', high = 'darkgreen') +
  xlab('Alchemy Category') +
  ylab('Number of websites') +
  theme_tufte() +
  theme(axis.text.x = element_text(face = 'bold', size = 15, angle = 45, vjust = 0.5))

ggplot(catSet, aes(x = avgUrlWords, y = numWebsites, fill = Evergreen)) + 
  geom_point(aes(size = numWebsites), size = 10) +  scale_fill_gradient(low='white', high = 'forestgreen') +
  theme_tufte()

png('bubbles.png',height=1280,width=1920, res=100)
qplot(x = alchemy_category, y = avgUrlWords, data = catSet, colour = Evergreen, size = numWebsites) + 
  scale_color_gradient(low = 'white', high = 'darkgreen') + scale_size(range = c(10,40)) +
  theme_tufte()
dev.off()


ggplot(dfTrain, aes(x = "", y = )) + geom_bar(width = 1) +
  coord_polar(theta = 'y')


symbols(x = catSet$avgUrlWords, y = catSet$Evergreen, circles = catSet$numWebsites/10)
  
