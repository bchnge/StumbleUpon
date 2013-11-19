setwd('~/Projects/StumbleUpon/')

dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=FALSE)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=FALSE)

require(Boruta)


logit_1 <- glm(label ~ html_ratio + image_ratio + embed_ratio + is_news, data = dfTrain, family = "binomial")

summary(logit_1)
require(coefplot)
coefplot(logit_1)

dfTest$label <- predict(logit_1, newdata = dfTest, type = "response")
dfSubmission <- dfTest[, grep('urlid|label',names(dfTest))]

write.table(dfSubmission,file="submissions/submission_1.csv",
            sep=",",row.names=F)         


#############################################################

dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=F)

require(dummies)

dfTrain$alchemy_category[dfTrain$alchemy_category == 'unknown' | dfTrain$alchemy_category == 'weather'] <- '?'


dfTrain2 <- dummy.data.frame(dfTrain,names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTest2 <- dummy.data.frame(dfTest, names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTrain2$label <- factor(dfTrain2$label)
dfTrain2 <- dfTrain2[, names(dfTrain2) != 'alchemy_category_score']
dfTest2 <- dfTest2[, names(dfTest2) != 'alchemy_category_score']


rf <- randomForest(dfTrain2[,4:39], dfTrain2[,40])
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest$label <- a[,2]
dfSubmission <- dfTest[, grep('urlid|label',names(dfTest))]

write.table(dfSubmission,file="submissions/submission_3.csv",
            sep=",",row.names=F)         

#################################################################################


dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=F)

require(dummies)

dfTrain$alchemy_category[dfTrain$alchemy_category == 'unknown' | dfTrain$alchemy_category == 'weather'] <- '?'


dfTrain2 <- dummy.data.frame(dfTrain,names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTest2 <- dummy.data.frame(dfTest, names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTrain2$label <- factor(dfTrain2$label)
dfTrain2 <- dfTrain2[, names(dfTrain2) != 'alchemy_category_score']
dfTest2 <- dfTest2[, names(dfTest2) != 'alchemy_category_score']

require(Boruta)


boruta <- Boruta(dfTrain2[,4:39], dfTrain2[,40])

# drop list from Boruta run
dropList <- c('alchemy_categorygaming','alchemy_categorylaw_crime','alchemy_categoryreligion','alchemy_categoryscience_technology','framebased')

x <- dfTrain2[,4:39]
x <- x[,!(names(x) %in% dropList)]
y <- dfTrain2[,40]


urls <- as.matrix(dfTrain$url)

domainUrl <- function(x){
  require(httr)  
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)]  
}

nameUrl <- function(x){
  require(httr)
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)-1]  
}


b <- apply(X = urls, FUN= domainUrl,1)
c <- apply(X = urls, FUN = nameUrl, 1)


#####################################

dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=F)

require(dummies)

dfTrain$alchemy_category[dfTrain$alchemy_category == 'unknown' | dfTrain$alchemy_category == 'weather'] <- '?'


dfTrain2 <- dummy.data.frame(dfTrain,names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTest2 <- dummy.data.frame(dfTest, names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTrain2$label <- factor(dfTrain2$label)
dfTrain2 <- dfTrain2[, names(dfTrain2) != 'alchemy_category_score']
dfTest2 <- dfTest2[, names(dfTest2) != 'alchemy_category_score']



urls <- as.matrix(dfTrain$url)

domainUrl <- function(x){
  require(httr)  
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)]  
}

nameUrl <- function(x){
  require(httr)
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)-1]  
}

path1Url <- function(x){
  require(httr)
  temp <- parse_url(x)
  path <- temp$path
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)-1]  
}

domain <- apply(X = as.matrix(dfTrain2$url), FUN= domainUrl,1)
dfTrain2 <- cbind(dfTrain2, domain)
dfTrain2 <- dummy.data.frame(dfTrain2,names = c('domain'))
domain <- apply(X = as.matrix(dfTest2$url), FUN= domainUrl,1)
dfTest2 <- cbind(dfTest2, domain)
dfTest2 <- dummy.data.frame(dfTest2, names = c('domain'))


a <- names(dfTest2)
b <- names(dfTrain2)
c <- intersect(a,b)

keeps <- c
dfTrain3 <- dfTrain2[keeps]

require(randomForest)

rf <- randomForest(dfTrain3[,4:74], dfTrain2[,40])
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest2$label <- a[,2]
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_5.csv",
            sep=",",row.names=F)         











dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=F)

require(dummies)

dfTrain$alchemy_category[dfTrain$alchemy_category == 'unknown' | dfTrain$alchemy_category == 'weather'] <- '?'


dfTrain2 <- dummy.data.frame(dfTrain,names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTest2 <- dummy.data.frame(dfTest, names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTrain2$label <- factor(dfTrain2$label)
dfTrain2 <- dfTrain2[, names(dfTrain2) != 'alchemy_category_score']
dfTest2 <- dfTest2[, names(dfTest2) != 'alchemy_category_score']



urls <- as.matrix(dfTrain$url)

domainUrl <- function(x){
  require(httr)  
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)]  
}

nameUrl <- function(x){
  require(httr)
  temp <- parse_url(x)
  dname <- temp$hostname
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)-1]  
}


path1Url <- function(x){
  require(httr)
  temp <- parse_url(x)
  path <- temp$path
  stripped <- unlist(strsplit(dname, '\\.'))
  domain <- stripped[length(stripped)-1]  
}

domain <- apply(X = as.matrix(dfTrain2$url), FUN= domainUrl,1)
webname <- apply(X = as.matrix(dfTrain2$url), FUN= nameUrl,1)

dfTrain2 <- cbind(dfTrain2, domain)
dfTrain2 <- cbind(dfTrain2, webname)
dfTrain2 <- dummy.data.frame(dfTrain2,names = c('domain'))
dfTrain2 <- dummy.data.frame(dfTrain2,names = c('webname'))


domain <- apply(X = as.matrix(dfTest2$url), FUN= domainUrl,1)
webname <- apply(X = as.matrix(dfTest2$url), FUN = nameUrl, 1)
dfTest2 <- cbind(dfTest2, domain)
dfTest2 <- cbind(dfTest2, webname)
dfTest2 <- dummy.data.frame(dfTest2, names = c('domain'))
dfTest2 <- dummy.data.frame(dfTest2, names = c('webname'))

a <- names(dfTest2)
b <- names(dfTrain2)
c <- intersect(a,b)

keeps <- c
dfTrain3 <- dfTrain2[keeps]

require(Boruta)

boruta <- Boruta(dfTrain3[,4:842], dfTrain2[,40],doTrace=T)

require(randomForest)

registerDoMC(cores = 2)
require(caret)

x <- dfTrain3[,4:842]
y <- dfTrain2[,40]

model <- train(x,y, method = 'rf')

test <- rfe(x, y, metric = 'Accuracy',
            sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
            rfeControl = rfeControl(functions = lmFuncs, rerank = T, number = 200)
            )

require(FSelector)

data <- cbind(y,x)

a <- gsub('-','',names(data))
a <- gsub('\\?','q', a)
names(data) <- a
weights <- random.forest.importance(y~., data, importance.type = 1)
print(weights)

b <- cbind(weights, rownames(weights))

bestFeatures <- b[abs(b$attr_importance)>3,]
names(bestFeatures) <- c('attr_importance', 'vname')

c <- intersect(names(data),names(dfTest2))

keeps <- intersect(c, bestFeatures$vname)

x_train <- data[keeps]
x_test <- dfTest2[keeps]

rf <- randomForest(x_train, y)
temp <- predict(rf, newdata = x_test, type = "prob")
dfTest2$label <- temp[,2]

dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_7.csv",
            sep=",",row.names=F)         

temp <- predict(rf, newdata = x_test, type = "class")

dfTest2$label <- temp

dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_8.csv",
            sep=",",row.names=F)         



####################################################

getWebText <- function(urlid){
  # Function to grab html text based on given urlid
  require(XML)
  require(RCurl)
  
  urlid <- gsub(' ','',urlid)
  doc.html<- htmlTreeParse(paste('data/raw_content/',urlid, sep=''), useInternalNodes=T)
  doc.text <- paste(xpathApply(doc.html, '//p', xmlValue), collapse = '\n')  
  doc.text <- gsub('\\n', ' ', doc.text)
  doc.text <- gsub('\\r', ' ', doc.text)
  doc.text <- gsub('\\t', ' ', doc.text)
  doc.text <- paste(doc.text, collapse = ' ')
  
  require(tm)
  doc.text <- stripWhitespace(doc.text)
  doc.text <- iconv(doc.text, 'UTF-8-MAC')
}


cleanDoc <- function(doc){
  # Clean html text from in a document
  require(tm)
  require(SnowballC)
  
  c <- paste(doc, collapse = '')
  d <- gsub('c\\(','',c)
  d <- gsub('\\(','',d)
  d <- gsub('\\)','',d)
  d <- gsub('\\n', '', d)
  d <- gsub('\\r', '', d)
  d <- gsub('\\t', '', d)
  d <- tolower(d)               
  d <- removePunctuation(d)
  d <- removeNumbers(d)
  d <- removeWords(d, stopwords('english'))
}


dfTrain <- read.csv('data/train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('data/test.tsv', sep = '\t',stringsAsFactors=F)

docs <- apply(as.matrix(dfTrain$urlid),1,getWebText)

install.packages("openNLP") ## Installs the required natural language processing (NLP) package
install.packages("openNLPmodels.en") ## Installs the model files for the English language
library(openNLP) ## Loads the package for use in the task
library(openNLPmodels.en) ## Loads the model files for the English language
require(NLP)

wordSentence <- function(doc){
  require(openNLP)
  require(openNLPmodels.en)
  require(NLP)
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  
  d <- annotate(doc, list(sent_token_annotator, word_token_annotator))
  
  containText <- function(doc, text){
    
  }
  
  countText() <- function(doc, text, normalize2char = F){
    
  }
  
  require(plyr)
  sumStat <- ddply(d, .variables = ' ', summarize,
                   numWords = ,
                   numSentences = ,
                   avgWordsInSentence = ,
                   )
  numWordsRelSentences
  containsDate <- grep(
    )
  containsLastUpdated <- grep(
    )
  containsExclamation <- grep(
    )
  numPipe <- grep(
    )
  numPipesRelChar
  containsLogin <- grep(
    )
  numAt <- grep(
    )
  numAtRelChar
  butterRatio <- grep(
    )
  waterRatio <- grep(
    )
  containsRecipe <- grep(
    )
  
  containsIngrediences <- grep(
    )
  
  containsServings <- grep(
    )
  
  containsComments <- grep(
    )
  
  containsUniversity <- grep(
    )
  
  containsFaculty <- grep(
    )
  
  numNumbers <- grep(
    )
  numNumbersRelChar
  
  numShare <- grep(
    )
  
  
  
}



require(tm)

dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)),
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                         stopwords = TRUE,
                                         stripWhitespace = TRUE)
                          )

findAssocs(dtm, 'york', 0.9)
findFreqTerms(dtm, 1000)

dtm2 <- removeSparseTerms(dtm,sparse= 0.9)
