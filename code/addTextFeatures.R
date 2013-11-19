setwd('~/Projects/StumbleUpon/')

setwd('C:/Users/chengb/Documents/GitHub/StumbleUpon')

getWebText <- function(urlid){
  # Function to grab html text based on given urlid
  require(XML)
  
  urlid <- gsub(' ','',urlid)
  doc.html<- htmlTreeParse(paste('data/raw_content/',urlid, sep=''), useInternalNodes=T)
  doc.text <- paste(xpathApply(doc.html, '//p', xmlValue), collapse = '\n')  
  doc.text <- gsub('\\n', ' ', doc.text)
  doc.text <- gsub('\\r', ' ', doc.text)
  doc.text <- gsub('\\t', ' ', doc.text)
  doc.text <- paste(doc.text, collapse = ' ')
  
  require(tm)
  doc.text <- stripWhitespace(doc.text)
  doc.text <- 
#  doc.text <- iconv(doc.text, 'UTF-8-MAC')
  doc.text <- iconv(doc.text, 'UTF-8')
  doc.text <- tolower(doc.text)
  
  
}


dfTrain <- read.csv('data/train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('data/test.tsv', sep = '\t',stringsAsFactors=F)


docs <- apply(as.matrix(dfTrain$urlid),1,getWebText)

docs_test <- apply(as.matrix(dfTest$urlid),1,getWebText)

save(docs, file='trainingWebText.RData')


load('trainingWebText.RData')



containsTEXT <- function(row,TEXT, Count = FALSE){
  require(stringr)
  result <- str_count(row, TEXT)[1]
  if(Count == TRUE){
    answer <- result
  }
  else{
    if(result >0){answer <- 1}
    else{
      answer <- 0}
  }
  answer
}

countCharacters <- function(row){
  nchar(row)[1]
}

doc.matrix <- as.data.frame(matrix(docs))
doc_test.matrix <- as.data.frame(matrix(docs_test))

dfTrain$numChars <- apply(doc.matrix, MARGIN = 1, FUN = countCharacters)
dfTrain$countJob <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTrain$relCountJob <- with(dfTrain, countJob/numChars)
dfTrain$countComma <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTrain$relCountComma <- with(dfTrain, countComma/numChars)
dfTrain$countExcl <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTrain$relCountExcl <- with(dfTrain, countExcl/numChars)
dfTrain$countButter <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTrain$countWater <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTrain$countCup <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTrain$countDegrees <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTrain$relCountDegrees <- with(dfTrain, countDegrees/numChars)
dfTrain$containRecipe <-apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTrain$containComments <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTrain$containEmail <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTrain$containContactUs <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'contact us', Count = FALSE)
dfTrain$countDate <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTrain$countPipes <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTrain$countNumbers <- apply(doc.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTrain$relCountNumbers <- with(dfTrain, countNumbers/numChars)


dfTest$numChars <- apply(doc_test.matrix, MARGIN = 1, FUN = countCharacters)
dfTest$countJob <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTest$relCountJob <- with(dfTest, countJob/numChars)
dfTest$countComma <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTest$relCountComma <- with(dfTest, countComma/numChars)
dfTest$countExcl <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTest$relCountExcl <- with(dfTest, countExcl/numChars)
dfTest$countButter <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTest$countWater <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTest$countCup <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTest$countDegrees <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTest$relCountDegrees <- with(dfTest, countDegrees/numChars)
dfTest$containRecipe <-apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTest$containComments <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTest$containEmail <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTest$containContactUs <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = 'contact us', Count = FALSE)
dfTest$countDate <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTest$countPipes <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTest$countNumbers <- apply(doc_test.matrix, MARGIN = 1, FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTest$relCountNumbers <- with(dfTest, countNumbers/numChars)







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

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dfTrain3[is.nan(dfTrain3)] <- 0
dfTest2[is.nan(dfTest2)] <- 0

require(randomForest)
rf <- randomForest(dfTrain3[,4:94], dfTrain2[,40])
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest2$label <- a[,2]
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_9.csv",
            sep=",",row.names=F)         



################

require(openNLP) ## Loads the package for use in the task
require(openNLPmodels.en) ## Loads the model files for the English language
require(NLP)


wordSentence <- function(doc){
  require(openNLP)
  require(openNLPmodels.en)
  require(NLP)
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  
  d <- annotate(doc, list(sent_token_annotator, word_token_annotator))
  

  
  
