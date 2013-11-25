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

doc.matrix <- as.data.frame(matrix(docs))
doc_test.matrix <- as.data.frame(matrix(docs_test))


############ Start Here #################

dfTrain <- read.csv('data/train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('data/test.tsv', sep = '\t',stringsAsFactors=F)

getTitle <- function(row){
  require(rjson)
  return(fromJSON(row)$title)
}

getBody <- function(row){
  require(rjson)
  return(fromJSON(row)$body)
}

getUrl <- function(row){
  require(rjson)
  return(fromJSON(row)$url)
}

# Extract body text from training data
dfTrain$title <- as.character(sapply(dfTrain$boilerplate, FUN = getTitle))
dfTrain$body <- as.character(sapply(dfTrain$boilerplate, FUN = getBody))

dfTest$title <- as.character(sapply(dfTest$boilerplate, FUN = getTitle))
dfTest$body <- as.character(sapply(dfTest$boilerplate, FUN = getBody))

require(tm)
dfTrain$body <- stripWhitespace(dfTrain$body)
dfTrain$title <- stripWhitespace(dfTrain$title)

dfTrain$body <- sapply(dfTrain$body, tolower)
dfTrain$title <- sapply(dfTrain$title, tolower)

dfTest$body <- stripWhitespace(dfTest$body)
dfTest$title <- stripWhitespace(dfTest$title)

dfTest$body <- sapply(dfTest$body, tolower)
dfTest$title <- sapply(dfTest$title, tolower)

#docs <- apply(as.matrix(dfTrain$urlid),1,getWebText)

#docs_test <- apply(as.matrix(dfTest$urlid),1,getWebText)

#save(docs, file='trainingWebText.RData')


#load('trainingWebText.RData')


getAvgSentLength <- function(doc){
  require(openNLP)
  require(openNLPmodels.en)
  require(NLP)
  if(doc!=' ' & doc!= ''){
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    d <- as.data.frame(annotate(doc, list(sent_token_annotator, word_token_annotator)))
    sentences <- d[d$type == 'sentence',]
    lengthSentences <- as.numeric(sentences$end) - as.numeric(sentences$start) + 1 
    return(mean(lengthSentences))    
  }
  else{
    return(0)
  }
}


dfTrain$avgSentLength <- sapply(dfTrain$body, FUN = getAvgSentLength)  



### Other features
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

countWords <- function(row){
  length(unlist(strsplit(row, ' ')))
}


dfTrain$numWords <- sapply(dfTrain$body,  FUN = countWords)
dfTrain$numChars <- sapply(dfTrain$body,  FUN = countCharacters)
dfTrain$countJob <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTrain$relCountJob <- with(dfTrain, countJob/numChars)
dfTrain$countComma <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTrain$relCountComma <- with(dfTrain, countComma/numChars)
dfTrain$countExcl <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTrain$relCountExcl <- with(dfTrain, countExcl/numChars)
dfTrain$countButter <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTrain$countWater <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTrain$countCup <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTrain$countDegrees <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTrain$relCountDegrees <- with(dfTrain, countDegrees/numChars)
dfTrain$containRecipe <-sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTrain$containComments <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTrain$containEmail <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTrain$containContact <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'contact', Count = FALSE)
dfTrain$countDate <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTrain$countPipes <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTrain$countNumbers <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTrain$countAt <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = '@', Count = TRUE)
dfTrain$relCountNumbers <- with(dfTrain, countNumbers/numChars)
dfTrain$countCalories <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'calories', Count = TRUE)
dfTrain$countSignIn <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'sign?in', Count = TRUE)
dfTrain$countLogIn <- sapply(dfTrain$body,  FUN = containsTEXT, TEXT = 'log?in', Count = TRUE)

dfTest$numWords <- sapply(dfTest$body,  FUN = countWords)
dfTest$numChars <- sapply(dfTest$body,  FUN = countCharacters)
dfTest$countJob <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTest$relCountJob <- with(dfTest, countJob/numChars)
dfTest$countComma <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTest$relCountComma <- with(dfTest, countComma/numChars)
dfTest$countExcl <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTest$relCountExcl <- with(dfTest, countExcl/numChars)
dfTest$countButter <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTest$countWater <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTest$countCup <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTest$countDegrees <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTest$relCountDegrees <- with(dfTest, countDegrees/numChars)
dfTest$containRecipe <-sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTest$containComments <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTest$containEmail <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTest$containContact <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'contact', Count = FALSE)
dfTest$countDate <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTest$countPipes <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTest$countNumbers <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTest$countAt <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = '@', Count = TRUE)
dfTest$relCountNumbers <- with(dfTest, countNumbers/numChars)
dfTest$countCalories <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'calories', Count = TRUE)
dfTest$countSignIn <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'sign?in', Count = TRUE)
dfTest$countLogIn <- sapply(dfTest$body,  FUN = containsTEXT, TEXT = 'log?in', Count = TRUE)




require(dummies)
# Making alchemy category comparable across datasets
dfTrain$alchemy_category[dfTrain$alchemy_category == 'unknown' | dfTrain$alchemy_category == 'weather'] <- '?'

dfTrain2 <- dummy.data.frame(dfTrain,names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTest2 <- dummy.data.frame(dfTest, names = c('alchemy_category', 'is_news', 'news_front_page'))
dfTrain2$label <- factor(dfTrain2$label)
dfTrain2 <- dfTrain2[, names(dfTrain2) != 'alchemy_category_score']
dfTest2 <- dfTest2[, names(dfTest2) != 'alchemy_category_score']


# Extract domain information from URL
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
dfTrain3 <- dfTrain3[, -40]
dfTrain3 <- dfTrain3[, -40]
dfTrain3 <- dfTrain3[, -40]


is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dfTrain3[is.nan(dfTrain3)] <- 0
dfTest2[is.nan(dfTest2)] <- 0

require(randomForest)
rf <- randomForest(dfTrain3[,4:99], dfTrain2[,40])
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest2$label <- a[,2]
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_10.csv",
            sep=",",row.names=F)         



################


