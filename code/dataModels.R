
####################################################
setwd('~/Projects/StumbleUpon/')
dfTrain <- read.csv('data/train.tsv',sep='\t',stringsAsFactors=F)
dfTest <- read.csv('data/test.tsv',sep='\t',stringsAsFactors=F)

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

require(tm)
dfTrain$title <- as.character(sapply(dfTrain$boilerplate, FUN = getTitle))
dfTrain$body <- as.character(sapply(dfTrain$boilerplate, FUN = getBody))

dfTrain$body <- stripWhitespace(dfTrain$body)
dfTrain$title <- stripWhitespace(dfTrain$title)

dfTrain$body <- sapply(dfTrain$body, tolower)
dfTrain$title <- sapply(dfTrain$title, tolower)

dfTrain$titleBody <- do.call(paste, c(dfTrain[c('title', 'body')], sep = ""))



dfTest$title <- as.character(sapply(dfTest$boilerplate, FUN = getTitle))
dfTest$body <- as.character(sapply(dfTest$boilerplate, FUN = getBody))

dfTest$body <- stripWhitespace(dfTest$body)
dfTest$title <- stripWhitespace(dfTest$title)

dfTest$body <- sapply(dfTest$body, tolower)
dfTest$title <- sapply(dfTest$title, tolower)

dfTest$titleBody <- do.call(paste, c(dfTest[c('title', 'body')], sep = ""))


containsTEXT <- function(row,TEXT, Count = FALSE){
  # This function allows us to find and optionally count the instances of a certain phrase in a document
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
  # Count the number of characters in a document
  nchar(row)[1]
}

countWords <- function(row){
  # Count the number of words in a document based on a simple space split criterion
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
dfTrain$countSlash <- sapply(dfTrain$url, FUN = containsTEXT, TEXT = '\\/', Count = TRUE)

dfTrain$TnumWords <- sapply(dfTrain$title,  FUN = countWords)
dfTrain$TnumChars <- sapply(dfTrain$title,  FUN = countCharacters)
dfTrain$TcountJob <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTrain$TrelCountJob <- with(dfTrain, countJob/numChars)
dfTrain$TcountComma <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTrain$TrelCountComma <- with(dfTrain, countComma/numChars)
dfTrain$TcountExcl <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTrain$TrelCountExcl <- with(dfTrain, countExcl/numChars)
dfTrain$TcountButter <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTrain$TcountWater <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTrain$TcountCup <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTrain$TcountDegrees <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTrain$TrelCountDegrees <- with(dfTrain, countDegrees/numChars)
dfTrain$TcontainRecipe <-sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTrain$TcontainComments <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTrain$TcontainEmail <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTrain$TcontainContact <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'contact', Count = FALSE)
dfTrain$TcountDate <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTrain$TcountPipes <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTrain$TcountNumbers <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTrain$TcountAt <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = '@', Count = TRUE)
dfTrain$TrelCountNumbers <- with(dfTrain, countNumbers/numChars)
dfTrain$TcountCalories <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'calories', Count = TRUE)
dfTrain$TcountSignIn <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'sign?in', Count = TRUE)
dfTrain$TcountLogIn <- sapply(dfTrain$title,  FUN = containsTEXT, TEXT = 'log?in', Count = TRUE)
dfTrain$TcountSlash <- sapply(dfTrain$url, FUN = containsTEXT, TEXT = '\\/', Count = TRUE)




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
dfTest$countSlash <- sapply(dfTest$url, FUN = containsTEXT, TEXT = '\\/', Count = TRUE)



dfTest$TnumWords <- sapply(dfTest$title,  FUN = countWords)
dfTest$TnumChars <- sapply(dfTest$title,  FUN = countCharacters)
dfTest$TcountJob <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'job', Count = TRUE)
dfTest$TrelCountJob <- with(dfTest, countJob/numChars)
dfTest$TcountComma <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = ',', Count = TRUE)
dfTest$TrelCountComma <- with(dfTest, countComma/numChars)
dfTest$TcountExcl <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = '\\!', Count = TRUE)
dfTest$TrelCountExcl <- with(dfTest, countExcl/numChars)
dfTest$TcountButter <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'butter', Count = TRUE)
dfTest$TcountWater <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'water', Count = TRUE)
dfTest$TcountCup <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'cup', Count = TRUE)
dfTest$TcountDegrees <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'degree', Count = TRUE)
dfTest$TrelCountDegrees <- with(dfTest, countDegrees/numChars)
dfTest$TcontainRecipe <-sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'recipe', Count = FALSE)
dfTest$TcontainComments <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'comment', Count = FALSE)
dfTest$TcontainEmail <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'email', Count = FALSE)
dfTest$TcontainContact <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'contact', Count = FALSE)
dfTest$TcountDate <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = '[0-9]{4}', Count = TRUE)
dfTest$TcountPipes <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = '\\|', Count = TRUE)
dfTest$TcountNumbers <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = '[:digit:]', Count = TRUE)
dfTest$TcountAt <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = '@', Count = TRUE)
dfTest$TrelCountNumbers <- with(dfTest, countNumbers/numChars)
dfTest$TcountCalories <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'calories', Count = TRUE)
dfTest$TcountSignIn <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'sign?in', Count = TRUE)
dfTest$TcountLogIn <- sapply(dfTest$title,  FUN = containsTEXT, TEXT = 'log?in', Count = TRUE)
dfTest$TcountSlash <- sapply(dfTest$url, FUN = containsTEXT, TEXT = '\\/', Count = TRUE)


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
dfTest2 <- dfTest2[keeps]
dfTest2 <- dfTest2[, -40]
dfTest2 <- dfTest2[, -40]
dfTest2 <- dfTest2[, -40]


x <- dfTrain3[,4:126]
y <- dfTrain2[,40]

require(randomForest)
imputed <- rfImpute(x,y)

X <- imputed[,2:124]
Y <- as.factor(imputed[,1])

Xtest <- dfTest2[, 4:126]

rf <- randomForest(X, Y, ntree=500)
rfPred <- predict(rf, newdata= dfTest2, type = 'prob')

#gbmGrid <- expand.grid(.interaction.depth = (1:3), .n.trees = (1:10)*25, .shrinkage = .1)
#set.seed(2)
#bootControl <- trainControl(number = 50)
#rf2 <- train(X, Y, method = "gbm", trControl = bootControl, verbose = FALSE, 
#                bag.fraction = 0.5, tuneGrid = gbmGrid, metric = 'auc')


require(glmnet)
glm <- glmnet(as.matrix(X),Y,family = 'binomial', alpha = 0.5)
glmPred <- predict(glm, newx = as.matrix(Xtest), type = 'response')
glmAvg <- rowMeans(glmPred)

result <- cbind(rfPred[,2], glmAvg)

finalResult <- rowMeans(result)

dfTest2$label <- finalResult
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_19.csv",
            sep=",",row.names=F)         

#require(kernlab)
#svm <- ksvm(as.matrix(X),Y,type="C-svc",C = 100,scaled=c(), prob.model = TRUE)
#svmPred <- predict(svm, Xtest, type = 'prob')
DF <- cbind(Y,X)

load('temp.RData')
require(gbm)
require(caret)
gbmGrid <- expand.grid(.interaction.depth = (1:3), .n.trees = (1:10)*25, .shrinkage = .1)
set.seed(2)
bootControl <- trainControl(number = 50)
gbmFit <- train(X, Y, method = "gbm", trControl = bootControl, verbose = FALSE, 
                bag.fraction = 0.5, tuneGrid = gbmGrid, metric = 'auc')
#gbm <- gbm(formula=Y~.,data = DF, n.trees=500, interaction.depth=1, shrinkage = 0.01)
gbmPred <- predict(gbmFit$finalModel, newdata = dfTest2, n.trees=gbmFit$finalModel$n.trees, type = 'response')

result <- cbind(rfPred[,2], glmAvg, gbmPred)

finalResult <- rowMeans(result)

dfTest2$label <- finalResult
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_18.csv",
            sep=",",row.names=F)         

require(gbm)
gbm1 <- gbm()
dfTest2

parseUrl <- function(url){
  require(tm)
  return((gsub('\\/|\\-|\\.|\\:|http|www|html|com|net|org|edu',' ',url))
}

dfTest2$URL <- sapply(dfTest2$url, FUN = parseUrl)
dfTrain3$URL <- sapply(dfTrain3$url, FUN = parseUrl)
parseUrl(url=dfTest2$url[1])

require(tm)
corpus <- Corpus(VectorSource(dfTrain3$URL))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords(kind='english'))
#corpus <- tm_map(corpus, stemDocument)


dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            weighting = weightTfIdf,
                            minWordLength = 2)
)

findFreqTerms(dtm,lowfreq=100)

require(scrapeR)
url <- 'data/raw_content/4042'
a <- xmlTreeParse(url)
xmlElementsByTagName(a, 'variable')
