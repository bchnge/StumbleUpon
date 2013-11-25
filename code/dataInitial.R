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
dfTest2 <- dfTest2[keeps]

x <- dfTrain3[,4:99]
y <- dfTrain2[,40]

require(randomForest)
imputed <- rfImpute(x,y)

X <- imputed[,2:97]
Y <- as.factor(imputed[,1])

Xtest <- dfTest2[, 4:102]
Xtest <- Xtest[, -37]
Xtest <- Xtest[, -37]
Xtest <- Xtest[, -37]

rf <- randomForest(X, Y, ntree=500)
rfPred <- predict(rf, newdata= dfTest2, type = 'prob')

require(glmnet)
glm <- glmnet(as.matrix(X),Y,family = 'binomial')
glmPred <- predict(glm, newx = as.matrix(Xtest), type = 'response')
glmAvg <- rowMeans(glmPred)

result <- cbind(rfPred[,2], glmAvg)

finalResult <- rowMeans(result)

#require(kernlab)
#svm <- ksvm(as.matrix(X),Y,type="C-svc",C = 100,scaled=c(), prob.model = TRUE)
#svmPred <- predict(svm, Xtest, type = 'prob')
DF <- cbind(Y,X)
require(gbm)
gbm <- gbm(formula=Y~.,data = DF, n.trees=500, interaction.depth=1, shrinkage = 0.01, n.cores=2, cv.folds = 5)

best.iter <- gbm.perf(gbm,method="cv")

gbmPred <- predict(gbm, dfTest2, type = 'response')

result <- cbind(rfPred[,2], glmAvg, svmPred[,2], rf2Pred[,2], glm2Avg)

finalResult <- rowMeans(result)

dfTest2$label <- finalResult
dfSubmission <- dfTest2[, grep('urlid|label',names(dfTest2))]

write.table(dfSubmission,file="submissions/submission_16.csv",
            sep=",",row.names=F)         
