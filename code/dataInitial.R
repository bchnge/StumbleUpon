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

rf <- randomForest(x, y)
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest$label <- a[,2]
dfSubmission <- dfTest[, grep('urlid|label',names(dfTest))]

write.table(dfSubmission,file="submissions/submission_4.csv",
            sep=",",row.names=F)         

#####################################

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(x_num)) # get eigenvalues
ap <- parallel(subject=nrow(x_num),var=ncol(x_num),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
dropList2 <- names(x)[grep('alchemy_*|is_news*|news_front_page*',names(x))]
x_num <- x[,!(names(x) %in% dropList2)]

fit <- factanal(x_num, 6, rotation="varimax", scores = 'regression')
x2 <- cbind(x, fit$scores)
rf <- randomForest(x2, y)
a <- predict(rf, newdata = dfTest2, type = "prob")
dfTest$label <- a[,2]
dfSubmission <- dfTest[, grep('urlid|label',names(dfTest))]

write.table(dfSubmission,file="submissions/submission_4.csv",
            sep=",",row.names=F)         



print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:6] 
plot(load,type="n") # set up plot 
text(load,labels=names(x_num),cex=.7) # add variable names