setwd('~/Projects/StumbleUpon/')

dfTrain <- read.csv('train.tsv',sep='\t',stringsAsFactors=FALSE)
dfTest <- read.csv('test.tsv', sep = '\t',stringsAsFactors=FALSE)

logit_1 <- glm(label ~ html_ratio + image_ratio + embed_ratio + is_news, data = dfTrain, family = "binomial")

summary(logit_1)
require(coefplot)
coefplot(logit_1)

dfTest$label <- predict(logit_1, newdata = dfTest, type = "response")
dfSubmission <- dfTest[, grep('urlid|label',names(dfTest))]

write.table(dfSubmission,file="submissions/submission_1.csv",
            sep=",",row.names=F)         
