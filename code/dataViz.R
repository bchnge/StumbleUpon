require(ggplot2)
require(plyr)

dfTrain$alchemy_category_score <- as.numeric(dfTrain$alchemy_category_score)

test = ddply(.data = dfTrain, .variables=c('alchemy_category'),
             summarize,
             avg_alch_score = mean(alchemy_category_score, na.rm=F)
)

cat_eg = ddply(.data = dfTrain, .variables=c('alchemy_category'),
             summarize,
             evergreen = mean(label, na.rm=F),
             freq = length(alchemy_category)
)

write.table(cat_eg, file = 'cat_eg.tsv', sep = '\t', row.names = F)

