##### DEFINE FUNCTIONS TO BE USED IN MAPPER FUNCTIONS

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
}


splitter <- function(f, n = 5){
  # Function for preparing our data...splitting it into separate chunks of files
  splt <- rep_len(1:n, length(f))
  f <- as.data.frame(cbind(f,splt))
  for(i in 1:n) {
    chunk <- f[f$splt == i,1]
    chunkText <- apply(as.matrix(chunk),1,getWebText)    
    save(chunkText,file=paste('chunk',i,'of',n,'.RData',sep='_'))
  }
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
  d <- iconv(d, 'UTF-8')
  d <- tolower(d)               
  d <- removePunctuation(d)
  d <- removeNumbers(d)
  d <- removeWords(d, stopwords('english'))
}

getWordCount <- function(chunk){
  ### Extract word count of each term in chunk of documents
  ### uses cleanup
  require(plyr)
  require(tm)
  require(SnowballC)

  docs <- strsplit(chunk, split = ' ')
  docs <- llply(docs, function(x) (x[x!=""]))
  docs <- apply(as.matrix(docs), MARGIN = 1, FUN = cleanDoc)
  
  dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)))
  counts <- as.data.frame(colSums(as.matrix(dtm)))
  counts$term <- row.names(counts)
  names(counts) <- c('Freq','term')
  counts <- counts[rev(order(counts$Freq)),]
}



# 1  PRELIMINARY STEPS #########################
setwd('~/Projects/StumbleUpon/')

dfTrain <- read.table(file = 'data/train.tsv', sep = '\t', header=T,stringsAsFactors=F)
files <- unique(dfTrain$urlid)

# 2  Mapper function #########################

### Mapper

map <- function(f, n = 5){
  # First split files into n chunks
  splitter(f, n) 
  for(i in 1:n){
    load(paste('chunk',i,'of',n,'.RData', sep = '_'))    
    # Process each chunk to get the word count
    temp <- getWordCount(chunkText)
    write.table(temp, file = paste('wc',i,'of',n,'.tsv',sep = '_'), sep = '\t', row.names = F)
  }
} 

# 3 Reducer function ########################
reduce <- function(n = 5){
 # Combine results from each chunk
  df <- data.frame(matrix(nrow = 1, ncol = 2))
  names(df) <- c('term','Freq')
  for(i in 1:n){
    temp <- read.csv(file = paste('wc',i,'of',n,'.tsv',sep = '_'), sep = '\t', header = T)
    df <- rbind.data.frame(df,temp)
  }
  df <- df[-1,]
  require(plyr)
  # Collapse and summarize
  aggTable <- ddply(.data = df, .variables=c('term'), summarize,
                    frequency <- sum(Freq)
                    )
  row.names(aggTable) <- NULL
  names(aggTable) <- c('term','Freq')
  aggTable <- aggTable[rev(order(aggTable[,2])),]
  save(aggTable, file = 'finalWordCount.RData')
}


#4 Enjoy the fruits of our labor

map(files, 5)
reduce(5)

load('finalWordCount.RData')
