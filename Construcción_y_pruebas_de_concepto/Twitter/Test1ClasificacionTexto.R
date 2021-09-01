# Creating the empty dataset with the formatted columns 
dataframe <- data.frame(ID=character(), 
                        datetime=character(), 
                        content=character(), 
                        label=factor()) 

source.url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip' 
target.directory <- '/tmp/clustering-r' 
temporary.file <- tempfile() 
download.file(source.url, temporary.file) 
unzip(temporary.file, exdir = target.directory) 
# Reading the files 
target.directory <- paste(target.directory, 'Health-Tweets', sep = '/') 
files <- list.files(path = target.directory, pattern='.txt$') 
# Filling the dataframe by reading the text content 
for (f in files) { 
  news.filename = paste(target.directory , f, sep ='/') 
  news.label <- substr(f, 0, nchar(f) - 4) # Removing the 4 last characters => '.txt' 
  news.data <- read.csv(news.filename, 
                        encoding = 'UTF-8', 
                        header = FALSE, 
                        quote = "", 
                        sep = '|', 
                        col.names = c('ID', 'datetime', 'content')) 
  # Trick to handle native split problem (cf. notebook for detail) 
  news.data <- news.data[news.data$content != "", ] 
  news.data['label'] = news.label # We add the label of the tweet  
  # Massive data loading memory problem : only loading a few (cf. notebook for detail) 
  news.data <- head(news.data, floor(nrow(news.data) * 0.05)) 
  dataframe <- rbind(dataframe, news.data) # Row appending 
} 
unlink(target.directory, recursive =  TRUE) # Deleting the temporary directory

url <- str_extract(dataframe$content, "http.+")
text <- gsub("http.+", "", dataframe$content)

www <- str_extract(text, "www.+")
text <- gsub("www.+", "", text)


corpus = tm::Corpus(tm::VectorSource(text)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte'))  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words 
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces

tdm <- tm::DocumentTermMatrix(corpus.cleaned) 
tdm.tfidf <- tm::weightTfIdf(tdm)

tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
# Cosine distance matrix (useful for specific clustering algorithms) 
library(proxy)
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

truth.K <- 7

library(dbscan)

clustering.kmeans <- kmeans(tfidf.matrix, truth.K) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}

points <- cmdscale(dist.matrix, k = 2) 
library(colorspace)
palette <- colorspace::diverge_hcl(truth.K) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par)
