##this file combbines both a wikipedia file of text and the job text to train the vector of words
library(text2vec)
#load/download wikipedia file
text8_file = "~/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)
# Create iterator over tokens
tokensWiki= space_tokenizer(wiki)
##get jobpsotings descritpions
tokensWikib = space_tokenizer(allDataset$CombinedDescString)
#append both wikipedia file and job file for one big list of words to train on
tokensWikid<-append(tokensWiki, tokensWikib)


# Create vocabulary. Terms will be unigrams (simple words).
itWiki  = itoken(tokensWikid , progressbar = F)
vocabWiki  = create_vocabulary(itWiki)

# Use our filtered vocabulary
vectorizerWiki = vocab_vectorizer(vocabWiki)
# use window of 5 for context words
tcmWiki = create_tcm(itWiki, vectorizerWiki, skip_grams_window = 10L)

gloveWiki = GlobalVectors$new(word_vectors_size = 300, vocabulary = vocabWiki, x_max = 10)
wv_mainWiki = gloveWiki$fit_transform(tcmWiki, n_iter = 10, convergence_tol = 0.01)
wv_contextWiki = gloveWiki$components

word_vectorsWiki = wv_mainWiki + t(wv_contextWiki)
GloveDataFrameWiki<-as.data.frame.matrix(word_vectorsWiki)

g<-GloveDataFrameWiki

#keep only words in both the trained words and the dataframe
keep <- colnames(DTMALL)
g<-g[rownames(g) %in% keep, ]
keep<-rownames(g)
DTMALLOVerlap<-DTMALL[,colnames(DTMALL) %in% keep ]

#make sure they are ordered alphabetically so that the words overlap
GloveDataFrameWiki<-g[order(rownames(g)),]
#get cosine similairyt of a sample word:java
cos_sim = sim2(x = word_vectorsWiki[, , drop = FALSE], y = word_vectorsWiki["java", , drop = FALSE], method = "cosine", norm = "l2")
##see which words are closest to java.
head(sort(cos_sim[,1], decreasing = TRUE), 10)





#order the other dataframe  alphabetically
DTMALLOVerlap<-DTMALLOVerlap[,order(colnames(DTMALLOVerlap))]


##multiply matrices to get vector sum of each job description
#get the average word use per description first
DTMALLOverlapMeans<-as.matrix(DTMALLOVerlap) 
DTMALLOverlapMeans<-DTMALLOverlapMeans/rowSums(DTMALLOverlapMeans)
##Now multiply the matrices
SumVectorMatrixOverlap<- as.matrix(DTMALLOverlapMeans)%*%as.matrix(GloveDataFrameWiki)
#Get the final output as a data frame
AverageVectorOverlapDF<-as.data.frame(SumVectorMatrixOverlap)

##reattatch variables

##reattatch Manager variable (capital M since lower case is the actual word manager freqeunce)
AverageVectorOverlapDF$Manager<-DF$Manager

#reattatch accountant
AverageVectorOverlapDF$AccountantPerson<-DF$AccountantPerson

#reattatch year variable too 
AverageVectorOverlapDF$Year<-DF$Year
##make sure IT and ID stays though
AverageVectorOverlapDF$IT<-DF$IT
AverageVectorOverlapDF$ID<-DF$ID
save(AverageVectorOverlapDF,file="AverageVectorOverlapDFWiki300")

