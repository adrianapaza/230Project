
##load the required libraries
library(tm)
library(text2vec)
library(data.table)
#library(ngram) I have not used ngrams yet
#Check to ensure correct working directory if need be with
#getwd()

##Load in the data from the file
DF<-read.csv("online-job-postings 2.csv")
X <- read.csv(url("http://drive.google.com/open?id=1uyT9mjp0A6g5_Tra2OGfdaOVy6UCOaJ6"))

DFlist<-DF
##Add other job indicatiors:
##Ereate avariable eaual to True if job title is for a manager or director. The grepl function searches to see if the input
#String is found in the data. We ignore upper and lower case distinction. 
DF$Manager<-(grepl("manager" , DF$Title, ignore.case = T)|grepl("director" , DF$Title, ignore.case = T))

##Ereate avariable eaual to True if the title is for an accountant.
DF$AccountantPerson<-grepl("accountant" , DF$Title, ignore.case = T)

# ##Make it a list to use for token
# DFlist<-apply(DFlist,2,string)
# is.list(DFlist$JobDescription)
# #reapply it to the main dataframe
# DF$listJobReq<-DFlist$JobRequirment
# ##add a column of ids so we may easily sample from dataframe
 DF$ID <- seq.int(nrow(DF))




##Put job description data into a variable that is a readable as variable type character
DF$JobDescString <- as.character(DF$JobDescription)

##All fields in the data are not quite what we expect after looking at them. Sometimes the  description and qualification data are in location field
#or they do other mis inputting when describing themselves.
#So I combine location, description, requirement, and qual into one variable called
DF$CombinedDescription <- paste(DF$JobDescription, DF$JobRequirment, DF$Location,DF$RequiredQual)

#Make the data readable as a string
DF$CombinedDescString <- as.character(DF$CombinedDescription)
DF$CombinedDescString[1]
##we can't have "'" or _ or - in words so we must drop as otherwise R will think it's quote which will affect functions
#used later
DF$CombinedDescString <- gsub("'", '', DF$CombinedDescString) #this function substitutes ' with nothing
DF$CombinedDescString <- gsub("-", ' ', DF$CombinedDescString) #this function substitutes ' with nothing
DF$CombinedDescString <- gsub("_", ' ', DF$CombinedDescString) #this function substitutes ' with nothing
#DF$CombinedDescString <- gsub(".", '', DF$CombinedDescString) #this function substitutes ' with nothing
#drop numbers too

# 
# it = itoken(head(DFlist$JobDescription, N), preprocessor = tolower, tokenizer = word_tokenizer)
# vocab = create_vocabulary(it)
# #create document term matrix
# dtm = create_dtm(it, vocab_vectorizer(vocab))
# #Creates BNS (bi-normal separation) model. Defined as: Q(true positive rate) - Q(false positive
# #rate), where Q is a quantile function of normal distribution.
# model_bns = BNS$new(treshold = 0.0005)
# dtm_bns = model_bns$fit_transform(dtm, head(DFlist$sentiment, N))
# 
# 
# ?itoken
# DF$AA<-apply(DF$JobDescription,2,list)
# 


##Now we must prepare the data so that it may later create a document term matrix.

#we must convert the dataframe to a matrix by using the below functions
DFMat<- as.matrix.data.frame(DF)
setDT(DF)
setkey(DF, ID)
all_ids = DF$ID
allDataset<-DF[J(all_ids)]

##drop all numbers
allDataset$CombinedDescString <-gsub('[0-9]+', '', allDataset$CombinedDescString)
#replace underscore
allDataset$CombinedDescString <-gsub('_', ' ', allDataset$CombinedDescString)


#Now we wish to tokenize our input strings of words by implementing text2vec in R which is sort of like word2vec.
#documentation available at https://cran.r-project.org/web/packages/text2vec/text2vec.pdf
prep_fun = tolower
tok_fun = word_tokenizer
it_all = itoken(allDataset$CombinedDescString, 
                preprocessor = prep_fun, 
                tokenizer = tok_fun, 
                ids = allDataset$ID, 
                progressbar = FALSE)
#Get a frequency of each term into vocab vector
vocabAll = create_vocabulary(it_all)

##Create a document term matrix. It checks how often per job description each term appears.
vectorizerAll = vocab_vectorizer(vocabAll)
dtm_All = create_dtm(it_all, vectorizerAll)


###Now let's get a term co-occurence matrix, (this gets's all terms withn N L terms of a given term in each description)
#we use 'symmetric' to get terms both preceding and after the given term
tcm <- create_tcm(it_all, vectorizerAll, skip_grams_window = 10L,skip_grams_window_context = 'symmetric')

#now use glove trained on our corpus of descriptions. and use both 100 dimensions and 200
glove = GlobalVectors$new(word_vectors_size = 300, vocabulary = vocabAll, x_max = 10)
glove200 = GlobalVectors$new(word_vectors_size = 200, vocabulary = vocabAll, x_max = 10)

wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
wv_main200 = glove200$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)








#get word vectors
wv_context = glove$components
word_vectors = wv_main + t(wv_context)
wv_context200 = glove200$components
word_vectors200 = wv_main200 + t(wv_context200)
#check similairty using the example of the word 'java'
cos_sim = sim2(x = word_vectors[, , drop = FALSE], y = word_vectors["android", , drop = FALSE], method = "cosine", norm = "l2")
##see which words are closest to java.
head(sort(cos_sim[,1], decreasing = TRUE), 10)
#get distance of two words
word_vectors["youtube", , drop = FALSE]-word_vectors["java", , drop = FALSE]
##now get a dataframe 
GloveDataFrame<-as.data.frame.matrix(word_vectors)
GloveDataFrame200<-as.data.frame.matrix(word_vectors200)
##order it alphabetically
GloveDataFrame<-GloveDataFrame[order(rownames(GloveDataFrame)),]
GloveDataFrame200<-GloveDataFrame200[order(rownames(GloveDataFrame200)),]


##Turn the matrix of all the words into a dataframe.
DTMALL<-as.data.frame.matrix(dtm_All)
#order it alphabetically
DTMALL<-DTMALL[,order(colnames(DTMALL))]

##multiply matrices to get vector sum of each job description
#get the average word use per description first
DTMALLMeans<-as.matrix(DTMALL) 
DTMALLMeans<-DTMALLMeans/rowSums(DTMALLMeans)
##Now multiply the matrices
SumVectorMatrix<- as.matrix(DTMALLMeans)%*%as.matrix(GloveDataFrame)
SumVectorMatrix200<- as.matrix(DTMALLMeans)%*%as.matrix(GloveDataFrame200)
#Get the final output as a data frame
AverageVectorDF<-as.data.frame(SumVectorMatrix)
AverageVectorDF200<-as.data.frame(SumVectorMatrix200)
dim(AverageVectorDF200)

##reattatch variables

##reattatch Manager variable (capital M since lower case is the actual word manager freqeunce)
AverageVectorDF$Manager<-DF$Manager
AverageVectorDF200$Manager<-DF$Manager

#reattatch accountant
AverageVectorDF$AccountantPerson<-DF$AccountantPerson
AverageVectorDF200$AccountantPerson<-DF$AccountantPerson

#reattatch year variable too 
AverageVectorDF$Year<-DF$Year
AverageVectorDF200$Year<-DF$Year
##make sure IT and ID stays though
AverageVectorDF$IT<-DF$IT
AverageVectorDF200$IT<-DF$IT
AverageVectorDF$ID<-DF$ID
AverageVectorDF200$ID<-DF$ID
#save the files
save(AverageVectorDF200,file="AverageVectorDF200")
save(AverageVectorDF,file="AverageVectorDF")


##Below this we go back to looking at a dataframe/matrix that is a bag of words and is term occurence
#eg rows of each entry and columns for frequency of each individual word
##Reattatch Year variable, Manager variable, and accountant variable

DTMALL$YEAR<-DF$Year
DTMALL$Manager<-DF$Manager
DTMALL$AccountantPerson<-DF$AccountantPerson


##We want to drop if a term only appears at least 6 times ever in the entire corpus.
#We do this because it allows us to create a dataframe to input into our neural network models that has
#fewer inputs and will compute faster.
#I do this because after much tuning I found that there was little predictive value from words that appear rarely.
#Computation time was really hindered and a decision was made to drop infrquent words.
 DTMALLTRUE<-DTMALL[,colSums(DTMALL) > 5]
 #Reattatch Variables that were dropped
 DTMALLTRUE$ID<-DF$ID
 DTMALLTRUE$IT<-DF$IT
 DTMALLTRUE$YEAR<-DF$Year


##We want to drop if a term only appears at least 100 times ever for similar reasons described above 
 #because we still fail to lose more than negligible predictve power but can speed up computation
DTMALLNew <- DTMALLTRUE[,colSums(DTMALLTRUE) > 100]

##Reattatch the numeric for IT or not as the last column
DTMALLNew$IT<-DF$IT

##Reattach the ID number so we can more easily keep track of data and sample
DTMALLNew$ID<-DF$ID
DTMALLNew$Manager<-DF$Manager
#Order the data alphabetically by column to make it easier to view using function view()
#DTMALLNew<-DTMALLNew[ , order(names(DTMALLNew))]

#check your data if neec be view(DTMALLNew)
#Drop the ones that feature numbers

##specify ones to drop. Including numbers and preposionts and NA
#drop <- c('1','2','3', 'and','for','NA','of','in','function','if',"company's")
#DTMALLNew = DTMALLNew[,!(names(DTMALLNew) %in% drop)]

DTMALLTopthree <- DTMALLNew

#this gets top 300 occurencences (tie at 301)
DTMALLTopthree <- DTMALLNew[,colSums(DTMALLNew) > 277]
##remove numeric columns

#names(DTMALLNew) check the names to ensure data is correct


##we will manually find some important words

adminwords = c("accountant" ,"accounting" ,"administration","administrative"  , "administrator",
               "assistant","analyst","assist","assistant"
               )
techwords= c("analyst","database","applications","application")

#findFreqTerms(dtm, 100)


##remove words less than length 3

#DTMALLNew[!nchar(as.character(names(DTMALLNew))) > 3, ]
DTMRealWords<-DTMALLNew[,!nchar(as.character(names(DTMALLNew))) < 3 ]
DTMALLTopthreeRealWords<-DTMALLTopthree[,!nchar(as.character(names(DTMALLTopthree))) < 10 ]

##make sure IT and ID stays though
DTMALLTopthreeRealWords$IT<-DF$IT
DTMALLTopthreeRealWords$ID<-DF$ID
##reattatch Manager variable (capital M since lower case is the actual word manager freqeunce)
DTMALLTopthreeRealWords$Manager<-DF$Manager
#reattatch year variable too 
DTMALLTopthreeRealWords$Year<-DF$Year

##DONT DO BELOW




# ## 
# it_train = itoken(train$JobDescString, 
#                   preprocessor = prep_fun, 
#                   tokenizer = tok_fun, 
#                   ids = train$ID, 
#                   progressbar = FALSE)
# #Get a frequency of each term into vocab vector
# vocab = create_vocabulary(it_train)
# 
# ?itoken
# 
# ##create a document term matrix. It checks how often per job description each term appears
# vectorizer = vocab_vectorizer(vocab)
# t1 = Sys.time()
# dtm_train = create_dtm(it_train, vectorizer)
# print(difftime(Sys.time(), t1, units = 'sec'))
# 
# 
# ##check the dimensions of the document term matrix
# dim(dtm_train)
# DTMDF<-as.data.frame.matrix(dtm_train)
# ##Note that the row number of  DTMDF matches wit
# #But to access it you need to use quotes
# 
# 
# ##we want to drop if a term only appears at least 5 times ever to prevent overfitting
# DTMDFNEW <- DTMDF[,colSums(DTMDF) > 5]


##create bigram
vocabAllBigram = create_vocabulary(it_all,ngram = c(1L, 2L))

##Create a document term matrix for BiGrams. It checks how often per job description each term appears.
##only keep bigrams which appear at least three times
vocabAllBigram = prune_vocabulary(vocabAllBigram, term_count_min = 5, term_count_max = Inf)
vectorizerAllBigram = vocab_vectorizer(vocabAllBigram)
dtm_AllBigram = create_dtm(it_all, vectorizerAllBigram)
DTMALLBiGram<-as.data.frame.matrix(dtm_AllBigram)
DTMALLBiGramNew<-DTMALLBiGram

##reattatch variables
##make sure IT and ID stays though
DTMALLBiGramNew$IT<-DF$IT
DTMALLBiGramNew$ID<-DF$ID
##reattatch Manager variable (capital M since lower case is the actual word manager freqeunce)
DTMALLBiGramNew$Manager<-DF$Manager
#reattatch year variable too 
DTMALLBiGramNew$Year<-DF$Year


