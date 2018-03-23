library(text2vec)
#this file loads in pretrained glove vectorsfrom pennington et al using wikipedia data
#available at https://nlp.stanford.edu/projects/glove/
# function definition --------------------------------------------------------------------------

# input .txt file, exports list of list of values and character vector of names (words)
proc_pretrained_vec <- function(p_vec) {
  
  
  # initialize space for values and the names of each word in vocab
  vals <- vector(mode = "list", length(p_vec))
  names <- character(length(p_vec))
  
  # loop through to gather values and names of each word
  for(i in 1:length(p_vec)) {
    if(i %% 1000 == 0) {print(i)}
    this_vec <- p_vec[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  # this needs testing, does it become numeric?
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[[i]] <- this_vec_name
  }
  
  # convert lists to data.frame and attach the names
  glove <- data.frame(vals)
  names(glove) <- names
  
  return(glove)
}




# using the function -------------------------------------------------------------------------

# here we are reading in the unzipped glove file
g6b_300 <- scan(file = "glove.6b/glove.6B.300d.txt", what="", sep="\n")


# call the function to convert the raw GloVe vector to data.frame (extra lines are for wall-time reporting)
#t_temp <- Sys.time()
glove.300 <- proc_pretrained_vec(g6b_300)  # this is the actual function call
#(t_elap_temp <- paste0(round(as.numeric(Sys.time() - t_temp, units="mins"), digits = 2), " minutes"))
##check dimensions
print(dim(glove.300)) 
##get transpose
glove.300<-t(glove.300)
glove.300<-as.data.frame(glove.300) 





####Now we must prepare data for use on our dataset of job postings
g<-glove.300

#keep only words in both the trained words and the dataframe
keep <- colnames(DTMALL)
g<-g[rownames(g) %in% keep, ]
keep<-rownames(g)
DTMALLOVerlap300<-DTMALL[,colnames(DTMALL) %in% keep ]

#make sure they are ordered alphabetically so that the words overlap
Glove300DataFrame<-g[order(rownames(g)),]






#order the other dataframe  alphabetically
DTMALLOVerlap300<-DTMALLOVerlap300[,order(colnames(DTMALLOVerlap300))]


##multiply matrices to get vector sum of each job description
#get the average word use per description first
DTMALLOverlapMeans300<-as.matrix(DTMALLOVerlap300) 
DTMALLOverlapMeans300<-DTMALLOverlapMeans300/rowSums(DTMALLOverlapMeans300)
##Now multiply the matrices
SumVectorMatrixOverlap300<- as.matrix(DTMALLOverlapMeans300)%*%as.matrix(Glove300DataFrame)
#Get the final output as a data frame
AverageVectorOverlapDF300<-as.data.frame(SumVectorMatrixOverlap300)

##reattatch variables

##reattatch Manager variable (capital M since lower case is the actual word 'manager' frequence)
AverageVectorOverlapDF300$Manager<-DF$Manager

#reattatch accountant
AverageVectorOverlapDF300$AccountantPerson<-DF$AccountantPerson

#reattatch year variable too 
AverageVectorOverlapDF300$Year<-DF$Year
##make sure IT and ID stays though
AverageVectorOverlapDF300$IT<-DF$IT
AverageVectorOverlapDF300$ID<-DF$ID

save(AverageVectorOverlapDF300,file="AverageVectorOverlapDF300")
