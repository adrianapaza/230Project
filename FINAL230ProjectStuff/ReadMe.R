
# 
# First Execute TryingtoVectorizeR.R to load in data, and create embeddings for Job posting data.class
# Then execute Glove300DimensionsPenningtonEtal.R to load in Pennington word embeddings 
# (make sure to use the 300 dimensional one downloaded from http://nlp.stanford.edu/data/glove.6B.zip)
# Then execute WikiWords.R to load combined Wikipedia and Job Postings Embeddings.
# 
# By doing the abbove we've loaded and prepared the inputs for the neural networks'

#h2oWordVectors.R was used for hyperparameter searching.

#ByYearTestingWikiCombine.R, GoodAllGlove.R, ByYearTestingOnlyJobb.R are the files that
# are used to generate the results. Yours may differ due to some of the randomness.
# Many random samples can be used and averaged.
# 
#PlottingEmbeddings.R creates the reduced dimension visualization of the word vectors
#found in the poster