##Let's plot the word embeddings
# load the Rtsne package
library(Rtsne)
# run Rtsne with default parameters for job posting data
rtsne_outJob <- Rtsne(as.matrix(GloveDataFrame))
# plot the output of Rtsne 
jpeg("OnlyJobEmbeddingsimage", width=2400, height=1800)
plot(rtsne_outJob$Y, t='n', main="OnlyJobEmbeddings")
text(rtsne_outJob$Y, labels=rownames(GloveDataFrame),cex=0.3)
save(rtsne_outJob,file="rtsne_outJob")


# run Rtsne with default parameters for job+Wiki  data
rtsne_outJobWiki <- Rtsne(as.matrix(GloveDataFrameWiki))
# plot the output of Rtsne 
jpeg("wikiJobEmbeddingsimage", width=2400, height=1800)
plot(rtsne_outJobWiki$Y, t='n', main="wikiJobEmbeddings")
text(rtsne_outJobWiki$Y, labels=rownames(GloveDataFrameWiki),cex=0.3)

save(rtsne_outJobWiki,file="rtsne_outJobWiki")

# run Rtsne with default parameters for Pennington  data
rtsne_out300Penn <- Rtsne(as.matrix(Glove300DataFrame))
# plot the output of Rtsne 
jpeg("PennEmbeddingsimage", width=2400, height=1800)
plot(rtsne_out300Penn$Y, t='n', main="PennEmbeddingsimage")
text(rtsne_out300Penn$Y, labels=rownames(sample(size=100,Glove300DataFrame)),cex=0.1)

save(rtsne_out300Penn,file="rtsne_out300Penn")
