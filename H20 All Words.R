library(h2o)
##Note Please see file of H20ByYEar first. It has more descriptions of what h20 is.
##this file was used for hyperparameter searching but with all words,
##In this file we use just about every word (those that appear no less than 6 times) to train our network
##However I don't use this because of how slow it is even after much tuning of hyperparameters including momentum
#learning rate, l2 and l1 regularization, minibatch sizes

h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll

##apparently the varaible 'n1' is constant (Collinearity) so we should drop it because it is exactly the same as another input variable
DTMALLTRUE = DTMALLTRUE[,!(names(DTMALLTRUE) %in% 'n1')]


#set data
dataAll=DTMALLTRUE
##randomly select training and test datasets based upon index
index <- sample(1:nrow(dataAll),round(0.75*nrow(dataAll)))
Alltrain <- dataAll[index,]
Alltest <- dataAll[-index,]

#Make the train and test data H2o readable
Alltrain.h2o <- as.h2o(Alltrain)
Alltest.h2o <- as.h2o(Alltest)




#check the names if i want wit the follwing names(Alltrain.h2o)
#Set the dependent variable (IT)
yAll.dep <- 10114
#Check to make sure it is the IT 
names(Alltrain.h2o[,10114])
#independent variables (Drop IT ID and Year variables)
xAll.indep <- c(1:10112)


#Metric to use for early stopping (AUTO is logloss for classification, deviance for regression)
dlearningAll.model <- h2o.deeplearning(y = yAll.dep,
                                        x = xAll.indep,
                                        training_frame = Alltrain.h2o,
                                        epoch = 60,        #Epochs
                                        hidden = c(2,1),
                                        activation = "Tanh",
                                        l2=2e-3,
                                        # l1=2e-3,                      #manually tuned L1 regularization 
#                                       adaptive_rate=F,                #see documentation but this if set to TRUE will update the hyperparameter learning rate to speed training
#                                       momentum_start=0.9,             ## manually tuned momentum 
#                                         
#                                          rate=0.99,      ## manually tuned learning rate 
                                      #  mini_batch_size =  round( 0.05*nrow(ALLtrain.h2o)),  #set minibatch size
                                        seed = 1122)

##get performance
#h2o.performance(dlearningAll.model)
##get test predictions

##get predictions from train data
predictTrain.dl2 <- as.data.frame(h2o.predict(dlearningAll.model, Alltrain.h2o))
#Truncate  prediction if greater than or less than one. And get distance from true value which is in Alltrain$IT 
BinarylossTrain<-(as.numeric(predictTrain.dl2$predict)-1)-Alltrain$IT #subtract by 1 because in h2o False is 1 and true is 2
#Get mean of losses. We must square so -1 becomes one
mean(BinarylossTrain^2)


#Repeat above but for the test data
predict.dl2 <- as.data.frame(h2o.predict(dlearningAll.model, Alltest.h2o))

Binaryloss<-(as.numeric(predict.dl2$predict)-1)-Alltest$IT
mean(Binaryloss^2)

