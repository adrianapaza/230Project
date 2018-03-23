library(h2o)
##Note Please see file of H20ByYEar first. It has more descriptions of what h20 is.
##this file was used for hyperparameter searching but with all words,
##In this file we use just about every word (those that appear no less than 6 times) to train our network
##However I don't use this because of how slow it is even after much tuning of hyperparameters including momentum
#learning rate, l2 and l1 regularization, minibatch sizes

h2o.init()
h2o.removeAll

#set data
##AverageVectorOverlapDF is traiend on wikipedia and job data
#AverageVectorOverlapDF300 is only from pennington
dataAll=AverageVectorOverlapDF300
##randomly select training, deb and test datasets based upon index
index <- sample(1:nrow(dataAll),round(0.75*nrow(dataAll)))
Alltrain <- dataAll[index,]
AlltestDev <- dataAll[-index,]
index <- sample(1:nrow(AlltestDev),round(0.5*nrow(AlltestDev)))
Alldev <- AlltestDev[index,]
Alltest <- AlltestDev[-index,]

#Make the train and test data H2o readable
Alltrain.h2o <- as.h2o(Alltrain)
Alltest.h2o <- as.h2o(Alltest)
Alldev.h2o <- as.h2o(Alldev)




#set dependent variable and independent variables

y.dep <- 304
#Check to make sure it is the IT 
names(Alltrain.h2o[,304])
#independent variables (Drop IT ID and Year variables)
x.indep <- c(1:300)
HiddenLayers<-c(100,1)  #set hidden layers andneurons in each

LearningVec.model <- h2o.deeplearning(y = y.dep, #set outcome variable as IT
                                        x = x.indep, #set input variables
                                        training_frame = Alltrain.h2o, #set the training data
                                        epoch = 100,#set number of epochs
                                        hidden = c(100,100,10),
                                        activation = "Rectifier", #set the activation function. Tanh was chosen after tuning.
                                        l2=8e-3, #choose L2 loss and set parameter for penalizing weights
                                        #This loss was chosen after extesnive tuning
                                        # l1=1e-5
                                        stopping_metric = 'logloss', #we will use logistic loss cost function
                                        #rate = 0.5, #learningrate if we set it manually
                                        #But we are using an adaptive rate
                                        #stopping_tolerance = 0.0001,
                                        #seed = 1122# A basic seed
) 
##get predicted values for training data
predictTrain.dl2 <- as.data.frame(h2o.predict(LearningVec.model, Alltrain.h2o))
#Truncate  prediction if greater than or less than one. And get distance from true value which is in Alltrain$IT 
BinarylossTrain<-(as.numeric(predictTrain.dl2$predict)-1)-Alltrain$IT #subtract by 1 because in h2o False is 1 and true is 2
#Get mean of losses. We must square so -1 becomes one
mean(BinarylossTrain^2)


##get predicted values for dev data
predictTrain.dl2 <- as.data.frame(h2o.predict(LearningVec.model, Alldev.h2o))
#Truncate  prediction if greater than or less than one. And get distance from true value which is in Alltrain$IT 
BinarylossDev<-(as.numeric(predictTrain.dl2$predict)-1)-Alldev$IT #subtract by 1 because in h2o False is 1 and true is 2
#Get mean of losses. We must square so -1 becomes one
mean(BinarylossDev^2)
