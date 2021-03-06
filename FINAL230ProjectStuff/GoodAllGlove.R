library(h2o)
##Note Please see file of H20ByYEar first. It has more descriptions of what h20 is.
##this file was used for hyperparameter searching but with all words from Pennington.

h2o.init()
h2o.removeAll

#set data
##AverageVectorDF is trained on only job data
#AverageVectorOverlapDF300 is only from pennington
#AverageVectorOverlapDF is from combined wiki and jobs ads data
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
#Check to make sure it is the IT variable
names(Alltrain.h2o[,304])
#independent variables (Drop IT ID and Year variables)
x.indep <- c(1:300)
HiddenLayers<-c(100,1)  #set hidden layers andneurons in each

LearningVec.model <- h2o.deeplearning(y = y.dep, #set outcome variable as IT
                                      x = x.indep, #set input variables
                                      training_frame = Alltrain.h2o, #set the training data
                                      epoch = 100,#set number of epochs
                                      hidden = c(300,300,300),
                                      activation = "Rectifier", #set the activation function. Tanh was chosen after tuning.
                                      l2=8e-3, #choose L2 loss and set parameter for penalizing weights
                                      #This loss was chosen after extesnive tuning
                                      # l1=1e-5
                                      stopping_metric = 'logloss', #we will use logistic loss cost function
                                      #rate = 0.5, #learningrate if we set it manually
                                      #But we are using an adaptive rate
                                      #stopping_tolerance = 0.0001,
                                      #seed = 1122# A basic seed
                                      # mini_batch_size=200,
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

predictTrain.dl2 <- as.data.frame(h2o.predict(LearningVec.model, Alltest.h2o))
#Truncate  prediction if greater than or less than one. And get distance from true value which is in Alltrain$IT 
BinarylossTest<-(as.numeric(predictTrain.dl2$predict)-1)-Alltest$IT #subtract by 1 because in h2o False is 1 and true is 2
#Get mean of losses. We must square so -1 becomes one
mean(BinarylossTest^2)
