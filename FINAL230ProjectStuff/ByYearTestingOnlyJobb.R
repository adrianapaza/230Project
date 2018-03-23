



#Set the data we will be using. In this case it is the three hundred (although more due to ties in frequency) most common words.
#AverageVectorOverlapDF is from combined wiki and jobs ads data
#AverageVectorOverlapDF300 is from Pennington 
#AverageVectorOverlapDF is just job data 
data=AverageVectorDF

sum(AverageVectorOverlapDF300$IT == T)


##We want to split the data into three roughly equal batches by checking how many are in 
##closet approximation is 2004-2008 (size = 6452),2009-2012 (size = 6548 ),2013-2015 (size=6001)
#split the datasets
Data04_08<-data[(data$Year<=2008),]
Data09_12<-data[(data$Year<=2012),]
Data09_12<-Data09_12[(Data09_12$Year>2008),]
Data13_16<-data[(data$Year>2012),]

#For all data sets we will split it into training and test data using a 70-30 split.

##First Get a random sample of indices based upon row numbers (each row is an observation)
index <- sample(1:nrow(Data04_08),round(0.70*nrow(Data04_08)))
##then split into train and test.
train04_08 <- Data04_08[index,]
test04_08 <- Data04_08[-index,]

##Repeat above for each sequence of years
index <- sample(1:nrow(Data09_12),round(0.70*nrow(Data09_12)))
train09_12 <- Data09_12[index,]
test09_12 <- Data09_12[-index,]

index <- sample(1:nrow(Data13_16),round(0.70*nrow(Data13_16)))
train13_16 <- Data13_16[index,]
test13_16 <- Data13_16[-index,]


##Make the data into H20 readable data for each year group. Get testing, training,
#and all data for each year in h20 readable format
train.h2o04_08 <- as.h2o(train04_08)
test.h2o04_08 <- as.h2o(test04_08)
train.h2o09_12 <- as.h2o(train09_12)
test.h2o09_12 <- as.h2o(test09_12)
train.h2o13_16 <- as.h2o(train13_16)
test.h2o13_16 <- as.h2o(test13_16)

All.h2013_16 <-as.h2o(Data13_16)
All.h2009_12 <-as.h2o(Data09_12)
All.h2004_08 <-as.h2o(Data04_08)


# As a good make sure we get right dependent variable by checking the column name. In this case we want IT
#After checking data using the view function we see IT is in column 310
if  (names(train.h2o13_16[,304]) == 'IT')
{y.dep <- 304 #Set the dependent Y variable
x.indep <- c(1:300) #set X inputs to be the word frequences of the 309 words
}else{
  message("Wrong variable, IT not used")
}

#Initalize vectors that will be used to store accuracy.
totalerrors13_16<-vector(,10)
totalerrors09_12<-vector(,10)
totalerrors04_08<-vector(,10)

#set the hidden layers and neurons hyperparameter
HiddenLayers<-c(400,400,400) # in this case their are two hidden layers, the first has two neurons and second has one
l2loss=2e-3#set l2loss
#Now get average change in error between test data and other data by running the model 50 times.
##we Run the model 50 times due to stochasticity
for (i in 1:10) {
  ##below is our model called 'Learning04_08.model'
  #note that h2o will by default randomly set weights for us to use
  #logistic loss is automatically used since h2o can tell this is a classification problem
  Learning04_08.model <- h2o.deeplearning(y = y.dep, #set outcome variable as IT
                                          x = x.indep, #set input variables
                                          training_frame = train.h2o04_08, #set the training data
                                          epoch = 50,#set number of epochs
                                          hidden = HiddenLayers,
                                          activation = "Rectifier", #set the activation function. Tanh was chosen after tuning.
                                          l2=l2loss, #choose L2 loss and set parameter for penalizing weights
                                          #This loss was chosen after extesnive tuning
                                          # l1=1e-5
                                          stopping_metric = 'logloss',
                                          #rate = 0.5, #learningrate
                                          #But we are using an adaptive rate
                                          #stopping_tolerance = 0.0001,
                                          #seed = 1122# A basic seed
  ) 
  
  ##Get test predictions for 04-08 test data using the above model
  predict04_08.04_08 <- as.data.frame(h2o.predict(Learning04_08.model, test.h2o04_08))
  #Truncate  prediction if greater than or less than one. And get distance from true value which is in test04_08$IT
  Binaryloss<-(as.numeric(predict04_08.04_08$predict)-1)-test04_08$IT #subtract by 1 because h2o codes False as 1 and true as 2
  ##save the accuracy of the model into a vector
  totalerrors04_08[i] = mean(Binaryloss^2)
  
  ##predict 09-12 data repeting above steps but using all avaialbe data in 09-12
  predict04_08.09_12 <- as.data.frame(h2o.predict(Learning04_08.model, All.h2009_12))
  Binaryloss<-(as.numeric(predict04_08.09_12$predict)-1)-Data09_12$IT
  totalerrors09_12[i] = mean(Binaryloss^2)
  
  ###predict 13-16 data repeting above steps but using all avaialbe data in 13-16
  predict04_08.13_16 <- as.data.frame(h2o.predict(Learning04_08.model, All.h2013_16))
  Binaryloss<-(as.numeric(predict04_08.13_16$predict)-1)-Data13_16$IT
  totalerrors13_16[i] = mean(Binaryloss^2)
  
  #update i in our loop
  i = i+1
}


##All of the above is then repeated below, but with models trained on  09-12 data and 13-16 data.
#the comments are not as rich below to avoid repetition


#initalize vectors so that we may later compare data
totalerrorsFrom13_13_16<-vector(,10)
totalerrorsFrom13_09_12<-vector(,10)
totalerrorsFrom13_04_08<-vector(,10)
##now get average change in error between test data and other data
for (i in 1:10) {
  Learning13_16.model <- h2o.deeplearning(y = y.dep,
                                          x = x.indep,
                                          training_frame = train.h2o13_16,
                                          epoch = 50,
                                          hidden = HiddenLayers,
                                          activation = "Rectifier",
                                          l2=l2loss,
                                          # l1=1e-5
                                          stopping_metric = 'logloss',
                                          #rate = 0.5, #learningrate
                                          #But we are using an adaptive rate
                                          #stopping_tolerance = 0.0001,
                                          seed = 1122)
  
  ##get test predictions
  predict13_16.04_08 <- as.data.frame(h2o.predict(Learning13_16.model, All.h2004_08))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict13_16.04_08$predict)-1)-Data04_08$IT
  totalerrorsFrom13_04_08[i] = mean(Binaryloss^2)
  
  ##predict 09-12 data
  predict13_16.09_12 <- as.data.frame(h2o.predict(Learning13_16.model, All.h2009_12))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict13_16.09_12$predict)-1)-Data09_12$IT
  totalerrorsFrom13_09_12[i] = mean(Binaryloss^2)
  
  ##predict 13-16 data
  predict13_16.13_16 <- as.data.frame(h2o.predict(Learning13_16.model, test.h2o13_16))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict13_16.13_16$predict)-1)-test13_16$IT
  totalerrorsFrom13_13_16[i] = mean(Binaryloss^2)
  i = i+1
}
















##now let's train on the midpoint of the data
#initalize vectors so that we may later compare data
totalerrorsFrom09_13_16<-vector(,10)
totalerrorsFrom09_09_12<-vector(,10)
totalerrorsFrom09_04_08<-vector(,10)
##now get average change in error between test data and other data
for (i in 1:10) {
  Learning09_12.model <- h2o.deeplearning(y = y.dep,
                                          x = x.indep,
                                          training_frame = train.h2o09_12,
                                          epoch = 50,
                                          hidden = HiddenLayers,
                                          activation = "Rectifier",
                                          l2=l2loss,
                                          # l1=1e-5
                                          stopping_metric = 'logloss',
                                          #rate = 0.5, #learningrate
                                          #But we are using an adaptive rate
                                          #stopping_tolerance = 0.0001,
                                          seed = 1122)
  
  ##get test predictions
  predict09_12.04_08 <- as.data.frame(h2o.predict(Learning09_12.model, All.h2004_08))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict09_12.04_08$predict)-1)-Data04_08$IT
  totalerrorsFrom09_04_08[i] = mean(Binaryloss^2)
  
  ##predict 09-12 data
  predict09_12.09_12 <- as.data.frame(h2o.predict(Learning09_12.model, test.h2o09_12))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict09_12.09_12$predict)-1)-test09_12$IT
  totalerrorsFrom09_09_12[i] = mean(Binaryloss^2)
  
  ##Get IT miscategorizations
  BinarylossIT<-(as.numeric(predict09_12.09_12$predict)-1)-test09_12$IT
  sum((as.numeric(predict09_12.09_12$predict)-1) == test09_12$IT)
  ##Number IT correctly labeled
  #  ITCorrectLabel <- sum((predict09_12.09_12$predict==T &test09_12$IT==T))/sum((test09_12$IT==T))
  
  ##predict 13-16 data
  predict09_12.13_16 <- as.data.frame(h2o.predict(Learning09_12.model, All.h2013_16))
  #truncate if greater than or less than one
  Binaryloss<-(as.numeric(predict09_12.13_16$predict)-1)-Data13_16$IT
  totalerrorsFrom09_13_16[i] = mean(Binaryloss^2)
  
  ##Number IT correctly labeled
  #ITCorrectLabel <- sum((predict09_12.13_16$predict==T & All.h2013_16$IT==T))/sum((All.h2013_16$IT==T))
  
  i = i+1
}



##Now I get the all the average errors. Which I then compare and use to make the graphs in my paper.

sum(totalerrors13_16)/10
sum(totalerrors09_12)/10
sum(totalerrors04_08)/10

sum(totalerrorsFrom13_13_16)/10
sum(totalerrorsFrom13_09_12)/10
sum(totalerrorsFrom13_04_08)/10

sum(totalerrorsFrom09_13_16)/10
sum(totalerrorsFrom09_09_12)/10
sum(totalerrorsFrom09_04_08)/10

