library(h2o) ##load the library h2o
##Note Please see file of H20ByYEar first. It has more descriptions of what h20 is.
##this file was used for hyperparameter searching

#h20 is areally great library that that allows one to build deep nueral nets easily
#while having great freedom to choose hyperparameters
##documentation available at https://cran.r-project.org/web/packages/h2o/h2o.pdf
#And here is a link to the startup that developed H2o
#You can skip down in the documentation to page 52 to see the deep learning model.
#However h2o does not work with java 9 so java 7 should be installed and used (or some other versions must be used) 

##The following link also provides a nice introduction and shows its versatality and tuning options which
#https://github.com/h2oai/h2o-tutorials/tree/master/tutorials/deeplearning


##start a new h2o session.
h2o.init(nthreads=-1, max_mem_size="2G") #allow h2o to use all cpu cores and determine memory allocation.

h2o.removeAll() ## reset h2o in case it was already running.

##set train and dest data
#Set the data we will be using
data=DTMALLTopthreeRealWords
##randomly get training and test datasets. we make 25% of the data test data
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
DTMALLTopthreeRealWords$IT


##make the data into H20
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
names(test.h2o[,310]) ##check to make sure we picked the right dependent variable (IT)
#dependent variable (IT)
y.dep <- 310
##get manager varaible
yManage.dep <- 312

#independent variables (dropping ID and IT variables)
x.indep <- c(1:309)

#another sample model
dlearning.model <- h2o.deeplearning(y = y.dep,
                                    x = x.indep,
                                    training_frame = train.h2o,
                                    epoch = 60,
                                    hidden = c(1),
                                    activation = "Tanh",
                                    l2=1e-5,
                                    seed = 1122)

##get performance
h2o.performance(dlearning.model)
##get test predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))

Loss<-predict.dl2$TRUE.-test$IT
##so the mean loss is still 0.0707
mean(Loss^2)




##now do with L2 regularization
dlearningLTwo.model <- h2o.deeplearning(y = y.dep,
                                    x = x.indep,
                                    training_frame = train.h2o,
                                    epoch = 60,
                                    hidden = c(2,1),
                                    activation = "Tanh",
                                    l2=2e-3,
                                   # l1=1e-5
                                    seed = 1122)

##get performance
h2o.performance(dlearningLTwo.model)
##get test predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearningLTwo.model, test.h2o))

Loss<-predict.dl2$TRUE.-test$IT
Binaryloss<-(as.numeric(predict.dl2$predict)-1)-test$IT

#mean binaryloss is 8% so on average we mis classify IT job 8% of the time
mean(Binaryloss^2)
##so the mean  squredloss is still 0.06576489709 (0.08273684211 for binary)
mean(Loss^2)

#Best model is as follows for predicting IT
# where data is top three
# dlearningLTwo.model <- h2o.deeplearning(y = y.dep,
#                                         x = x.indep,
#                                         training_frame = train.h2o,
#                                         epoch = 60,
#                                         hidden = c(2,1),
#                                         activation = "Tanh",
#                                         l2=2e-3,
#                                         # l1=1e-5
#                                         seed = 1122)







##now let's do a model preiditcing that the job title is manger or director
##now do with L2 regularization
dlearningLTwoManage.model <- h2o.deeplearning(y = yManage.dep,
                                        x = x.indep,
                                        training_frame = train.h2o,
                                        epoch = 60,
                                        hidden = c(5,2),
                                        activation = "Tanh",
                                        l2=2e-3,
                                        # l1=1e-5
                                        seed = 1100)

##get performance
#h2o.performance(dlearningLTwoManage.model)
##get test predictions
predictManage.dl2 <- as.data.frame(h2o.predict(dlearningLTwoManage.model, test.h2o))

Binaryloss<-(as.numeric(predictManage.dl2$predict)-1)-test$Manager
#mean loss is 
mean(Binaryloss^2)
##so the mean  squredloss 


predictManageTrain.dl2 <- as.data.frame(h2o.predict(dlearningLTwoManage.model, train.h2o))

Loss<-predictManageTrain.dl2$TRUE.-train$Manager
Binaryloss<-(as.numeric(predictManageTrain.dl2$predict)-1)-train$Manager
#mean loss is best at 5,2 layers of 0.1121324819
mean(Binaryloss^2)
##so the mean  squredloss 
mean(Loss^2)











##sample model
m3 <- h2o.deeplearning(
  model_id="dl_model_tuned", 
  training_frame=train, 
  validation_frame=valid, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(1),          ## more hidden layers -> more complex interactions
  epochs=2,                      ## to keep it short enough
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.25,         ## don't score more than 25% of the wall time
#   adaptive_rate=F,                ## manually tuned learning rate
#   rate=0.01, 
#   rate_annealing=2e-6,            
#   momentum_start=0.2,             ## manually tuned momentum
#   momentum_stable=0.4, 
#   momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(m3)