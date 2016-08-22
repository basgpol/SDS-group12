##=========================================================================================
##--------------------------------- 4. Prediction Models _---------------------------------
##=========================================================================================

#install.packages("glmnet")
library("glmnet")

## Notes ##
# Possible prediction models
#   Average
#   OlS    
#   Lasso
#   Support Vector machine
#   Regression Tree
#   Random forest

### Proposed process
## Splitting the data randomly into a test and a training set
## Internal validation: Cross-validation on the training set 
## External validation: Using the model on test set 

## http://stats.stackexchange.com/questions/103459/how-do-i-know-which-method-of-cross-validation-is-best
## 


#install.packages("caret")
library(caret)
library(plotly)
library(ggplot2)
library(glmnet)
library(dplyr)
library(plotly)

## Loading the final data set
transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", encoding = "UTF8", header = TRUE)
## Creating a new variable which is age squared
transfer.data$transferage_sq = transfer.data$transferage^2


## creating a vector with selected predictors for transferfee ()
predicting.var = c("transfer.fee", "positions", "appearances", "total.goals", "total.assists", 
               "total.minutes.played", "contract.left.month","transferage",
               "league", "Status", "searchresults","transferage_sq")
## Removing observations where contract lenght is unknown
transfer.data=filter(transfer.data , is.na(contract.left.month) == FALSE) 


##================ 4.1 Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data)) # creates a vector 

## setting seed to enable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data[train.indicator,predicting.var] # selecting observations with a train indicator
test_sample = transfer.data[-train.indicator, predicting.var] # selecting observations without a train indicator



##================ 4.2 Create evaluation function  ================
## creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 4.3 Baseline model: Simple average from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1

get.rmse(test_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 

#create new data frame
train_sample.1<- train_sample %>% 
  select(transfer.fee,league) 
train_sample.1<- train_sample.1%>% 
  mutate(index=1:258)

#creating GGplot for visualisation
p = ggplot(train_sample.1, aes(x = index , y = transfer.fee))+
  geom_segment(aes(x= index, xend=index, y=transfer.fee, yend=estimate_M1), color="red") +
  geom_point(aes(x = index, y = transfer.fee, color = "black"))   +
  geom_line(aes(x = index, y = estimate_M1), color="green", size =1)+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks= element_line(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text=element_text(family="Goudy Old Style"))

gg <- ggplotly(p)  #using plotly to make it interactive
gg


##================ 4.4 Ordinary least square model   ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample)) # generating linear model on training data
summary(Model_2)

estimate_M2 = predict(Model_2, test_sample)

get.rmse(test_sample$transfer.fee, estimate_M2)

##================ 4.5 Lasso model  ================
## Creating matrices with all regressors beacuse the glmnet function only works with matrices
RegressorMatrix_train=model.matrix(~ positions+transferage+
                                    league+
                                     #searchresults+
                                   Status, train_sample)

RegressorMatrix_test=model.matrix(~ positions+transferage+
                                    league+
                                    #searchresults+
                                    Status, test_sample)


## Training Lasso
Model_3 = glmnet(x = RegressorMatrix_train, y = train_sample$transfer.fee)
Model_3

##
# estimate_M3 = predict(Model_3, RegressorMatrix_test)
# estimate_M3


# get.rmse(test_sample$transfer.fee, estimate_M3)

# Calculating RSME for each lambda
lambda_values = Model_3$lambda

performance_Lasso = data.frame()

for (lambda in lambda_values){
  performance_Lasso = rbind(performance_Lasso,
                            data.frame(lambda = lambda,
                                       RMSError = get.rmse(predict(Model_3, RegressorMatrix_test, s = lambda),
                                                       test_sample$transfer.fee)))
}
performance_Lasso

##Visualization of RSME as a function of lamda
ggplot(performance_Lasso, aes(x = lambda, y = RMSError))+
  geom_point() + 
  geom_line() + 
  theme_minimal()

## Identifying lambda with the lowest RMSE
best.lambda = performance_Lasso$lambda[performance_Lasso$RMSError == min(performance_Lasso$RMSError)]

## Coefficients for best models
coef(Model_3, s = best.lambda)

## RMSE for best model
get.rmse(predict(Model_3, RegressorMatrix_test, s=best.lambda), test_sample$transfer.fee)


##================ 4.6 Decision tree   ================
<<<<<<< Updated upstream
# install.packages("rpart")
library("rpart")
Model_4=rpart(formula=transfer.fee~positions+transferage+
                league+Status+contract.left.month+total.goals+appearances, data=train_sample)
Model_4$cptable

##Finding CP value with lowest xerror
alpha <-Model_4$cptable[which.min(Model_4$cptable[,"xerror"]),"CP"]

printcp(Model_4)
summary(Model_4)
##Pruning the tree
prune.Model_4 <- prune(Model_4,alpha)

plot(Model_4)
text(Model_4)

plot(prune.Model_4, uniform=TRUE, branch=0.6, margin=0.05)
text(prune.Model_4, all=TRUE, use.n=TRUE)

##================ 4.6 Random Forest  ================
## Gode link: http://www.listendata.com/2014/11/random-forest-with-r.html
# install.packages("randomForest")
library(randomForest)
set.seed(1)

### More complex attempt
## First, find the best number of trees
Model_5a = randomForest(transfer.fee ~ ., data = train_sample, ntree = 200)
print(Model_5a)
Model_5b = randomForest(transfer.fee ~ ., data = train_sample, ntree = 300)
print(Model_5b)
Model_5c = randomForest(transfer.fee ~ ., data = train_sample, ntree = 500)
print(Model_5c)
Model_5d = randomForest(transfer.fee ~ ., data = train_sample, ntree = 750)
print(Model_5d)
Model_5e = randomForest(transfer.fee ~ ., data = train_sample, ntree =1000) ## gives the lowest residuals
print(Model_5e)
Model_5f = randomForest(transfer.fee ~ ., data = train_sample, ntree =2000)
print(Model_5f)

## Second, find the number of variables in each split with the lowest OOB-error 
mtry = tuneRF(train_sample[-1],train_sample$transfer.fee, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, na.action=na.roughfix)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m) ##the best mtry is 3

## Run the RF with the best numbers of trees and variables
set.seed(1)
Model_5 = randomForest(transfer.fee ~ .,
                       data = train_sample,
                       ntree = 1000,
                       mtry = best.m)
print(Model_5)

estimate_M5 = predict(Model_5, test_sample) # calculating estimate from model 5
get.rmse(test_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample

importance(Model_5) #calculate the importance of the different variables

## the simple way:
Model_5 = randomForest(transfer.fee ~ ., data = train_sample)
print(Model_5)

estimate_M5 = predict(Model_6, test_sample) # calculating estimate from model 5
get.rmse(test_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample

importance(Model_5)

#plot(test_sample$transfer.fee, estimate_M6)
#abline(a=0, b=1.0)
