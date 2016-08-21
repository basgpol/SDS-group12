##=========================================================================================
##--------------------------------- 4. Prediction Models _---------------------------------
##=========================================================================================

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


install.packages("caret")
library(caret)

## Loading the final data set
transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.tidy.csv", encoding = "UTF8", header = TRUE)

## creating a vector with selected predictors for transferfee ()
predictors = c(transfer.data$positions, transfer.data$nationality, transfer.data$appearances,
               transfer.data$total.goals, transfer.data$total.assists, transfer.data$minutes.pr.goal, 
               transfer.data$total.minutes.played, transfer.data$contract.left.month, 
               transfer.data$transferage, transfer.data$league, transfer.data$Status, transfer.data$searchresults)


##================ 4.1 Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data)) # creates a vector 

## setting seed to inable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data[train.indicator, ] # selecting observations with a train indicator
test_sample = transfer.data[-train.indicator, ] # selecting observations without a train indicator


##================ 4.2 Create evaluation function  ================
## creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 4.3 Baseline model: Simple average from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1
get.rmse(test_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 


##================ 4.4 XXXXX  ================