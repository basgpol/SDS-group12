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


#install.packages("caret")
library(caret)
library(plotly)
library(ggplot2)

## Loading the final data set
transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.tidy.csv", encoding = "UTF8", header = TRUE)

## creating a vector with selected predictors for transferfee ()
predicting.var = c("transfer.fee", "positions", #"nationality", 
                   "appearances", "total.goals", "total.assists", 
               "minutes.pr.goal", "total.minutes.played", "contract.left.month", "transferage",
               "league", "Status", "searchresults")


##================ 4.1 Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data)) # creates a vector 

## setting seed to inable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data[train.indicator, predicting.var] # selecting observations with a train indicator
test_sample = transfer.data[-train.indicator, predicting.var] # selecting observations without a train indicator



##================ 4.2 Create evaluation function  ================
## creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 4.3 Baseline model: Simple average from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1
get.rmse(test_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 


##================ 4.4 Ordinary least square model   ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample) # generating linear model on training data
summary(Model_2)

estimate_M2 = predict(Model_2, test_sample)

get.rmse(test_sample$transfer.fee, estimate_M2)



<<<<<<< HEAD
=======
get.rmse(test_sample$transfer.fee, estimate_M1)
>>>>>>> origin/master


<<<<<<< HEAD

=======
#create new data frame
train_sample.1<- train_sample %>% 
  select(name,transfer.fee,Status,club.to) 
train_sample.1<- train_sample.1%>% 
  mutate(index=1:496)

#creating GGplot
p = ggplot(train_sample.1, aes(x = index , y = transfer.fee))+
  geom_segment(aes(x= index, xend=index, y=transfer.fee, yend=estimate_M1), color="red") +
  geom_point(aes(x = index, y = transfer.fee,text = paste(name, " to ", club.to)), color = "black")   +
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

#adding theme
gg+theme(axis.title.x=element_blank(),
         axis.text.x =element_text(size  = 7,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1),
         axis.ticks= element_line(color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.title.y=element_blank(),
         text=element_text(family="Goudy Old Style"))
##================ 4.4 XXXXX  ================>>>>>>> origin/master
