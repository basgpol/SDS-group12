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




##================ 4.1 Cross validation: Dividing into a train and test sample  ================



##================ 4.2 Create evaluation function  ================
## creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real-estimate)^2)))
}

##================ 4.3 XXXXX  ================

##================ 4.4 XXXXX  ================