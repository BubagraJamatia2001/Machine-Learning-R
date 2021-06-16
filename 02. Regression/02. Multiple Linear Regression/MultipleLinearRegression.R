# Multiple Linear Regression

#-----------Data Preprocessing--------------

# Importing the dataset
dataset = read.csv('50_Startups.csv')

#Encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'), 
                       labels = c(1, 2, 3)
                       )
              

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#--------------------------------------------

# Fitting Multiple Linear Regression to the Training Set
regressor = lm(formula = Profit ~ . ,
               data = training_set
               )

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Building the optimal model using Backward Elimination
regressor2 = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set
               )

summary(regressor2)

regressor2 = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = training_set
                )

summary(regressor2)

regressor2 = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = training_set
                )

summary(regressor2)

regressor3 = lm(formula = Profit ~ R.D.Spend,
                data = training_set
)

summary(regressor3)

# Automated version of Backward Elimination
backwardElimination <- function(x , sl, mlr){
  numVars = length(coef(summary(mlr))[,1])-1
  for(i in c(1:numVars)){
    mlr = lm(formula = Profit ~ . ,data=x)
    maxVal =  max(coef(summary(mlr))[c(2:numVars), "Pr(>|t|)"])
    if(maxVal > sl){
      j = which(coef(summary(mlr))[2:numVars, 4]==maxVal)
      x=x[,-j]
      numVars= numVars-1
    }
    else 
      break
  }
  return(summary(mlr))
}

sl = 0.05
mlr = lm(formula = Profit ~ ., data = training_set)
backwardElimination(training_set, sl, mlr)