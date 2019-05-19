# To learn and demostrate how to pre and post prune decistion tree to avoid overfitting issue
# codes taken from Dzone.com 

#install.packages("rpart")
library(rpart)
library(rpart.plot)
hr_data <- read.csv("HR_comma_sep.csv")

# split data set to 70% Train and 30% Test
sample_ind <- sample(nrow(hr_data),nrow(hr_data)*0.70)
train <- hr_data[sample_ind,]
test <- hr_data[-sample_ind,]

# Rpart has default parameters and value (with we can hypertune next time)
# CP refers to the complexity parameter which we use to control the tree growth
# If cost* of adding a veriable is higher than the value of CP, tree stops growth

#Base Model
hr_base_model <- rpart(left ~ ., data = train, method = "class",
                       control = rpart.control(cp = 0))
summary(hr_base_model)
#Plot Decision Tree
rpart.plot(hr_base_model)
# Examine the complexity plot
printcp(hr_base_model)
plotcp(hr_base_model)

# Evaluation: 
# The printcp and plotcp provide the cross validation error (Xerror) for each nsplit
# can be used to prune the tree. 
# the one with least Xerror is the optimal value of CP given by printcp function
# The use of this plot is described in the post pruning section. 
# look at plotcp. lowest Xerror is when the size of tree is approx at 12. 

# Compute the accuracy of the pruned tree
test$pred <- predict(hr_base_model, test, type = "class")
base_accuracy <- mean(test$pred == test$left)

## PrePrune

# Grow a tree with minsplit of 100 and max depth of 8
hr_model_preprun <- rpart(left ~ ., data = train, method = "class", 
                          control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
# Compute the accuracy of the pruned tree
test$pred <- predict(hr_model_preprun, test, type = "class")
accuracy_preprun <- mean(test$pred == test$left)

## Post Prune

# Prune the hr_base_model based on the optimal cp value
hr_model_pruned <- prune(hr_base_model, cp = 0.0084 )
# Compute the accuracy of the pruned tree
test$pred <- predict(hr_model_pruned, test, type = "class")
accuracy_postprun <- mean(test$pred == test$left)
data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)
