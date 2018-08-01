### data camp course ####
# topic: machine learning with tree based models 
# part 1 classification trees
# data set : creditsub - classification of loan defaults 

packages = c("rpart","tidyverse","caret")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

credit_sub <- read.csv("credit.csv")

### data spliting 

n <- nrow(credit_sub)
n_train <- round(n*0.8)

### training set
set.seed(1234)
train_indices <- sample(1:n,n_train)

credit_train <- credit_sub[train_indices,]
credit_test <- credit_sub[-train_indices,]



# Train a gini-based model
credit_model1 <- rpart(formula = default ~ ., 
                       data = credit_train, 
                       method = "class",
                       parms = list(split ='gini'))

# Train an information-based model
credit_model2 <- rpart(formula = default ~ ., 
                       data = credit_train, 
                       method = "class",
                       parms = list(split = 'information'))

# Generate predictions on the validation set using the gini model
pred1 <- predict(object = credit_model1, 
                 newdata = credit_test,
                 type = "class")    

# Generate predictions on the validation set using the information model
pred2 <- predict(object = credit_model2, 
                 newdata = credit_test,
                 type = "class")

# Compare classification error
ce(actual = credit_test$default, 
   predicted = pred1 )

ce(actual = credit_test$default, 
   predicted = pred2)  

