### data camp course ####
# topic: machine learning with tree based models 
# part 2: regression trees
# data set : creditsub - classification of loan defaults 

packages = c("rpart","tidyverse","caret","rpart.plot","Metrics")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

help(package="Metrics")

grade <- read.csv("grade.csv")

# Look/explore the data
str(grade)

# Randomly assign rows to ids (1/2/3 represents train/valid/test)
# This will generate a vector of ids of length equal to the number of rows
# The train/valid/test split will be approximately 70% / 15% / 15% 
set.seed(1)
assignment <- sample(1:3, size = nrow(grade), prob = c(0.7,0.15,0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
grade_train <- grade[assignment == 1, ]    # subset the grade data frame to training indices only
grade_valid <- grade[assignment ==2, ]  # subset the grade data frame to validation indices only
grade_test <- grade[assignment == 3, ]   # subset the grade data frame to test indices only

# Train the model
grade_model <- rpart(formula = final_grade ~ ., 
                     data = grade_train, 
                     method = "anova")

# Look at the model output                      
print(grade_model)

# Plot the tree model
rpart.plot(x = grade_model, yesno = 2, type = 0, extra = 0)

# Generate predictions on a test set
pred <- predict(object = grade_model,   # model object 
                newdata = grade_test)  # test dataset

table(pred)

# Compute the RMSE
rmse(actual = grade_test$final_grade, 
     predicted = pred)
