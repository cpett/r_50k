# Packages to install
library(ggplot2)
library(stringr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(caret)
library(e1071)
library(dplyr)
library(fastAdaboost)0.0.
library(LiblineaR)


# Read the csv, and declare the ? as Na
census <- read.csv("./data.csv", na.strings = "?", stringsAsFactors = TRUE)
str(census)
summary(census)
View(census)

# See how many rows have missing
table (complete.cases (census))
# Cleaned data -- no NA, didn't want to impute missing values --> Skew data
myCleanTrain <- census [!is.na (census$workclass) & !is.na (census$occupation), ]
# View(myCleanTrain)
myCleanTrain <- myCleanTrain [!is.na (myCleanTrain$native.country), ]
# View(myCleanTrain)
table(complete.cases(myCleanTrain))

# Exploratory Data Analysis
summary (myCleanTrain$age)
boxplot (age ~ income, data = myCleanTrain, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "salmon")
incomeBelow50K <- (myCleanTrain$income == "<=50K")
summary (myCleanTrain$education.num)
boxplot (education.num ~ income, data = myCleanTrain, 
         main = "Years of Education distribution for different income levels",
         xlab = "Income Levels", ylab = "Years of Education", col = "blue")
nearZeroVar (myCleanTrain[, c("capital.gain", "capital.loss")], saveMetrics = TRUE)
summary (myCleanTrain[ myCleanTrain$income == "<=50K", 
                       c("capital.gain", "capital.loss")])
summary (myCleanTrain$hours.per.week)
boxplot (hours.per.week ~ income, data = myCleanTrain, 
         main = "Hours Per Week distribution for different income levels",
         xlab = "Income Levels", ylab = "Hours Per Week", col = "salmon")
nearZeroVar (myCleanTrain[, "hours.per.week"], saveMetrics = TRUE)

# Correlation between continuous vars
corMat <- cor (myCleanTrain[, c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week")])
diag (corMat) = 0 #Remove self correlations
corMat

# Explore Sex
table (myCleanTrain[,c("sex", "income")])
summary(myCleanTrain$sex)
str(myCleanTrain$sex)

#Visualize income level by sex
ggplot(myCleanTrain, aes(x = sex, fill = factor(income))) +
  geom_bar() +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Income") 

# Education level into ordinal variable (factor)
# Modify the levels to be ordinal
myCleanTrain$education <- ordered (myCleanTrain$education,
                                  levels (myCleanTrain$education) [c(14, 4:7, 1:3, 12, 15, 8:9, 16, 10, 13, 11)])

print (levels (myCleanTrain$education))
summary(myCleanTrain$education)
str(myCleanTrain$education)

# Viz income level by Education
ggplot(myCleanTrain, aes(x = education, fill = factor(income))) +
  geom_bar() +
  xlab("Education") +
  ylab("Total Count") +
  labs(fill = "Income") 

# Visualize Gender vs. Education
ggplot(myCleanTrain, aes(x = education, fill = income)) +
  geom_bar() +
  facet_wrap(~sex) + 
  ggtitle("Sex") +
  xlab("Education") +
  ylab("Total Count") +
  labs(fill = "Income")



set.seed(123)

# Train/Test
split <- createDataPartition(myCleanTrain$income, p = .7, list = FALSE)
trainData <- myCleanTrain[split,]
testData <- myCleanTrain[-split,]

nrow(trainData)
nrow(testData)

# Cross-Validation
ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 5)

#################
# Models
#################
# RandomForest: All vars
# rf.all <- train(income ~ .,
#                 data = trainData,
#                 method = "rf",
#                 trControl = ctrl)
# confusionMatrix(rf.all)
# rf.all.pred <- predict(rf.all, testData)
# rf.all.pred

# RandomForest: Sex, Education
rf.2v <- train(income ~ sex + education,
                data = trainData,
                method = "rf")
confusionMatrix(rf.2v)
rf.2v.pred <- predict(rf.2v, testData)
rf.2v.pred
confusionMatrix(data = rf.2v.pred, testData$income)
# RandomForest: Sex, Education, cap gains/loss
rf.4v <- train(income ~ sex + education + capital.gain + capital.loss,
               data = trainData,
               method = "rf")
confusionMatrix(rf.4v)
rf.4v.pred <- predict(rf.4v, testData)
rf.4v.pred
confusionMatrix(data = rf.4v.pred, testData$income)
# RandomForest: Sex, Education, cap gains/loss, workclass
rf.5v <- train(income ~ sex + education + workclass + capital.gain + capital.loss,
               data = trainData,
               method = "rf")
confusionMatrix(rf.5v)
rf.5v.pred <- predict(rf.5v, testData)
rf.5v.pred
confusionMatrix(data = rf.5v.pred, testData$income)


# # Build Model
# # Boosting with Cross Validation
# set.seed (32323)
# trCtrl = trainControl (method = "cv", number = 10)
# 
# boostFit = train (income ~ age + workclass + education + education.num +
#                     marital.status + occupation + relationship +
#                     race + capital.gain + capital.loss + hours.per.week +
#                     native.country, trControl = trCtrl, 
#                   method = "gbm", data = myCleanTrain, verbose = FALSE)
# confusionMatrix (myCleanTrain$income, predict (boostFit, myCleanTrain))
# 
# # Validate Model
# myCleanTest$predicted = predict (boostFit, myCleanTest)
# confusionMatrix (myCleanTest$income, myCleanTest$predicted)

