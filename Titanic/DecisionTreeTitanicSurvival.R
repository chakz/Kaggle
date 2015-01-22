# To identify what types of people are likely to survive. We apply the tools of Machine Learning
# to predict which passengers survived the tragedy

# Training Data
TrainTitanic <- read.csv("train.csv")

# Testing Data
TestTitanic <- read.csv("test.csv")

# Testing the first hypothesis method: Decision Trees using CARTs

library("rpart")

# Training the decision tree classifier
TrainTitanic$Survived <- as.factor(TrainTitanic$Survived)
titanic.rpart <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, data = TrainTitanic)

# Predicting the classes for the Titanic Decision Tree
TestTitanic$Survived <- predict(titanic.rpart, newdata = TestTitanic[,c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")] ,type = "class")

TestTitanic <- TestTitanic[,c(1, 12, 2:11)]

result <- TestTitanic[,c(1:2)]
write.csv(result, file = "TitanicPredictionSurvived.csv", row.names = FALSE)
