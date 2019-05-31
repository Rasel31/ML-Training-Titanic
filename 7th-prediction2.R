setwd("D:/ML - R (TITANIC) - Raqueeb")
combined_set <- read.csv("D:/ML - R (TITANIC) - Raqueeb/combined_set.csv")

train <- combined_set[1:891,]
test <- combined_set[892:1309,]

library(party)

set.seed(291)

fit2 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

# fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID,
#               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

# Now let's make a prediction and write a submission file

MyPredict <- predict(fit2, test, OOB=TRUE, type = "response")
predict7th <- data.frame(PassengerId = test$PassengerId, Survived = MyPredict)
write.csv(predict7th, file = "tree2.csv", row.names = FALSE)

?cforest
