# Set working directory and import datafiles
# Your working directory might vary

setwd("D:/ML - R (TITANIC) - Raqueeb")
train <- read.csv("D:/ML - R (TITANIC) - Raqueeb/train.csv")
test <- read.csv("D:/ML - R (TITANIC) - Raqueeb/test.csv")

# We need to Install + load packages for decision trees and random forests

library(rpart)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combined_set <- rbind(train, test)

# Convert to a string
combined_set$Name <- as.character(combined_set$Name)

# Engineered variable: Title
combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_set$Title <- sub(' ', '', combined_set$Title)

# Combine small title groups
combined_set$Title[combined_set$Title %in% c('Ms')] <- 'Miss'
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_set$Title[combined_set$Title %in% c('Col','Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle','Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Convert to a factor
combined_set$Title <- factor(combined_set$Title)

# Mother
combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18 & combined_set$Title == 'Mrs'] <- 'Mother'
combined_set$Mother <- factor(combined_set$Mother)

# Child

combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'
combined_set$Child <- factor(combined_set$Child)

# Cabin

combined_set$Cabin <- as.character(combined_set$Cabin)
strsplit(combined_set$Cabin[2], NULL)[[1]]
combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1])) 
combined_set$Cabin <- factor(combined_set$Cabin)

# Engineered variable: Family size
combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1

# Engineered variable: Family
combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined_set$FamilyID <- paste(as.character(combined_set$FamilySize), combined_set$Surname, sep="")

combined_set$FamilyID[combined_set$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combined_set$FamilyID)
# Removing all erroneous family IDs
famIDs <- data.frame(table(combined_set$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combined_set$FamilyID[combined_set$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combined_set$FamilyID <- factor(combined_set$FamilyID)

combined_set$FamilySizeGroup[combined_set$FamilySize == 1] <- 'single'
combined_set$FamilySizeGroup[combined_set$FamilySize < 5 & combined_set$FamilySize > 1] <- 'Smaller'
combined_set$FamilySizeGroup[combined_set$FamilySize > 4] <- 'large'

# Convert to a factor
combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

# Fill in Age NAs
summary(combined_set$Age)

FillAge <- rpart(Age ~ Pclass + Mother + FamilySize + Sex + SibSp + Parch + Deck + Fare + Embarked + Title + FamilyID + FamilySizeGroup + FamilySize, 
                data=combined_set[!is.na(combined_set$Age),], method="anova")

combined_set$Age[is.na(combined_set$Age)] <- predict(FillAge, combined_set[is.na(combined_set$Age),])

summary(combined_set)

# Fill in Embarked blanks
summary(combined_set$Embarked)
which(combined_set$Embarked == '')
combined_set$Embarked[c(62,830)] = "S"
combined_set$Embarked <- factor(combined_set$Embarked)

# Fill in Fare NAs
summary(combined_set$Fare)
which(is.na(combined_set$Fare))
combined_set$Fare[1044] <- median(combined_set$Fare, na.rm=TRUE)

# New factor for new technique , only allowed <32 levels, so reduce number
combined_set$FamilyID2 <- combined_set$FamilyID
# Convert back to string
combined_set$FamilyID2 <- as.character(combined_set$FamilyID2)
combined_set$FamilyID2[combined_set$FamilySize <= 3] <- 'Small'
# And convert back to factor
combined_set$FamilyID2 <- factor(combined_set$FamilyID2)

#once again for both the variable
# Mother
combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18 & combined_set$Title == 'Mrs'] <- 'Mother'
combined_set$Mother <- factor(combined_set$Mother)

# Child

combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'
combined_set$Child <- factor(combined_set$Child)

# Check what else might be missing
summary(combined_set)

# Split back into test and train sets
train <- combined_set[1:891,]
test <- combined_set[892:1309,]

install.packages('mice')
install.packages('lattice')
library('mice')
library('lattice')

md.pattern(combined_set)
# Build a new tree with our new features
dtree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID2,
             data=train, method="class")

# Now let's make a prediction and write a submission file
Prediction6th <- predict(dtree, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction6th)
write.csv(submit, file = "6thprediction.csv", row.names = FALSE)
