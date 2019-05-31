# Set working directory and import datafiles
# Your working directory might vary

getwd()

setwd("D:/ML - R (TITANIC) - Raqueeb")
train <- read.csv("D:/ML - R (TITANIC) - Raqueeb/train.csv")
test <- read.csv("D:/ML - R (TITANIC) - Raqueeb/test.csv")

# Can we see a name?
train$Name
train$Name[1]
# We need to add test and train sets for feature engineering

test$Survived <- NA
combined_set <- rbind(train, test)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Child, Adult  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Creating new variable Child and Adult

combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'

# Show counts
table(combined_set$Child, combined_set$Survived)
prop.table(table(combined_set$Child, combined_set$Survived),1) * 100

# Convert to a factor
combined_set$Child <- factor(combined_set$Child)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Cabin  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

combined_set$Cabin <- as.character(combined_set$Cabin)
strsplit(combined_set$Cabin[2], NULL)[[1]]

combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

summary(combined_set$Cabin)

# Convert to a factor
combined_set$Cabin <- factor(combined_set$Cabin)

prop.table(table(combined_set$Deck,combined_set$Survived),1) * 100

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  fare_type  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                 
combined_set$Fare_type[combined_set$Fare<50]<-"low"
combined_set$Fare_type[combined_set$Fare>50 & combined_set$Fare<=100]<-"med1"
combined_set$Fare_type[combined_set$Fare>100 & combined_set$Fare<=150]<-"med2"
combined_set$Fare_type[combined_set$Fare>150 & combined_set$Fare<=500]<-"high"
combined_set$Fare_type[combined_set$Fare>500]<-"vhigh"

aggregate(Survived ~ Fare_type, data=combined_set, mean) 
prop.table(table(combined_set$Fare_type, combined_set$Survived), 1) * 100

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Title  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Convert to a string
combined_set$Name <- as.character(combined_set$Name)

# What's in a name, again?
combined_set$Name[1]

# Find the indexes for the title piece of the name
strsplit(combined_set$Name[1], split='[,.]')
strsplit(combined_set$Name[1], split='[,.]')[[1]]
strsplit(combined_set$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title
combined_set$Title <- strsplit(combined_set$Name, split='[,.]')[[1]][2]  # Won't work!
combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_set$Title <- sub(' ', '', combined_set$Title)

# Inspect new feature
table(combined_set$Title)

combined_set$Name[combined_set$Title == 'Rev']

# Combined_setne small title groups
combined_set$Title[combined_set$Title %in% c('Ms')] <- 'Miss'
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_set$Title[combined_set$Title %in% c('Col','Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle','Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

table(combined_set$Title)

# Convert to a factor
combined_set$Title <- factor(combined_set$Title)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  MOther  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18 & combined_set$Title == 'Mrs'] <- 'Mother'

# Show counts
table(combined_set$Mother, combined_set$Survived)

# Convert to a factor
combined_set$Mother <- factor(combined_set$Mother)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Family Size  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Engineered variable: Family size
combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1

table(combined_set$FamilySize, combined_set$Survived)
prop.table(table(combined_set$FamilySize, combined_set$Survived), 1) * 100

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Family ID  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Engineered variable: Family
combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined_set$FamilyID <- paste(as.character(combined_set$FamilySize), combined_set$Surname, sep="")

table(combined_set$FamilyID)

combined_set$FamilyID <- factor(combined_set$FamilyID)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Family Size Group  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

combined_set$FamilySizeGroup[combined_set$FamilySize <= 2] <- 'Single'
combined_set$FamilySizeGroup[combined_set$FamilySize < 5 & combined_set$FamilySize > 2] <- 'Small'
combined_set$FamilySizeGroup[combined_set$FamilySize > 4] <- 'Large'

# a new plot
mosaicplot(table(combined_set$FamilySizeGroup, combined_set$Survived), main='Survival affected by Family Size ', shade=TRUE)

# Inspect new feature
table(combined_set$FamilySizeGroup)
table(combined_set$FamilySizeGroup, combined_set$Survived)
prop.table(table(combined_set$FamilySizeGroup, combined_set$Survived), 1) * 100

# Convert to a factor
combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Split Data Set  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Split back into test and train sets
train <- combined_set[1:891,]
test <- combined_set[892:1309,]
                                
# Install and load required packages for fancy decision tree plotting

install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySizeGroup,
             data=train, method="class")

fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
prediction_5th <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction_5th)
write.csv(submit, file = "prediction5th.csv", row.names = FALSE)
