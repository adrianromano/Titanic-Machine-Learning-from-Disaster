## Load the dataset
titanicTrain <- read.csv("/Users/adrianromano/R/titanic/train.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
titanicTest <- read.csv("/Users/adrianromano/R/titanic/test.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))

names(titanicTrain)
names(titanicTest)

library(knitr)
table <- data.frame(Variable = c("Survived", "Name", "Pclass", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked"), 
                    Description = c("Indicates whether the Passenger Survived or Died (1 = Survived, 0 = Died)", "Name of the Passenger", "Ticket Class (1 = 1st class, 2 = 2nd class, 3 = 3rd class)", "Gender of the Passenger (Male/Female)", "Age of the Passenger", "Number of Siblings/Spouse aboard with the Passenger", "Number of Parents/Children aboard with the Passenger", "Passenger's Ticket Number", "Ticket Price", "Cabin Number", "Port of Embarkation (C = Cherbourg, Q = Queenstown, S = Southampton)"))
kable(table) 

titanicTest$Survived <- "NA"
titanicTrain$Set <- "Train"
titanicTest$Set <- "Test"

titanicFull <- rbind(titanicTrain, titanicTest)
str(titanicFull)
dim(titanicFull)

## Cleaning the Data
colSums(is.na(titanicFull))

# Embarked
table(titanicFull$Embarked)
titanicFull[is.na(titanicFull$Embarked), "Embarked"] <- "S"

# Fare
fareMedian <- median(titanicFull$Fare, na.rm = TRUE)
titanicFull[is.na(titanicFull$Fare), "Fare"] <- fareMedian

titanicFull$FareGroup <- cut(titanicFull$Fare, breaks = 3, labels = c("Low", "Medium", "High"))
table(titanicFull$FareGroup)

## Cabin
head(titanicFull$Cabin)

# Name
head(titanicFull$Name)
length(unique(titanicFull$Name))
titanicFull$Title <- gsub("^.*, (.*?)\\..*$", "\\1", titanicFull$Name)
table(titanicFull$Sex, titanicFull$Title)
titanicFull$Title[titanicFull$Title %in% c("Mlle", "Ms")] <- "Miss" 
titanicFull$Title[titanicFull$Title == "Mme"] <- "Mrs"
titanicFull$Title[titanicFull$Title %in%  c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")] <- "Other"
table(titanicFull$Sex, titanicFull$Title)

# SibSp + Parch
titanicFull$FamilySize <- titanicFull$SibSp + titanicFull$Parch + 1
titanicFull$FamilyGroup[titanicFull$FamilySize == 1] <- "Alone"
titanicFull$FamilyGroup[titanicFull$FamilySize > 1 & titanicFull$FamilySize < 5] <- "Small"
titanicFull$FamilyGroup[titanicFull$FamilySize > 4] <- "Large"

## Ticket
head(titanicFull$Ticket)
length(unique(titanicFull$Ticket))

# Age
boxplot(titanicFull$Age, col = "bisque", main = "Boxplot of Age")
outlierFilter <- titanicFull$Age < boxplot.stats(titanicFull$Age)$stats[5]

ageModel <- lm(Age ~ Pclass + Sex + Fare + FamilyGroup + Embarked + Title, data = titanicFull[outlierFilter,])
newdata <- titanicFull[is.na(titanicFull$Age), c("Pclass", "Sex", "Fare", "FamilyGroup", "Embarked", "Title")]
titanicFull[is.na(titanicFull$Age), "Age"] <- predict(ageModel, newdata)

titanicFull$AgeGroup[titanicFull$Age < 18] <- "Child"
titanicFull$AgeGroup[titanicFull$Age >= 18] <- "Adult"

colSums(is.na(titanicFull))

# Categorical Casting
titanicFull$Pclass <- as.factor(titanicFull$Pclass)
titanicFull$Sex <- as.factor(titanicFull$Sex)
titanicFull$AgeGroup <- as.factor(titanicFull$AgeGroup)
titanicFull$FareGroup <- as.factor(titanicFull$FareGroup)
titanicFull$Embarked <- as.factor(titanicFull$Embarked)
titanicFull$Title <- as.factor(titanicFull$Title)
titanicFull$FamilySize <- as.factor(titanicFull$FamilySize)
titanicFull$FamilyGroup <- as.factor(titanicFull$FamilyGroup)

str(titanicFull)

# Split data back
titanicTrain <- titanicFull[titanicFull$Set == "Train", ]
titanicTest <- titanicFull[titanicFull$Set == "Test", ]
titanicTrain$Survived <- as.factor(titanicTrain$Survived)
str(titanicTrain)

library(dplyr)
library(ggplot2)
library(pander)

## Survived
ggplot(titanicTrain, aes(x = Survived)) +
    geom_bar(stat = "count", aes(fill = Survived), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Number of Survivors") +
    geom_label(stat = "count", aes(label = ..count..)) +
    theme_classic()

## Age
ggplot(titanicTrain, aes(Age)) +
    geom_histogram() +
    labs(x = "Age", y = "Number of Passengers", title = "Number of Passengers based on Age") +
    theme_bw()
 

ggplot(titanicTrain) +
    geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 1) +
    labs(x = "Age", y = "Number of Passengers", title = "Survival Comparison by Age") +
    theme_bw()

## Age Group
agegroupCount <- titanicTrain %>%
    group_by(AgeGroup) %>%
    count(AgeGroup) %>%
    select(AgeGroup, Passengers = n)

pandoc.table(agegroupCount)

ggplot(agegroupCount, aes(x = AgeGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = AgeGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Child vs Adult on board") +
    theme_classic()

agegroupSurvived <- titanicTrain %>%
    group_by(AgeGroup) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(AgeGroup, Passengers = n)

pandoc.table(agegroupSurvived)

agegroupSurvived <- titanicTrain %>%
    group_by(AgeGroup) %>%
    count(Survived) %>%
    select(AgeGroup, Survived, Passengers = n)

ggplot(agegroupSurvived, aes(x = AgeGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = AgeGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Survival of Child vs Adult on board (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Fare
ggplot(titanicTrain, aes(Fare)) +
    geom_density() +
    labs(x = "Fares", y = "Number of Passengers", title = "Number of Passengers based on Fares") +
    theme_bw()

ggplot(titanicTrain) +
    geom_freqpoly(mapping = aes(x = Fare, color = Survived), binwidth = 0.05) +
    scale_x_log10() +
    labs(x = "Fares", y = "Number of Passengers", title = "Number of Survival based on Fares") +
    theme_bw()

## Fare Group
faregroupCount <- titanicTrain %>%
    group_by(FareGroup) %>%
    count(FareGroup) %>%
    select(FareGroup, Passengers = n)

pandoc.table(faregroupCount)

ggplot(faregroupCount, aes(x = FareGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FareGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Passengers based on Fare levels") +
    theme_classic()

faregroupSurvived <- titanicTrain %>%
    group_by(FareGroup) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(FareGroup, Passengers = n)

pandoc.table(faregroupSurvived)

faregroupSurvived <- titanicTrain %>%
    group_by(FareGroup) %>%
    count(Survived) %>%
    select(FareGroup, Survived, Passengers = n)

ggplot(faregroupSurvived, aes(x = FareGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FareGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Number of Survivors based on Fare Levels (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Pclass
classCount <- titanicTrain %>%
    group_by(Pclass) %>%
    count(Pclass) %>%
    select(Pclass, Passengers = n)

pandoc.table(classCount)

ggplot(classCount, aes(x = Pclass, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Pclass), col = "black", alpha = 0.8) +
    labs(x = "Passenger Class", y = "Number of Passengers", title = "Number of Passengers in each passenger class") +
    theme_classic()


classSurvived <- titanicTrain %>%
    group_by(Pclass) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(Pclass, Passengers = n) 

pandoc.table(classSurvived)

classSurvived <- titanicTrain %>%
    group_by(Pclass) %>%
    count(Survived) %>%
    select(Pclass, Survived, Passengers = n)

ggplot(classSurvived, aes(x = Pclass, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Pclass), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Survivors based on their passenger class (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Sex
sexCount <- titanicTrain %>%
    group_by(Sex) %>%
    count(Sex) %>%
    select(Gender = Sex, Passengers = n)

pandoc.table(sexCount)

ggplot(sexCount, aes(x = Gender, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Gender), col = "black", alpha = 0.8) +
    labs(x = "Gender", y = "Number of Passengers", title = "Number of Males vs Females on board") +
    theme_classic()

sexSurvived <- titanicTrain %>%
    group_by(Sex) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(Gender = Sex, Passengers = n)

pandoc.table(sexSurvived)

sexSurvived <- titanicTrain %>%
    group_by(Sex) %>%
    count(Survived) %>%
    select(Gender = Sex, Survived, Passengers = n)

ggplot(sexSurvived, aes(x = Gender, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Gender), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Males vs Females Survival Comparison (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Embarked
embarkedCount <- titanicTrain %>%
    group_by(Embarked) %>%
    count(Embarked) %>%
    select(Port = Embarked, Passengers = n)

pandoc.table(embarkedCount)

ggplot(embarkedCount, aes(x = Port, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Port), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Passengers from each Port of Embarkation") +
    theme_classic()

embarkedSurvived <- titanicTrain %>%
    group_by(Embarked) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(Port = Embarked, Passengers = n)

pandoc.table(embarkedSurvived)

embarkedSurvived <- titanicTrain %>%
    group_by(Embarked) %>%
    count(Survived) %>%
    select(Port = Embarked, Survived, Passengers = n)

ggplot(embarkedSurvived, aes(x = Port, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Port), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Survivors based on the Port of Embarkation (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Title of Names
titleCount <- titanicTrain %>%
    group_by(Title) %>%
    count(Title) %>%
    select(Title, Passengers = n)

pandoc.table(titleCount)

ggplot(titleCount, aes(x = Title, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Title), col = "black", alpha = 0.8) +
    labs(x = "Title", y = "Number of Passengers", title = "Number of Passengers based on the Title of their name") +
    theme_classic()

titleSurvived <- titanicTrain %>%
    group_by(Title) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(Title, Passengers = n)

pandoc.table(titleSurvived)

titleSurvived <- titanicTrain %>%
    group_by(Title) %>%
    count(Survived) %>%
    select(Title, Survived, Passengers = n)

ggplot(titleSurvived, aes(x = Title, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = Title), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Survivors based on the Title of their name (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Family Size
famsizeCount <- titanicTrain %>%
    group_by(FamilySize) %>%
    count(FamilySize) %>%
    select(FamilySize, Passengers = n)

pandoc.table(famsizeCount)

ggplot(famsizeCount, aes(x = FamilySize, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FamilySize), col = "black", alpha = 0.8) +
    labs(x = "Family Size", y = "Number of Passengers", title = "Number of Passengers based on their Family Size") +
    theme_classic()

famsizeSurvived <- titanicTrain %>%
    group_by(FamilySize) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(FamilySize, Passengers = n)

pandoc.table(famsizeSurvived)

famsizeSurvived <- titanicTrain %>%
    group_by(FamilySize) %>%
    count(Survived) %>%
    select(FamilySize, Survived, Passengers = n)

ggplot(famsizeSurvived, aes(x = FamilySize, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FamilySize), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Survivors based on their Family Size (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Family Group
famgroupCount <- titanicTrain %>%
    group_by(FamilyGroup) %>%
    count(FamilyGroup) %>%
    select(FamilyGroup, Passengers = n)

pandoc.table(famgroupCount)

ggplot(famgroupCount, aes(x = FamilyGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FamilyGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Passengers based on their Family Group") +
    theme_classic()

famgroupSurvived <- titanicTrain %>%
    group_by(FamilyGroup) %>%
    count(Survived) %>%
    filter(Survived == 1) %>%
    select(FamilyGroup, Passengers = n)

pandoc.table(famgroupSurvived)

famgroupSurvived <- titanicTrain %>%
    group_by(FamilyGroup) %>%
    count(Survived) %>%
    select(FamilyGroup, Survived, Passengers = n)

ggplot(famgroupSurvived, aes(x = FamilyGroup, y = Passengers)) +
    geom_bar(stat = "identity", aes(fill = FamilyGroup), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Passengers", title = "Number of Survivors based on their Family Group (0 = Died, 1 = Survived)") +
    facet_wrap(~Survived) +
    theme_bw()

## Prediction
library(caret)
library(randomForest)

set.seed(1995)
inTrain <- createDataPartition(titanicTrain$Survived, p = 0.7, list = FALSE)
training <- titanicTrain[inTrain, ]
testing <- titanicTrain[-inTrain, ]

set.seed(1995)
rfModel <- randomForest(Survived ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilyGroup, data = training)
rfPred <- predict(rfModel, newdata = testing)
rfCM <- confusionMatrix(rfPred, testing$Survived)
rfCM$table

accuracy <- rfCM$overall[1]
accuracy

prediction <- predict(rfModel, newdata = titanicTest)

titanicPrediction <- data.frame(PassengerId = titanicTest$PassengerId, Survived = prediction)
write.csv(titanicPrediction, file = "TitanicPrediction.csv", row.names = FALSE)
