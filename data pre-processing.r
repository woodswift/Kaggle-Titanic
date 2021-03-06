library(Hmisc)

getFamily_Size <- function(data){
  data$Family_Size <- data$SibSp + data$Parch
  return(data$Family_Size)
}

getTitle <- function(data){
  title.dot <- regexpr("\\,[A-Z ]{1,20}\\.",data$Name,T)
  title.comm <- title.dot + attr(title.dot,"match.length") -1
  data$Title <- substr(data$Name,title.dot+2,title.comm -1)
  return(data$Title)
}

getFare <- function(data){
  data$Fare[data$Fare == 0] <- NA
  data$Fare <- replaceByMedian(data$Fare,data$Pclass,levels(data$Pclass))
  return(data$Fare)
}

replaceByMedian <- function(Missing,Filter,missCategory){
  for( level in missCategory){
    Missing[which(Filter == level)] <- impute(Missing[which(Filter == level)])
    #the impute function fill the NA automatically
  }
  return(Missing)
}

cTitleAgeRelation <- function(data){
  options(digits=2)
  data$Title <- as.factor(getTitle(data))
  bystats(data$Age,data$Title,fun = function(x)c(Mean=mean(x),Median=median(x)))
}

# --- main --- #
# --- start processing train.csv --- #
raw.train <- read.csv("train.csv",header =T)

raw.train$Survived <- as.factor(raw.train$Survived)
raw.train$Pclass <- as.factor(raw.train$Pclass)
# show a summary
summary(raw.train)

# add new attribute "Family_Size"
raw.train$Family_Size <- getFamily_Size(raw.train)

# add new attribute "Title"
raw.train$Title <- getTitle(raw.train)
raw.train$Title <- factor(raw.train$Title)

# fill in the missing spots in "Age" based on median age of each "Title"
cTitleAgeRelation(raw.train)
train.missTitle <- c("Dr","Master","Miss","Miss","Mr","Mrs")
raw.train$Age <- replaceByMedian(raw.train$Age,raw.train$Title,train.missTitle)

# alternative way to predict the missing "Age" spots using ANOVA
# library(rpart)
# predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Family_Size + Title, data = raw.train[!is.na(raw.train$Age),], method = "anova")
# raw.train$Age[is.na(raw.train$Age)] <- predict(predicted_age, raw.train[is.na(raw.train$Age),])

# replace 0 in "Fare" with median fare of each "Pclass"
raw.train$Fare <- getFare(raw.train)

# fill "S"  in the missing spots in "Embarked"
raw.train[raw.train$Embarked=="","Embarked"] <- "S" 
raw.train$Embarked <- factor(raw.train$Embarked)

train.attrName <- c("PassengerId","Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Family_Size","Title")
train <- raw.train[,train.attrName]
# --- End processing train.csv --- #

# --- start processing test.csv --- #
raw.test <- read.csv("test.csv",header =T)

raw.test$Pclass <- as.factor(raw.test$Pclass)
# show a summary
summary(raw.test)

# add new attribute "Family_Size"
raw.test$Family_Size <- getFamily_Size(raw.test)

# add new attribute "Title"
raw.test$Title <- getTitle(raw.test)
# two special "Title" Dona -> Don/ Ms -> Mr
raw.test[raw.test$Title=="Dona","Title"]<- "Don"
raw.test[raw.test$Title=="Ms","Title"]<- "Mr"
raw.test$Title <- factor(raw.test$Title)

# fill in the missing spots in "Age" based on median age of each "Title"
cTitleAgeRelation(raw.test)
test.missTitle <- c("Master","Miss","Mr","Mrs")
raw.test$Age <- replaceByMedian(raw.test$Age,raw.test$Title,test.missTitle)

# alternative way to predict the missing "Age" spots using ANOVA
# library(rpart)
# predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Family_Size + Title, data = raw.test[!is.na(raw.test$Age),], method = "anova")
# raw.test$Age[is.na(raw.test$Age)] <- predict(predicted_age, raw.test[is.na(raw.test$Age),])

# replace 0 in "Fare" with median fare of each "Pclass"
raw.test$Fare <- getFare(raw.test)

# fill "S"  in the missing spots in "Embarked"
raw.test[raw.test$Embarked=="","Embarked"] <- "S" 
raw.test$Embarked <- factor(raw.test$Embarked)

test.attrName <- c("PassengerId","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Family_Size","Title")
test <- raw.test[,test.attrName]
