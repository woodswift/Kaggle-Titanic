# --- Decision Tree without Name, Ticket, Cabin --- #
# --- Score: 0.78469, "0.78469_my_solution_1101v1.csv" --- #
# Load in the R package  
library(rpart)
# Build the decision tree
my_tree_one <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
# Make your prediction using the test set
my_prediction <- predict(my_tree_one, test, type = "class")
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
# Check that your data frame has 418 entries
nrow(my_solution)
# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Decision Tree without Name, Ticket, Cabin; adding control--- #
# --- Score: 0.77990, "0.77990_my_solution_1101v2.csv" --- #
library(rpart)
my_tree_one <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control(minsplit=50,cp=0))
my_prediction <- predict(my_tree_one, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Decision Tree without Name, Ticket, Cabin --- #
# --- New attributes: Family_Size, Title --- #
# --- Without any filling missing spots or replacing 0s --- #
# --- Score: 0.79904, "0.79904_my_solution_1101v3.csv" --- #
library(rpart)
my_tree_one <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train, method="class")
my_prediction <- predict(my_tree_one, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Decision Tree without Name, Ticket, Cabin --- #
# --- New attributes: Family_Size, Title --- #
# --- With all filling missing spots in "Age" "Embarked" and replacing 0s in "Fare" --- #
# --- Score: 0.79426, "0.79426_my_solution_1101v4.csv" --- #
library(rpart)
my_tree_one <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train, method="class")
my_prediction <- predict(my_tree_one, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Decision Tree without Name, Ticket, Cabin --- #
# --- New attributes: Family_Size, Title --- #
# --- With all filling missing spots in "Age" "Embarked" and replacing 0s in "Fare" --- #
# --- Predict missing "Age" spots by using ANOVA --- #
# --- Score: 0.78947, "0.78947_my_solution_1101v5.csv" --- #
library(rpart)
my_tree_one <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train, method="class")
my_prediction <- predict(my_tree_one, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Random Forest without Name, Ticket, Cabin --- #
# --- New attributes: Family_Size --- #
# --- With all filling missing spots in "Age" "Embarked" and replacing 0s in "Fare" --- #
# --- Score: 0.77512, "0.77512_my_solution_1123v1.csv" --- #
# Load in the package
library(randomForest)
# Set seed for reproducibility
set.seed(111)
# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size, data = train, importance = TRUE, ntree = 1000)
# two graphs appear:
# the accuracy plot shows how much worse the model would perform without the included variables.
# So a high decrease (= high value x-axis) links to a high predictive variable.
# The second plot is the Gini coefficient.
# The higher the variable scores here, the more important it is for the model.
varImpPlot(my_forest)
# waiving bugs
# levels(test$Pclass) <- levels(train$Pclass)
# levels(test$Sex) <- levels(train$Sex)
# levels(test$Embarked) <- levels(train$Embarked)
# levels(test$Title) <- levels(train$Title)
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Random Forest without Name, Ticket, Cabin --- #
# --- New attributes: Family_Size --- #
# --- With all filling missing spots in "Age" "Embarked" and replacing 0s in "Fare" --- #
# --- Predict missing "Age" spots by using ANOVA --- #
# --- Score: 0.77990, "0.77990_my_solution_1123v2.csv" --- #
library(randomForest)
set.seed(111)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size, data = train, importance = TRUE, ntree = 1000)
varImpPlot(my_forest)
# waiving bugs
# levels(test$Pclass) <- levels(train$Pclass)
# levels(test$Sex) <- levels(train$Sex)
# levels(test$Embarked) <- levels(train$Embarked)
# levels(test$Title) <- levels(train$Title)
my_prediction <- predict(my_forest, test)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# --- Rweka J48 using raw train and test dataset --- #
# --- Score: 0.76555, "0.76555_my_solution_1122v1.csv" --- #
train <- read.csv("train.csv")
test <- read.csv("test.csv")
library(RWeka)
# Setting factor variables as factors using loops
fs <- c(2,3,5,7,8,9,10,11,12)
for(i in 1:length(fs)){
  train[,i] <- as.factor(train[,i])
}
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- J48(Survived~.,data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka J48 using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.77033, "0.77033_my_solution_1122v2.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- J48(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka J48 using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Factor SibSp, Parch, Family_size --- #
# --- Score: 0.77033, "0.77033_my_solution_1122v3.csv" --- #
library(RWeka)
# Setting factor variables as factor
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)
train$Family_Size <- as.factor(train$Family_Size)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
test$Family_Size <- as.factor(test$Family_Size)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- J48(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka LMT using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.52153, "0.52153_my_solution_1123v3.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- LMT(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka DecisionStump using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.37321, "0.37321_my_solution_1123v4.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- DecisionStump(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka Logistic using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.33014, "0.33014_my_solution_1123v5.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- Logistic(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka SMO using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.74163, "0.74163_my_solution_1122v7.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- SMO(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka IBk using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.74641, "0.74641_my_solution_1122v8.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- IBk(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka LBR using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.68900, "0.68900_my_solution_1122v9.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Setting factor variables as factor
# Because LBR cannot handle numeric attributes
train$Age <- as.factor(train$Age)
train$Fare <- as.factor(train$Fare)
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)
train$Family_Size <- as.factor(train$Family_Size)
test$Age <- as.factor(test$Age)
test$Fare <- as.factor(test$Fare)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
test$Family_Size <- as.factor(test$Family_Size)
# Fitting the model
model <- LBR(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka AdaBoostM1 using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.40670, "0.40670_my_solution_1123v6.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- AdaBoostM1(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka Bagging using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.32536, "0.32536_my_solution_1123v7.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- Bagging(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka LogitBoost using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.60287, "0.60287_my_solution_1123v8.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- LogitBoost(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka MultiBoostAB using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.70813, "0.70813_my_solution_1123v9.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- MultiBoostAB(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka Stacking using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.62679, "0.62679_my_solution_1123v0.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- Stacking(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka JRip using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.77990, "0.77990_my_solution_1125v1.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- JRip(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka OneR using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.23923, "0.23923_my_solution_1125v2.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- OneR(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)

# --- Rweka PART using processed train and test dataset --- #
# --- New attributes: Family_Size, Title --- #
# --- Score: 0.76555, "0.76555_my_solution_1125v3.csv" --- #
library(RWeka)
# Removing any rows with NAs
train <- train[complete.cases(train),]
# Fitting the model
model <- PART(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Size + Title, data=train)
# Making predictions
test$Survived <- predict(model, newdata=test, type="class")
# Saving predictions
write.csv(test[,c("PassengerId", "Survived")], file="my_solution.csv", row.names=FALSE, quote=FALSE)
