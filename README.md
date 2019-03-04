# Predicting-play-type-probability-in-nfl-games
#Title: "Predicting Playtype greatest probability in NFL Games"
#Team : "BHAGYA and PHIL"
#submission Date : 03-04-2019
#output : html_notebook
# Description Of the Variables.
- week: week within the regular season (1-17).  
- season_year: 2009-2015.  
- month: September - January.  
- drive_id: drive (possession) number within each game.  
- quarter: quarter within each game (1Q, 2Q, 3Q, 4Q, OT).  
- time: time left in seconds to play in each game (1-4500)[^3].  
- pos_team: team with ball possession/offense (32 teams).  
- pos_division: team with ball possession/offense division (8 divisions).  
- def_team: team defending (32 teams).  
- def_division: team defending division (8 divisions).  
- home: dummy variable to indicate if the team with ball possession is playing at home.  
- yardline: ball location on the field (1-99).  
- down: play down (1-4).  
- yards_to_go: yards left for first down (1-48).  
- play_type: pass or rush.  
- play_direction: 12 possible values (e.g., deep left, right tackle).  
- game_time: dummy variable to indicate if the game is played on regular time or prime time[^4].  
- yards: number of yards gained on play.  
#connecting to SQL server
```{r}
library(RMySQL)
con = dbConnect(MySQL(),
                user = 'root', password ="",
                dbname = 'nfl', host = 'localhost')
result = dbSendQuery(con,"SELECT*FROM nflgames")

```
```{r}
nflgames = fetch(result, n = -1)
print(nflgames)
```
##re-order data
```{r}
nflgames <- nflgames[, c(1,2,5,7,8,10,11,16,18,3,4,6,9,12:15,17)]
print(nflgames)
```
#data types
```{r}
nflgames$week <- as.integer(nflgames$week)
nflgames$season_year <- as.integer(nflgames$season_year)
nflgames$month <- as.integer(nflgames$month)
nflgames$drive_id <- as.integer(nflgames$drive_id)
nflgames$quarter <-as.numeric(nflgames$quarter)
nflgames$home <- as.integer(nflgames$home)
nflgames$play_type <- as.integer(nflgames$play_type)
nflgames$yards <- as.numeric(nflgames$yards)
print(str(nflgames))
```
#correlation
```{r}
library(corrplot)

numeric.var <- sapply(nflgames, is.numeric)

corrplot(cor(nflgames[,numeric.var]), method = "number", number.cex = 0.7)
```
#split the data
```{r}
library(caTools)
set.seed(100)
split <- sample.split(nflgames$play_type,SplitRatio = 0.8)
training_set<- subset(nflgames,split==TRUE)
test_set<- subset(nflgames,split==FALSE)
```
##logistic model 
##Creating a new variable "Result" to convert playtype to a factor. 0 represents pass and 1 represents Run
```{r}
#nflgames$result <- ifelse(nflgames$play_type ==1,0,1)
#nflgames
```
```{r}
training_set$result <- ifelse(training_set$play_type ==1,0,1)
test_set$result <- ifelse(test_set$play_type ==1,0,1)
```
# (<0.5 prob)
```{r}
classfier <- glm(formula =result~month+drive_id+time+home+yardline+down+yards_to_go+yards,family = "binomial" ,data= training_set)
prob_pred <- predict(classfier,type = 'response',newdata = test_set[-19])
y_pred <- ifelse(prob_pred<0.50,0,1)
#y_pred
cm<- table(y_pred,test_set$result)
cm
```


#decision tree (<0.5 prob)
```{r}
library(rpart)
d <- rpart(formula =result~month+drive_id+time+home+yardline+down+yards_to_go+yards, data=training_set)
d_pred<- predict(d,newdata = test_set[-19],type="matrix")
d1_pred <- ifelse(d_pred<0.50,0,1)
#d1_pred
cm<- table(d1_pred,test_set$result)
cm
```

#svm
library(kernlab)
library(class)
library(e1071)

c <- svm(formula=result~month+drive_id+time+home+yardline+down+yards_to_go, data=training_set, type="C-classification",kernel= "radial")
s_pred<- predict(c,newdata = test_set[-9])
s_pred
cm<- table(s_pred,test_set$result)
cm
# finding the accuracy using logistic
#Extract values for training data 
```{r}
resultTrain = unlist(training_set[19])
monthTrain = unlist(training_set[10])
drive_idTrain = unlist(training_set[11])
timeTrain = unlist(training_set[12])
homeTrain = unlist(training_set[13])
yardlineTrain = unlist(training_set[14])
downTrain = unlist(training_set[15])
play_typetrain =unlist(training_set[16])
yardstrain = unlist(training_set[18])
```
```{r}
LogisticModel <- glm(result ~ month+drive_id+time+home+yardline+down+yards_to_go+yards, data=training_set)
summary(LogisticModel)
```
#Extract values for Testing Data
```{r}
resultTest = unlist(test_set[19])
monthTest = unlist(test_set[10])
drive_idTest = unlist(test_set[11])
timeTest = unlist(test_set[12])
homeTest = unlist(test_set[13])
yardlineTest = unlist(test_set[14])
downTest = unlist(test_set[15])
yards_to_goTest = unlist(test_set[16])
yardsTest =unlist(test_set[18])
```
##Store the data into a dataframe
```{r}
mainData <- data.frame(result=resultTest , down=downTest , month=monthTest, drive_id=drive_idTest, time=timeTest , home=homeTest , yardline=yardlineTest , yards_to_go=yards_to_goTest , yards=yardsTest)
```
#Obtain predictions
```{r}
predictionsMain <- predict(LogisticModel, mainData,type="response")
predictionsMain
```
#Evaluate prediction accuracy
#Use threshold of 0.5 to determine class boundary
```{r}
firstParam = 1
secondParam = 0
finalPreds = as.data.frame(predictionsMain)
#Cycles through the the predictions and uses the first and second parameters for filtering.
#adjusts finalPreds to either 1 or 0 based on classification
for (i in predictionsMain)
{
  #Set threshold at 0.5
  if (i > 0.50)
  {
    predictionsMain[firstParam] <- 1
    secondParam = secondParam + 1;
  }
  else
  {
    predictionsMain[firstParam] <- 0
  }
  firstParam = firstParam + 1
}
finalPreds = as.data.frame(predictionsMain)
targets = as.data.frame(resultTest)
finalPreds
targets
```
#Analyze how many of the test samples were accurate
```{r}
accurate = 0
for (i in 1:42777)
{
  if(finalPreds[i,]==targets[i,])
  {
    accurate = accurate + 1;
  }
}
accuracyRate = accurate/42777
accuracyRate
```
#ANN model (reconnect to RMySQL)
```{r}
library(RMySQL)
con = dbConnect(MySQL(),
                user = 'root', password ="",
                dbname = 'nfl', host = 'localhost')
result = dbSendQuery(con,"SELECT*FROM nflgames")

nflgames = fetch(result, n = -1)
```
#re-order data
```{r}
nflgames <- nflgames[, c(1,2,7,8,10,11,16,18,3,4,5,6,9,12,13,14,15,17)]
print(nflgames)
```

#Creating a new variable "Result" to convert playtype to a factor. 0 represents pass and 1 represents Run

```{r}
library(caTools)
set.seed(100)
split <- sample.split(nflgames$play_type,SplitRatio = 0.8)
training_set<- subset(nflgames,split==TRUE)
test_set<- subset(nflgames,split==FALSE)
training_set$result <- ifelse(training_set$play_type=="0", 0 ,1)
test_set$result<- ifelse(test_set$play_type=="0",0,1)
```
#imp variables
```{r}
training_set <- training_set[, c(9:19)]
test_set <- test_set[, c(9:19)]
```
#removing the play_type Variable
```{r}
training_set$play_type<- NULL
test_set$play_type<- NULL
```
#Feature scaling, because in this package feature scaling is must for optimal results.
```{r}
training_set[-10]<- scale(training_set[-10])
test_set [-10]<- scale(test_set[-10])
```
#extract values for training
```{r}
resultTrain = unlist(training_set[10])
monthTrain = unlist(training_set[1])
drive_idTrain = unlist(training_set[2])
quarterTrain = unlist(training_set[3])
timeTrain = unlist(training_set[4])
homeTrain = unlist(training_set[5])
yardlineTrain = unlist(training_set[6])
downTrain = unlist(training_set[7])
yards_to_goTrain = unlist(training_set[8])
yardsTrain = unlist(training_set[9])
```
#extract values for testing
```{r}
resultTest = unlist(test_set[10])
monthTest = unlist(test_set[1])
drive_idTest = unlist(test_set[2])
quaterTest = unlist(test_set[3])
timeTest = unlist(test_set[4])
homeTest = unlist(test_set[5])
yardlineTest = unlist(test_set[6])
downTest = unlist(test_set[7])
yards_to_goTest = unlist(test_set[8])
yardsTest =unlist(test_set[9])
```
#Store the data into a dataframe
```{r}
mainData <- data.frame(down=downTest , result=resultTest , month=monthTest, drive_id=drive_idTest, time=timeTest , home=homeTest , yardline=yardlineTest , yards_to_go=yards_to_goTest ,yards=yardsTest)
```
#connecting to h2o cluster (<0.5 prob)
```{r}
library(h2o)

h2o.init()

mainData.h2o <- as.h2o(mainData, destination_frame = "mainData.h2o")

h2o.nfl.1 <- h2o.deeplearning(x = names(mainData)[-10],
                              y = 'result',
                              training_frame = mainData.h2o,
                              standardize = TRUE,
                              hidden = c(10),
                              nfolds = 10,
                              epochs = 10)
neural.pred <- h2o.predict(h2o.nfl.1, newdata = as.h2o(test_set[-10]))
network.pred <- ifelse(neural.pred<0.5,0,1)
network.pred <- as.vector(network.pred)
network.pred
m <- table(network.pred,test_set$result)
m
```
#connecting to SQL server (rush stats and qb stats)
```{r}
library(RMySQL)
con = dbConnect(MySQL(),
                user = 'root', password ="",
                dbname = 'newnfl', host = 'localhost')
result = dbSendQuery(con,"SELECT*FROM qb")
qb = fetch(result, n = -1)
print(qb)
```
```{r}
library(RMySQL)
con = dbConnect(MySQL(),
                user = 'root', password ="",
                dbname = 'newnfl', host = 'localhost')
result = dbSendQuery(con,"SELECT*FROM rush")
rush = fetch(result, n = -1)
print(rush)

```








