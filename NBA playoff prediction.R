require(caTools)
require(mlbench)
require(caret)
require(corrplot)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(mlbench)
require(RGtk2)
require(randomForest)
require(rpart)
require(data.table)
require(dplyr)
require(mclust)
require(FactoMineR)

all_Data <- read.csv('NBA data base 82-83_16-17.csv')

# Vector of the NBA team names (helps us later)
teamNames = c('Atlanta Hawks', 'Boston Celtics', 'Brooklyn Nets', 'Charlotte Hornets', 'Chicago Bulls',
              'Cleveland Cavaliers', 'Dallas Mavericks', 'Denver Nuggets', 'Detroit Pistons', 'Golden State Warriors',
              'Houston Rockets', 'Indiana Pacers', 'Los Angeles Clippers', 'Los Angeles Lakers', 'Memphis Grizzlies',
              'Miami Heat', 'Milwaukee Bucks', 'Minnesota Timberwolves', 'New Orleans Pelicans', 'New York Knicks',
              'Oklahoma City Thunder', 'Orlando Magic', 'Philadelphia 76ers', 'Phoenix Suns', 'Portland Trail Blazers',
              'Sacramento Kings', 'San Antonio Spurs', 'Toronto Raptors', 'Utah Jazz', 'Washington Wizards')


# Prepare the data.table for the future use
data_preparation <- function(data_table){
  data_table$Team = as.numeric(factor(data_table$Team,levels = teamNames,labels = 1:30))
  data_table <- subset(data_table, select = -c(1))
  return(data_table)
}


season.games <- data_preparation(all_Data)


# Splitting of the training test and the testing set
set.seed(123)
split <- sample.split(season.games$Playoff,SplitRatio = 0.75)
training_set <- subset(season.games,split == TRUE)
testing_set <- subset(season.games,split == FALSE)
training_set_scaled <- training_set
testing_set_scaled <- testing_set
training_set_scaled[,-25] <- scale(training_set[,-25])
testing_set_scaled[,-25] <- scale(testing_set[,-25])
training_set$Playoff <- factor(training_set$Playoff)
testing_set$Playoff <- factor(testing_set$Playoff)


#Predict using decision tree model
decisionTree = rpart(Playoff~., method="class", data=training_set, na.action = na.pass)
plot(decisionTree)
text(decisionTree)
fancyRpartPlot(decisionTree,cex = 0.55)
predictDesicionTree = predict(object = decisionTree, newdata = testing_set, type = "class")
treeConfusionMatrix =  confusionMatrix(data = predictDesicionTree,reference = testing_set$Playoff)
decisionTreeAcuuracy = treeConfusionMatrix$overall['Accuracy']


# Predict using random forest model
randomForest = randomForest(as.factor(training_set_scaled$Playoff) ~., data = training_set_scaled)
predictRandomForest = predict(randomForest, testing_set_scaled[,-25], type="prob")[,2]
binaryRandomForest = ifelse(predictRandomForest > 0.5,1,0)
randomForestAccuracy = mean(binaryRandomForest == testing_set_scaled$Playoff)
randomForest.confusionMatrix = randomForest$confusion
varImpPlot(randomForest)


# Predict using linear regression model
relation <- lm(Playoff ~., data = training_set_scaled);
predictRegression <-  predict(relation, data = testing_set_scaled);
#Binary prediction
predictRegression[predictRegression<0] <- 0;
predictRegression[predictRegression>0] <- 1;
correctPredictions <- predictRegression == testing_set_scaled[,25];
predictionRate = sum(correctPredictions)/length(predictRegression)


# Plots the models accuracy
modelName = c("DecisionTree","RandomForest",
                       "LinearRegression")
modelAccuracy = c(decisionTreeAcuuracy, randomForestAccuracy, predictionRate)*100
modelAccuracy = round(modelAccuracy, digits = 2)
names(modelAccuracy) = modelName
barPlot = barplot(modelAccuracy,main = "Models Accuracy",
             ylim = c(50,100),yinch(0.5), xpd = FALSE,
             xlab = "Model Name",ylab = "Model Accuracy",
             col = c(67:69))
text(barPlot, modelAccuracy,labels = modelAccuracy, pos = 1)