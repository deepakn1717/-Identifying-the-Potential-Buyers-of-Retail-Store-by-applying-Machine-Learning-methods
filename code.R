# -Identifying-the-Potential-Buyers-of-Retail-Store-by-applying-Machine-Learning-methods

#data import
salesMade <- read.csv("C:\\Users\\deepa\\Desktop\\Logistic Regression - Problem Statement\\Retail_Case_Study_Data.csv")

#understandng of data
View(salesMade)
class(salesMade)
dim(salesMade)
str(salesMade)
summary(salesMade)

#data Engineering
sum(is.na(salesMade))
salesMade1 <- salesMade[,-1]
table(salesMade1$Sale.Made)
salesMade1$Mens.Merchandise<- as.factor(salesMade1$Mens.Merchandise)
salesMade1$Womens.Merchandise<- as.factor(salesMade1$Womens.Merchandise)
salesMade1$New.Customer<- as.factor(salesMade1$New.Customer)
salesMade1$Visited.Website<- as.factor(salesMade1$Visited.Website)
salesMade1$Sale.Made<- as.factor(salesMade1$Sale.Made)

#subsetting data into test and train
set.seed(88)
library(caTools)
split <- sample.split(salesMade1$Sale.Made,SplitRatio = 0.75)
class(split)
split
salesMadeTrain <- subset(salesMade1,split == TRUE)
salesMadeTest <- subset(salesMade1,split == FALSE)
nrow(salesMade1)
nrow(salesMadeTest)
nrow(salesMadeTrain)
names(salesMade1)

#Logical Regression model
salesLog <- glm( Sale.Made ~ ., data = salesMade1, family = binomial)
summary(salesLog)
salesLog1 <- glm( Sale.Made ~ Months.Since.Last.Buy+Spend.Category+New.Customer+Visited.Website, data = salesMade1, family = binomial)
summary(salesLog1)
predicttrain <- predict(salesLog1, type ="response")
predicttrain
library(ROCR)
ROCRPred <- prediction (predicttrain, salesMade1$Sale.Made)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize = TRUE, print.cutoffs.at = seq(0,1,by=0.1), text.adj = c(-0.2, 1.7))
table(salesMade1$Sale.Made, predicttrain > 0.25)
predicttest <- predict(salesLog, newdata = salesMadeTest, type = "response")
predicttest > 0.25

library(InformationValue)
plotROC(actuals=salesMadeTrain$Sale.Made, predictedScores=predicttrain)
misClassError(salesMadeTest$Sale.Made, predicttest, threshold = 2.5)
Concordance(salesMadeTest$Sale.Made, predicttest)

salesMade2 <- salesMade1
names(salesMade2)

#subsetting data into test and train
set.seed(81)
library(caTools)
split <- sample.split(salesMade2$Sale.Made,SplitRatio = 0.75)
class(split)
split
salesMade2Train <- subset(salesMade2,split == TRUE)
salesMade2Test <- subset(salesMade2,split == FALSE)
nrow(salesMade2Train)
nrow(salesMade2Test)
nrow(salesMade2)

#class(Sales.Made1)
names(salesMade2)

#decision tree
library(rpart)
library(rpart.plot)
churn.rp <- rpart(Sale.Made ~ ., data = salesMade2Train)
churn.rp

plot(churn.rp, margin = 0.1)
text(churn.rp, all=TRUE, use.n = TRUE, pretty = 0)

rpart.plot(churn.rp,tweak = 1.8)

# Make prediction with the help of tree 
predictions <- predict (churn.rp, salesMade2Test, type = "class")
predictions

library(caret)
confusionMatrix(table(predictions, salesMade2Test$Sale.Made))


# Random Forest 

library(randomForest)
View(salesMadeTrain)
str(salesMadeTrain)



churn.rf <- randomForest(Sale.Made ~ . , data = salesMadeTrain)
churn.rf
churn.prediction <- predict(churn.rf, salesMadeTest)

confusionMatrix(table(churn.prediction, salesMadeTest$Sale.Made))

# Get importance
importance    <- importance(churn.rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

plot(churn.rf, ylim=c(0,0.36))
