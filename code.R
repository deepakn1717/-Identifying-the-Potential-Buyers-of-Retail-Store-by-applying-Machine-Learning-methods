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
