# -Identifying-the-Potential-Buyers-of-Retail-Store-by-applying-Machine-Learning-methods

#librs
library(caTools)
library(randomForest)
library(caret)
library(dplyr)
library(ggthemes)
#data import
salesMade <- read.csv("C:\\Users\\deepa\\Desktop\\Logistic Regression - Problem Statement\\Retail_Case_Study_Data.csv")

#understandng of data
head(salesMade)
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

#Spending category updates
table(salesMade1$Spend.Category)
mean(salesMade1$Spend.Numeric)
salesMade1$Spend.Category <- as.character(salesMade1$Spend.Category)
str(salesMade1$Spend.Category)
salesMade1$Category[salesMade1$Spend.Category == "1) $0 - $100"] <- "low"
salesMade1$Category[salesMade1$Spend.Category == "2) $100 - $200"] <- "low"
salesMade1$Category[salesMade1$Spend.Category == "3) $200 - $350"] <- "mid"
salesMade1$Category[salesMade1$Spend.Category == "4) $350 - $500"] <- "mid"
salesMade1$Category[salesMade1$Spend.Category == "5) $500 - $750"] <- "high"
salesMade1$Category[salesMade1$Spend.Category == "6) $750 - $1,000"] <- "high"
salesMade1$Category[salesMade1$Spend.Category == "7) $1,000 +"] <- "high"
table(salesMade1$Category)
salesMade1$Category <- as.factor(salesMade1$Category)
salesMade1$Spend.Category <- as.factor(salesMade1$Spend.Category)
sum(is.na(salesMade1))

#subsetting data into test and train
set.seed(88)
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
head(salesMadeTrain)
str(salesMadeTrain)
sales.rf <- randomForest(Sale.Made ~ . , data = salesMadeTrain)
sales.rf
sales.prediction <- predict(sales.rf, salesMadeTest)
confusionMatrix(table(sales.prediction, salesMadeTest$Sale.Made))

# Get importance
importance    <- importance(sales.rf)
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

plot(sales.rf, ylim=c(0,0.36))

