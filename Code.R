#Reading data
#.Xlsx Format
library(openxlsx)
library(readxl)
data_row=read.xlsx("2017matchdataOraclesElixir_clean.xlsx", 1 , TRUE)

#.Csv Format
data_lol = read.csv("2017clean.csv", sep = ";")
str(data_lol)
summary(data_lol)
subset(data_lol, count(data_lol, vars= "team")>360)

wii=as.data.frame(table(data_lol$result,data_lol$team)/6)
plot(wins)
#Average Damage Dealt by Champion
tapply(data_lol$dmgtochamps, data_lol$champion, mean)

data_lol$KDA = (data_lol$k+data_lol$a)/data_lol$d
summary(data_lol$KDA)
data_lol = subset(data_lol, data_lol$KDA < 8)
hist(data_lol$KDA)

ggplot(data_rowcsv, aes(x=gamelength, fill=result)) + geom_bar(stat = "count") + labs(title="Games length and win rate", x="Game length", y="Wins")+ scale_fill_discrete(name="Win or Lose")

tapply(data_rowcsv$result, data_rowcsv$side, sum)/6

model_1 <- lm(data_rowcsv$dmgshare ~ data_rowcsv$earnedgoldshare)
summary(model_1)
plot(model_1)

model_1 = lm(dmgshare ~ earnedgoldshare, data = data_rowcsv)
summary(model_1)
plot(model_1)

model_2 = lm(k ~ dmgtochamps, data = data_rowcsv)
summary(model_2)
plot(model_2)

model_3 = lm(result ~ . , data = data_rowcsv)
summary(model_3)

cor(data_rowcsv)

datacc=data_rowcsv[complete.cases(data_rowcsv), ]

#Correlation with result
i1 <- sapply(datacc, is.numeric)
y1 <- "result"
x1 <- setdiff(names(datacc)[i1], y1)
cor(datacc[x1], datacc[[y1]])


# - SPLITTING INTO A TRAINING AND TESTING SET

set.seed(100)
library(caTools)

split = sample.split(datacc$result, SplitRatio = 0.5)

train = subset(datacc, split == TRUE)
test = subset(datacc, split == FALSE)

#Now, use logistic regression trained on the training set to predict the dependent variable not.fully.paid using all the independent variables.

mod<-glm(result ~ gdat10+gdat15+fb+teamkills+teamdeaths+ft+teamtowerkills+opptowerkills+fd+teamdragkills+oppdragkills,data=train,family = binomial)
summary(mod)
## 

mod<-glm(result ~ gdat15+teamkills+teamdeaths+ft+teamtowerkills+opptowerkills,data=train,family = binomial)
summary(mod)

testPredict = predict(mod, type="response", newdata=test)
table(test$result, testPredict>0.5)

smp_siz = floor(0.5*nrow(datacc))
set.seed(123)   # set seed to ensure you always have same random numbers generated
train_ind = sample(seq_len(nrow(datacc)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =datacc[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=datacc[-train_ind,] 


### CART Model

library(rpart)
library(rpart.plot)

StevensTree = rpart(result ~ ft+teamtowerkills+opptowerkills, data = train, method="class", minbucket=10) 
#method="class" arg tells us to make classification tree and not regression tree.
#minbucket = 25 limits the tree so that it does not overfit to our training set.We selected a value of 25, but we could pick a smaller or larger value.We will see another way to limit the tree later in this lecture.

#The model can be be represented as a decision tree:
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = test, type = "class") #We need to give type = "class" if we want the majority class predictions.This is like using a threshold of 0.5.

#Now lets assess the accuracy of the model through confusion matrix
cmat_CART<-table(test$result, PredictCART)  #first arg is the true outcomes and the second is the predicted outcomes
cmat_CART

#ROC Curve
library(ggplot2)
library(ROCR)

PredictROC = predict(StevensTree, newdata = test)
head(PredictROC)

pred = prediction(PredictROC[,2], test$result)
perf = performance(pred, "tpr", "fpr")

#and the plot
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

### Random Forest
library(randomForest)
# Build random forest model
StevensForest = randomForest(result ~ gdat15+teamkills+teamdeaths, data = train1, ntree=100, nodesize=25 )

# Convert outcome to factor
train1$result = as.factor(train$result)
test1$result = as.factor(test$result)

StevensForest = randomForest(result ~ ft+teamtowerkills+opptowerkills , data = train1, ntree=100, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = test1)
table(test1$result, PredictForest)


distances <- dist(data_rowcsv,method="euclidean")
airlineClust <- hclust(distances,method="ward.D")

#Then, plot the dendrogram of the hierarchical clustering process: 
plot(airlineClust)

tapply(datacc$dmgtochamps, datacc$champion, mean)

cor(datacc$dmgshare, datacc$earnedgoldshare)
