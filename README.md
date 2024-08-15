# Final-Project

library(readxl)
library(rpart)
library(maptree)
library(partykit)
library(xlsx)


# load data in
data <- read_excel("/Users/jordanjohnson/Desktop/STAT 551 - Predictive Analytics/LendingClub2021.xlsx")

# summary
summary(data)
head(data)
str(data)
names(data)

# plot of target vs monthly income
boxplot(MonthlyIncome~target, data=data,col="darkblue", main = "Good/Bad Loan vs
     Monthly Income", ylab="Monthly Income (US Dollars)",
     xlab="0 = Good Loan            1 = Bad Loan")

boxplot(OpenCREDITLines~target,data=data)
plot(data$OpenCREDITLines,data$target)

boxplot(TotalCREDITLines~target,data = data,col='darkgreen',
        xlab = "0 = Good Loan           1 = Bad Loan",ylab = "Total Credit Line",
        main="Good/Bad Loan vs Total Credit Line")
plot(data$TotalCREDITLines,data$target)

boxplot(RevolvingCREDITBalance~target,data = data, col="red",
        xlab = "0 = Good Loan           1 = Bad Loan",
        ylab = "Total Credit Card Balance",main="Good/Bad Loan vs Total Credit Card Balance")
plot(data$RevolvingCREDITBalance,data$target)

boxplot(RevolvingLineUtilization~target,data = data,col="red")

# plot of good/bad loan vs loan inquiries
boxplot(Inquiries6M~target,data = data,col='blue', 
        ylab = "Number of Loan Inquiries in Past 6 Months",
        main="Good/Bad Loan vs Loan Inquiries",
        xlab = "0 = Good Loan            1 = Bad Loan")

plot(data$DQ2yrs,data$target,col='darkgreen')

plot(data$PublicRec,data$target)

plot(data$Amount.Requested,data$target)

# correlation matrix 
library(corrplot)
corrdata <- data[, c(8,10,11,12,14,30)]
x <- matrix(corrdata)
corr.variables <- cor(corrdata)
corrplot(corr.variables,type = "upper",order = "alphabet",tl.srt = 45)


# split data into 70% training/30% test
split_data <- nrow(data) * 0.7 
test_index <- sample(nrow(data), size = split_data)
training_set <- data[test_index,]
test_set <- data[-test_index,]


# logistic regression model built on training set with only significant predictors
# TotalCREDITLines and RevolvingCREDITBalance are not significant so I will leave them out 
glm1 <- glm(target ~ Amount.Requested+Inquiries6M+MonthlyIncome
                    ,data = training_set,
            family = 'binomial')
summary(glm1)

# make a tree with same predictors as logistic regression model
tree1 <- rpart(target ~ Amount.Requested+Inquiries6M+MonthlyIncome,data = training_set,
               cp=.001)
tree1

draw.tree(tree1, units="P(Bad)")
# prune the tree
prunedtree <- clip.rpart(tree1, best = 5)
prunedtree
draw.tree(prunedtree, units="P(Bad)")
plot(as.party(prunedtree), tp_args = list(id = F))


# logistic predictions
glm2 <- glm(target ~ Amount.Requested+Inquiries6M+MonthlyIncome
            ,data = test_set,
            family = 'binomial')
logisticpred <- predict(glm2, type = 'response',newdata = test_set)

# tree predictions
tree2 <- rpart(target ~ Amount.Requested+Inquiries6M+MonthlyIncome,data = test_set,
               cp=.001)
treepred <- predict(tree2,newdata = test_set)


# gains tables
library(gains)
logisticgains <- gains(test_set$target,logisticpred)
logisticgains

treegains <- gains(test_set$target,treepred)
treegains


# ROC Curves
library(InformationValue)
logisticplogis <- plogis(predict(glm2, test_set))
# logistic regression ROC curve based on test set
plotROC(test_set$target,logisticplogis)

treeplogis <- plogis(predict(tree2,test_set))
# tree ROC curve based on test set
plotROC(test_set$target,treeplogis)


# results of both models
logisticresults <- as.data.frame(cbind(test_set$target,logisticpred))
names(logisticresults)[1]="Bad"
names(logisticresults)[2]="Predicted Probability of Bad (Logit)"
head(logisticresults)
tail(logisticresults)

treeresults <- as.data.frame(cbind(test_set$target,treepred))
names(treeresults)[1]="Bad"
names(treeresults)[2]="Predicted Probability of Bad (Tree)"
head(treeresults)
tail(treeresults)

# export scored data to Excel
test_set$Predicted.Probability.of.Bad.Logistic.Regression <- logisticpred
test_set$Predicted.Probability.of.Bad.Tree <- treepred
#write.xlsx(test_set, "/Users/jordanjohnson/Desktop/STAT 551 - Predictive Analytics/Final.Project.Scored.Data.test_set.xlsx")


# export results to Excel to make KS plots
logistic.regression.results <- write.xlsx(logisticresults,"/Users/jordanjohnson/Desktop/STAT 551 - Predictive Analytics/logistic.regression.results.KS.xlsx")
tree.results <- write.xlsx(treeresults, "/Users/jordanjohnson/Desktop/STAT 551 - Predictive Analytics/tree.results.KS.xlsx")

