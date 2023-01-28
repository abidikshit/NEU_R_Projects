# install.packages("ISLR")
# install.packages("stats")
# install.packages("pscl")
# install.packages("AER")
#install.packages("varImp")
library(pscl)
library(ggplot2)
library(dplyr)
library(ISLR)
library(stats)
library(psych) # Can be used for headTail function
library(base)
library(AER)
library(varImp)

?College

headTail(College, top = 4, bottom = 4, ellipsis = F)

dim(College)
cat("Number of Rows:", nrow(College), "\n")
cat("Number of Columns:", ncol(College), "\n")
cat("Blank cells count:", sum(!complete.cases(College)), "\n") # Displaying blank cells count 

summary(College)

pairs(College[,1:10], col = "skyblue")

attach(College)
plot(Private, Outstate, col = "red", varwidth = T, xlab = "Private", ylab = "Outstate")

# to divide the print window into four regions
par(mfrow=c(3,2))
# calling 4 histograms
hist(Top10perc, col = 2)
hist(Top25perc, col =7)
hist(Grad.Rate, col = 3)
hist(PhD, col = 4)
hist(Terminal, col=8)
hist(perc.alumni, col=10)

#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(College), replace=TRUE, prob=c(0.7,0.3))
train <- College[sample, ]
test <- College[!sample, ]  

#fit logistic regression model
model1 <- glm(Private~., family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model1)

pscl::pR2(model1)["McFadden"]

imp <- as.data.frame(caret::varImp(model1))
imp <- data.frame(overall = imp$Overall,
           names   = rownames(imp))
imp_df = imp[order(imp$overall,decreasing = T),]
imp_df

top5= head(imp_df, n=5)$names
top5

#define two individuals
new <- data.frame(Apps=4000, Accept= 1000, Enroll= 600, Top10perc= 5, Top25perc= 20, F.Undergrad = 2500, P.Undergrad= 6000,Outstate = 5000, Room.Board = 3000, Books= 800, Personal= 1500, PhD= 20, Terminal=15, S.F.Ratio= 6, perc.alumni= 15, Expend= 8000, Grad.Rate = 30, Elite= c("Yes", "No"), Elite.1= c("Yes", "No"))

#predict probability of defaulting
predict(model1, new, type="response")

#calculate probability of default for each individual in test dataset
predicted <- predict(model1, test, type="response")

head(predicted)

devtools::install_github("selva86/InformationValue")

library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test$Private1 <- ifelse(test$Private=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$Private1, predicted)[1]
optimal

confusionMatrix(test$Private1, predicted)

#calculate sensitivity
sensitivity(test$Private1, predicted)

#calculate specificity
specificity(test$Private1, predicted)

# calculate precision
precision(test$Private1, predicted)

#calculate total misclassification error rate
misClassError(test$Private1, predicted, threshold=optimal)

plotROC(test$Private1, predicted)

#fit logistic regression model
model2 <- glm(Private~F.Undergrad+Outstate+S.F.Ratio+Grad.Rate, family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model2)

pscl::pR2(model2)["McFadden"]

imp2 <- as.data.frame(caret::varImp(model2))
imp2 <- data.frame(overall = imp2$Overall,
           names   = rownames(imp2))
imp_df2 = imp2[order(imp2$overall,decreasing = T),]
imp_df2

top2= head(imp_df2, n=2)$names
top2

#calculate probability of default for each individual in test dataset
predicted2 <- predict(model2, test, type="response")

head(predicted2)

#convert defaults from "Yes" and "No" to 1's and 0's
#test$Private <- ifelse(test$Private2=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$Private1, predicted2)[1]
optimal

confusionMatrix(test$Private1, predicted2)

#calculate sensitivity
sensitivity(test$Private1, predicted2)

#calculate specificity
specificity(test$Private1, predicted2)

# calculate precision
precision(test$Private1, predicted2)

#calculate total misclassification error rate
misClassError(test$Private1, predicted2, threshold=optimal)

plotROC(test$Private1, predicted2)

anova(model1, model2, test='LR')

AIC(model1, model2)

deviance(model1)/df.residual(model1)

deviance(model2)/df.residual(model2)

pchisq(summary(model2)$dispersion * model1$df.residual, model1$df.residual, lower = F)

## NA
