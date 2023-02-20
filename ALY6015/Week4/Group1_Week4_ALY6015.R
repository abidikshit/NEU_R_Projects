data('mtcars') 
head(mtcars) 

?mtcars

set.seed(100)
trainIndex <- sort(sample(x = nrow(mtcars), size = nrow(mtcars) * 0.7)) 
sample_train <- mtcars[trainIndex,] 
sample_test <- mtcars[-trainIndex,] 
head(sample_train)
head(sample_test)

summary(sample_train)

hist(sample_train$mpg,breaks = 10,xlab="MPG", col = "skyblue", xlim=c(5,35))

input<- sample_train
input$am <- as.factor(input$am)
levels(input$am) <-c("AT", "MT")

table(input$am)

dim(input)

library(ggplot2)
library(caret)
ggplot(input, aes(x=am, y=mpg)) + geom_boxplot(fill="lightgreen")

pairs(mpg ~ ., data = sample_train, col= "red")

options(scipen = 100)
model_step <- step(lm(mpg ~ ., data = mtcars), direction = 'both') 
summary(model_step)

step(lm(mpg ~ 1, data = mtcars), direction = 'forward', scope = ~ disp + hp + drat + wt + qsec) 
model_forward <- lm(formula = mpg ~ wt + hp, data = mtcars) 
summary(model_forward) 

par(mfrow=c(2,2))
plot(model_step,pch=23,col="orange",cex=2.5,cex.lab=1.6,lwd=3)

fit1 <- lm(formula = mpg ~ wt, data = mtcars) 
fit2 <- lm(formula = mpg ~ wt + hp, data = mtcars) 
anova(fit1, fit2)  

AIC(fit1, fit2) 

BIC(fit1, fit2) 

library(leaps) 
library(ISLR) 
library(dplyr) 

summary(Hitters) 
Hitters <- Hitters %>% na.omit() 

best_subset = regsubsets(Salary ~ ., data = Hitters, nvmax = 19) 
reg.summary <- summary(best_subset) 
reg.summary 
names(reg.summary) 

reg.summary$cp 
reg.summary$adjr2 
reg.summary$bic 

which.min(reg.summary$cp) 
which.max(reg.summary$adjr2) 
which.min(reg.summary$bic) 

backward = regsubsets(Salary ~ ., data = Hitters, method = "backward") 
reg.summary <- summary(backward) 
reg.summary 
names(reg.summary) 

which.max(reg.summary$adjr2) 

## NA
