#install.packages("Rtools")
#install.packages("car")
#install.packages("leaps")
library(car)
library(leaps)
library(ggplot2)
library(dplyr)

?state.x77
summary(state.x77)

head(state.x77)

dim(state.x77)

state.x77.df= data.frame(state.x77)
state_data= data.frame(state.x77[,c("Population","Income","Illiteracy","Murder","Frost")])

sta= cbind(state.abb, state.x77.df, state.region)
st = sta[, 2:9] # Take numeric variables as goal matrix
library(ellipse) 
library(corrplot)
corMatrix <- cor(as.matrix(st)) # Calculate correlation matrix
col <- colorRampPalette(c("red", "yellow", "blue"))  # 3 colors to represent coefficients -1 to 1.
corrplot.mixed(corMatrix, order = "AOE", lower = "number", lower.col = "black", 
               number.cex = .8, upper = "ellipse",  upper.col = col(10), 
               diag = "u", tl.pos = "lt", tl.col = "black") # Mix plots of "number" and "ellipse"

summary(state_data)

model1=lm(Murder~Population+Income+Illiteracy+Frost,data=state_data)
summary(model1)

cor(state_data)

scatterplotMatrix(state_data,main="Scatterplot Matrix")

vif(model1)

outlierTest(model1)

model2=lm(formula = Murder ~ Population + Illiteracy, 
    data = state_data)
summary(model2)

par(mfrow=c(2,2))
plot(model1)

par(mfrow=c(2,2))
plot(model2)

polynomial_regression= lm(Murder~ Illiteracy +I(Illiteracy^2),data=state_data)

par(mfrow=c(2,2))

plot(polynomial_regression)

AIC(model1,model2)

best_model_state= regsubsets(Murder ~ Population+Illiteracy+Income+Frost,data =state_data, nbest = 4)
plot(best_model_state, scale = "adjr2")

subsets(best_model_state,statistic = "cp",main="CP plot for finding the best possible model", 
        legend = F, xlim= c(0,5), ylim = c(0,60))
abline(1,1,lty=2,col= "blue")

## NA
