cat("Student Name: Abhilash Dikshit","\n")
cat("Student Roll Number: 002702209")

data('ToothGrowth') 
head(ToothGrowth) 

summary(ToothGrowth)
nrow(ToothGrowth)
ncol(ToothGrowth)
names(ToothGrowth)

library(GGally)
ggpairs(ToothGrowth)

library(plotly)
par(mfrow=c(2,3))
plot_ly(data=ToothGrowth, x=~supp, y=~len, type="box")

ToothGrowth$dose = factor(ToothGrowth$dose, levels=c(0.5,1.0,2.0),labels=c("low","med","high"))   

str(ToothGrowth)

replications(len ~ supp * dose, data=ToothGrowth)

replications(len ~ supp * dose, data=ToothGrowth[1:58,])

aggregate(ToothGrowth$len, by=list(ToothGrowth$supp,ToothGrowth$dose), FUN = mean)
aggregate(ToothGrowth$len, by=list(ToothGrowth$supp,ToothGrowth$dose), FUN = sd)

dose <- factor(ToothGrowth$dose)
fit <- aov(ToothGrowth$len ~ ToothGrowth$supp*ToothGrowth$dose)
summary(fit)

dose <- factor(dose)
fit <- aov(ToothGrowth$len ~ ToothGrowth$supp*ToothGrowth$dose)
summary(fit)

 boxplot(len ~ supp * dose, data=ToothGrowth, ylab="Tooth Length", main="Boxplots of Tooth Growth Data", col= "skyblue")

with(ToothGrowth, interaction.plot(x.factor=dose, trace.factor=supp,response=len, fun=mean, type="b", legend=T,ylab="Tooth Length", main="Interaction Plot",pch=c(1,19)))

coplot(len ~ dose | supp, data=ToothGrowth, panel=panel.smooth,xlab="ToothGrowth data: length vs dose, given type of supplement", col= "red")

aov.out = aov(len ~ supp * dose, data=ToothGrowth)

model.tables(aov.out, type="means", se=T)

summary(aov.out)

options("contrasts")

summary.lm(aov.out)

## NA
