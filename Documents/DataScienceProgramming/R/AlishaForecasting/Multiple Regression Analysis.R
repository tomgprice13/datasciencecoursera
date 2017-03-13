#multiple regression analysis https://rstudio-pubs-static.s3.amazonaws.com/65641_88a692252c6c4f2ab279d115e59e6767.html

library(car)
library(corrplot)
library(visreg)
install.packages("visreg")
library(visreg)
library(rgl)
library(knitr)
library(scatterplot3d)


#inspect and summarise the data
head(Prestige,5)
str(Prestige)
?str
summary(Prestige)

#subset data just to capture income, education, women and prestige
newdata=Prestige[,c(1:4)]
summary(newdata)

#plot the matrix of all variables in data
plot(newdata, pch=16, col="blue", main="Matrix Scatterplot of Income, Education, Women and Prestige")

#the above graph allows us to see the relationship between each of the variables in one graph

#we will fit a linear regression on this dataset and see how well it models the data
#we'll then add more predictors to the model giving each a seperate coefficient


#for the multiple regression we want to try and solve the following equation
#Income = B0 + B1*Education + B2*Prestige + B3*Women

#we want to use to try and create a plane or line that best models all the data

#here we will use the least squares approach
set.seed(1)
education.c=scale(newdata$education, center=T, scale=F)
prestige.c=scale(newdata$prestige, center=T, scale=F)
women.c=scale(newdata$women, center=T, scale=F)

?scale
newdata$education
education.c

#bind these new variables into newdata and display
new.c.vars=cbind(education.c, prestige.c, women.c)
?cbind
newdata=cbind(newdata, new.c.vars)
names(newdata)[5:7] = c("education.c","prestige.c","women.c")
summary(newdata)

#fit a linear model and run a summary of its results
mod1=lm(income~education.c+prestige.c+women.c,data=newdata)
summary(mod1)

#see website for details on R squared, what the coefficients represent and colinearity

newdatacor=cor(newdata[1:4])
?cor
?corrplot
corrplot(newdatacor, method="number")

#see website for details on excluding predcitor variables and the F stat


#now fit a linear model excluding the variable education
mod2=lm(income~prestige.c+women.c, data=newdata)
summary(mod2)
#exlcuding education from model has improved F stat, but didn't affect residual standard
#error or adjusted r squared

#now plot the residuals
plot(mod2, pch=16)

#still some points lying far away from middle area

#now create a 3d graph tovisualise the predictor and target variables

newdat<-expand.grid(prestige.c=seq(-35,45,by=5), women.c=seq(-25,70,by=5))
newdat$pp<-predict(mod2,newdata=newdat)
with(newdata, plot3d(prestige.c, women.c, income,col="blue",size=1, type="s",main="3D Linear Model Fit"))
with(newdat,surface3d(unique(prestige.c),unique(women.c), pp, alpha=0.3, front="line", back="line"))
?plot3d
?surface3d
?unique

#this interactive 3d plot allows us to change the view of the graph and we can
#see the outlier points more clearly

#now we will try to improve on the model by taking the log of income, still exclude
#education and squaring both predictors

mod3=lm(log(income)~prestige.c+I(prestige.c^2)+women.c+I(women.c)^2, data=newdata)
summary(mod3)

#plot the graph
plot(mod3, pch=16, which=1)

#now fit the 3d graph for new model
newdat2=expand.grid(prestige.c=seq(-35,45,by=5), women.c=seq(-25, 70, by=5))
newdat$pp<-predict(mod3, newdata=newdat2)
with(newdata, plot3d(prestige.c, women.c, log(income), col="blue", size=1, type="s", 
                     main="3D Quadratic Model Fit with Log of Income"))
with(newdat2, surface3d(unique(prestige.c), unique(women.c), newdat$pp, alpha=0.3, front="line", back="line"))
