##data visualisations and graphs


#look and investiagte the data
dim(iris)
names(iris)
str(iris)
attributes(iris)
iris[1:3,]
head(iris,3)

tail(iris,3)

#view first 10 values of sepal length
iris[1:10, "Sepal.Length"]
iris$Sepal.Length[1:10]

#view numeric variables of the data
summary(iris)

library(Hmisc)

#view frequency variables of the data, for columns 1 & 5
describe(iris[,c(1,5)])


#averages
range(iris$Sepal.Length)

quantile(iris$Sepal.Length)

quantile(iris$Sepal.Length, c(0.1,0.3,0.65))

#variance
var(iris$Sepal.Length)

#histogram
hist(iris$Sepal.Length)

#density
plot(density(iris$Sepal.Length))

#pie chart
table(iris$Species)
pie(table(iris$Species))

#bar chart
barplot(table(iris$Species))


#correlation & covariance
cov(iris$Sepal.Length, iris$Petal.Length)

cor(iris$Sepal.Length, iris$Petal.Length)

cov(iris[,1:4])

#box plot
boxplot(Sepal.Length~Species, data = iris)

#scatter plot
with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))

#scatter plot with some white noise added
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))

#a graph matrix of all variables paired up against each other
pairs(iris)

#3d scatterplot
library(scatterplot3d)
scatterplot3d(iris$Petal.Width,iris$Sepal.Width,iris$Sepal.Length)

library(rgl)
plot3d(iris$Petal.Width,iris$Sepal.Length, iris$Sepal.Width)


#calculate the similarity between different flowers in iris and plot as heat map
dist.matrix<-as.matrix(dist(iris[,1:4]))
heatmap(dist.matrix)
?dist

#level plot
library(lattice)
?levelplot
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts=9, col.regions=rainbow(10)[10:1])

#contour
?filled.contour
filled.contour(volcano, color=terrain.colors, asp=1, plot.axes=contour(volcano, add=T))

#3d Surface
persp(volcano, theta=25, phi=30, expand=0.5, col="lightblue")

library(ggplot2)
?qplot
qplot(Sepal.Length, Sepal.Width, data=iris, facets= Species~.)
