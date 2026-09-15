#installing and checking for packages
install.packages("GGally")
install.packages("corrplot")

library(GGally)
library(corrplot)

#loading dataset
data(mtcars)

#viewing dataset
head(mtcars)
tail(mtcars)
View(mtcars)

#structure of dataset
str(mtcars)
dim(mtcars)

names(mtcars)
summary (mtcars)

#descriptive statistics-- dataset[rows, cols]
colMeans(mtcars[1:3,1:4])

#1-- apply function to each row 
#2-- apply function to each column
apply(mtcars, 2, median)
apply(mtcars, 2, sd)
apply(mtcars, 2, var)
apply(mtcars, 2, min)
apply(mtcars, 2, max)

#covariance and correlation matrix
cov_matrix <- cov(mtcars)
cor_matrix <- cor(mtcars)

cov_matrix
cor_matrix


#scatter plot
plot(
  mtcars$mpg,
  mtcars$cyl,
  col = mtcars$gear,
  pch = 19,
  xlab = "MPG (Miles Per Gallon)",
  ylab = "Cylinders",
  main = "Scatter Plot"
)


#scatter plot matrix
pairs(
  mtcars[1:5],
  col=mtcars$gear,
  pch = 19,
  main="scatter plot matrix"
)


#enhanced scatter plot matrix
mtcars$gear <- factor(mtcars$gear)

ggpairs(
  mtcars,
  aes(color=gear)
)


#correlation heat map plot
corrplot(
  cor_matrix,
  method="color",
  type="upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90
)


#histograms
hist(
  mtcars$mpg,
  col="skyblue",
  main="Miles Per Gallon (MPG)",
  xlab="MPG"
)

#boxplot
boxplot(
  mtcars,
  col="green",
  main="BoxPlot",
  ylab="measurement"
)


boxplot(
  mpg~gear,
  data=mtcars,
  col=c("red","green","blue"),
  main="Mpg VS Gear",
  xlab="MPG",
  ylab="Gear"
)


plot(
  mtcars$mpg,
  mtcars$gear,
  col=mtcars$cyl,
  pch=19,
  main="Mpg VS Gear",
  xlab="MPG",
  ylab="Gear"
)

legend(
  "topleft",
  legend=levels(mtcars$mpg),
  col=1:5,
  pch=19
)

round(cor_matrix,3)

#EXERCISE QUESTIONS

#q1
data(mtcars)
View(mtcars)

#q2
head(mtcars)

#q3
str(mtcars)
dim(mtcars)

#q4
cols=cbind(mpg,disp,hp,wt)
colMeans(cols)
apply(cols, 2, median)
apply(cols, 2, sd)
apply(cols, 2, var)
apply(cols, 2, min)
apply(cols, 2, max)

#q5
cov_matrix <- cov(mtcars)
cor_matrix <- cor(mtcars)
cor_matrix2 <- cor(cols)

cov_matrix
cor_matrix
cor_matrix2

#q6
plot(
  mtcars$wt,
  mtcars$mpg,
  col=mtcars$hp,
  pch=16,
  main="Weight VS Milage",
  xlab="Weight",
  ylab="Milage"
)

pairs(
  cols,
  col=mtcars$cyl,
  pch = 19,
  main="scatter plot matrix of selected matrix"
)

#for everything
corrplot(
  cor_matrix,
  method="color",
  type="upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90
)

#for selected cols
library(corrplot)
corrplot(
  cor_matrix2,
  method="color",
  type="upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90
)

attach(mtcars)
par(mfrow=c(2,2))
hist(
  mpg,
  col="skyblue",
  main="Miles Per Gallon (MPG)",
  xlab="MPG"
)

hist(
  disp,
  col="green",
  main="Display",
  xlab="Display"
)

hist(
  wt,
  col="yellow",
  main="Weight",
  xlab="wt"
)

hist(
  hp,
  col="blue",
  main="HP",
  xlab="HP"
)
par(mfrow = c(1, 1))

boxplot(
  cols,
  col="darkgreen",
  main="BoxPlot",
  ylab="measurement"
)

cor_matrix3 <- cor(wt,mpg)
cor_matrix3

