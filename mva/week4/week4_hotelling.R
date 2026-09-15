dinstall.packages("Hotelling")
library(Hotelling)

data(iris)
head(iris)

View(iris)

setosa <- subset(iris, Species=="setosa")[,1:4]
versicolor <- subset(iris, Species=="versicolor")[,1:4]

cat("\nSample size of setosa: ", nrow(setosa), "\n")
cat("\nSample size of versicolor: ", nrow(versicolor), "\n")

cat("\nMean Vector of Setosa and Versicolor:\n")
print(colMeans(setosa))
print(colMeans(versicolor))

cat("\nCovariance Matrix of Setosa and Versicolor:\n")
print(cov(setosa))
print(cov(versicolor))
  
#Performing Hotelling's Two-Sample T^2 test
result <- hotelling.test(setosa, versicolor)

cat("\nHotelling's Two Sample result:\n")
print(result)

pvalue <- result$pval
cat("\nP-Value=",format(pvalue, scientific=TRUE), "\n")

alpha <- 0.05
if (pvalue<alpha){
  cat("\nDecision: Reject teh Null Hypothesis (H0)\n")
}


#################### PRACTICE QUESTIONS ####################
data("mtcars")
head(mtcars)
View(mtcars)


automatic <- subset(mtcars, am=="0")[, c("mpg","hp","wt")]
manual <- subset(mtcars, am=="1")[, c("mpg","hp","wt")]

automatic
manual

alpha <- 0.05

result <- hotelling.test(automatic, manual)
result

pvalue <- result$pval

if (pvalue>alpha){
  cat("Accept")
} else{
  cat("Reject")
}





