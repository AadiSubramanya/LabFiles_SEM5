install.packages("readxl")
install.packages("ggplot2")
install.packages("factoextra")

library(readxl)
library(ggplot2)
library(factoextra)

PCA_DATA <- read_excel("C:\\Users\\maths\\Documents\\Aadi_240957002\\MVA\\week7n8\\PCA Data .xls")
View(PCA_DATA)
data <- data.frame(PCA_DATA)
data_numeric <- data[,-1]

data_scaled <- scale(data_numeric)

pca_result <- prcomp(data_scaled,center = TRUE, scale=TRUE)
summary(pca_result)

eigenvalues <- pca_result$sdev^2
print(eigenvalues)

prop_var <- eigenvalues/sum(eigenvalues)
print(prop_var)

loadings <-pca_result$rotation
print(loadings)

scores <- pca_result$x
head(scores)

fviz_eig(pca_result, addlabels=TRUE)

fviz_pca_biplot(pca_result,
                repel=TRUE,
                col.var="red",
                col.ind="blue")

fviz_pca_var(pca_result,
             col.var="contrib",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)

fviz_pca_ind(pca_result,
             col.ind="cos2",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)

#-------------------------------------------------------------------------------------------------------
#PRACTICE Q

data("iris")
View(iris)
data <- data.frame(iris)
data_numeric <- data[,-1-4]
data_numeric

data_scaled <- scale(data_numeric)

pca_result <- prcomp(data_scaled,center = TRUE, scale=TRUE)
summary(pca_result)

eigenvalues <- pca_result$sdev^2
print(eigenvalues)

prop_var <- eigenvalues/sum(eigenvalues)
print(prop_var)

loadings <-pca_result$rotation
print(loadings)

scores <- pca_result$x
head(scores)

fviz_eig(pca_result, addlabels=TRUE)

fviz_pca_biplot(pca_result,
                repel=TRUE,
                col.var="red",
                col.ind="blue")

fviz_pca_var(pca_result,
             col.var="contrib",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)

fviz_pca_ind(pca_result,
             col.ind="cos2",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)

#-------------------------------------------------------------------------------------------------------------------------------


dataser <- read_excel("C:\\Users\\maths\\Documents\\Aadi_240957002\\MVA\\week7n8\\Pizza Data Set.xlsx")
View(dataser)
data <- data.frame(dataser)
data_numeric <- data[,3:9]
data_numeric

data_scaled <- scale(data_numeric)

pca_result <- prcomp(data_scaled,center = TRUE, scale=TRUE)
summary(pca_result)

eigenvalues <- pca_result$sdev^2
print(eigenvalues)

prop_var <- eigenvalues/sum(eigenvalues)
print(prop_var)

loadings <-pca_result$rotation
print(loadings)

scores <- pca_result$x
head(scores)

fviz_eig(pca_result, addlabels=TRUE)

fviz_pca_biplot(pca_result,
                repel=TRUE,
                col.var="red",
                col.ind="blue")

fviz_pca_var(pca_result,
             col.var="contrib",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)

fviz_pca_ind(pca_result,
             col.ind="cos2",
             gradient.cols=c("blue","yellow","red"),
             repel=TRUE)







