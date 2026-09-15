#Practice Questions
#Q1
v <- seq(from=10,to=40, by=0.5)
class(v)
print("Increasing:")
v
print("Descending:")
sort(v,decreasing = TRUE)

v[4]
v[8]

v[c(5,10,7)]=c(100,101,120)
v

summary(v)

#Q2
q()
#ctrl+L
install.packages("ggplot2")
library(ggplot2)
summary(v)
#c(v1,v2)
#Ctrl + Enter
#Tools > Global Options > Pane Layout 
#Tools > Global Options > Appearance