"Hello World!"
5 + 5

plot(1:10)

num <- 5

for (x in 1:10) {
  print(x)
}

num

x <- "R is exciting"
class(x)

x <- 10.5   # numeric
y <- 10L    # integer
z <- 1i     # complex

str <- "Lorem ipsum dolor sit amet,
consectetur adipiscing elit,
sed do eiusmod tempor incididunt,
ut labore et dolore magna aliqua."

cat(str)

ls()

fruits <- list("apple", "banana", "cherry")
class(fruits)

thislist <- list("apple", "banana", 50, 100)
thislist

thismatrix <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2,byrow=TRUE)

thismatrix[1,2]


thismatrix <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
thismatrix
thismatrix[, c(1,2)]


for (rows in 1:nrow(thismatrix)) {
  for (columns in 1:ncol(thismatrix)) {
    print(thismatrix[rows, columns])
  }
}

plot(1, 3)

plot(c(1, 8), c(3, 10))


x <- c(1, 2, 3, 4, 5)
y <- c(3, 7, 8, 9, 12)

plot(x, y, main="My Graph", xlab="The x-axis", ylab="The y axis", col="blue", pch=16)