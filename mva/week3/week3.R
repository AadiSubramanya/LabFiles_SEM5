#Library
library(MASS)

#Mean Vector
mu <- c(10,20,30,40)
mu

#Covariance Matrix
Sigma <- matrix(
  c(
    9,3,2,1,
    3,16,4,2,
    2,4,25,5,
    1,2,5,36
  ),
  nrow=4,
  byrow=TRUE
)

Sigma

#Generating Multivariate Normal Data
set.seed(123)

sim_data <- mvrnorm(
  n=100000,
  mu=mu,
  Sigma=Sigma
)

#Coverting to DataFrame
sim_data <- as.data.frame(sim_data)

#Assign variable names
names(sim_data) <- c("X1","X2","X3","X4")
head(sim_data)

#Sample Mean and Covariance matrix
colMeans(sim_data)
cov(sim_data)

#Partition the mean vector and cov matrix
mu_a <- mu[1]
mu_b <- mu[2:4]

mu_a
mu_b


Sigma_AA <- matrix(
  Sigma[1, 1],
  nrow=1
)

Sigma_AB <- matrix(
  Sigma[1, 2:4],
  nrow=1
)

Sigma_BA <- matrix(
  Sigma[2:4, 1],
  nrow=3
)

Sigma_BB <- Sigma[2:4, 2:4]


Sigma_AA


#Specifying values of conditioning variables
x_B <- c(22,32,41)
x_B


#Calculating conditional means:
conditional_mean <- as.numeric(
  mu_a+
    Sigma_AB%*%solve(Sigma_BB)%*%
    (x_B - mu_b)
)
conditional_mean


#Calculating conditional variance:
conditional_variance <- as.numeric(
  Sigma_AA-
    Sigma_AB%*%solve(Sigma_BB)%*%
    Sigma_BA
)
conditional_variance

#conditional sd
conditional_sd <- sqrt(conditional_variance)
conditional_sd

#generating observation using the conditional distribution
set.seed(123)

conditional_sample <- rnorm(
  n=1000,
  mean=conditional_mean,
  sd=conditional_sd
)

mean(conditional_sample)
var(conditional_sample)


########################### REAL DATA ##############################

data("USArrests")
head(USArrests)

#Mean Vector and covariance matrix
mu_us <- colMeans(USArrests)
mu_us

sigma_us <- cov(USArrests)
sigma_us

#and all above learnt operations on this dataset...







######################## practice questions #######################

#dataset
data("swiss")
head(swiss)
View(swiss)

#Q1: Find Mean vector and covariance matrix
mu_swiss <- colMeans(swiss)
sigma_swiss <- cov(swiss)

mu_swiss
sigma_swiss





#Q2: Conditional Distribution of Fertility
mu_A <- mu_swiss["Fertility"]
mu_B <- mu_swiss[c("Agriculture", "Examination", "Education")]

sigma_AA <- matrix(
  sigma_swiss["Fertility", "Fertility"],
  nrow=1
)

sigma_AB <- matrix(
  sigma_swiss[
    "Fertility",
    c("Agriculture", "Examination", "Education")
  ],
  nrow=1
)

sigma_BA <- matrix(
  sigma_swiss[
    c("Agriculture", "Examination", "Education"),
    "Fertility"
  ],
  nrow=3
)

sigma_BB <- sigma_swiss[c("Agriculture", "Examination", "Education"), c("Agriculture", "Examination", "Education")]

x_B <- c(
  Agriculture = 40,
  Examination = 20,
  Education = 12
)

cond_mean <- as.numeric(
  mu_A+sigma_AB %*% solve(sigma_BB) %*% (x_B - mu_B)
)

cond_var <- as.numeric(
  sigma_AA-sigma_AB %*% solve(sigma_BB) %*% sigma_BA
)

cond_mean
cond_var






#Q3: Conditional distribution of Education
mu_A <- mu_swiss["Education"]
mu_B <- mu_swiss[c("Fertility", "Examination", "Catholic")]

sigma_AA <- matrix(
  sigma_swiss["Education", "Education"],
  nrow=1
)

sigma_AB <- matrix(
  sigma_swiss[
    "Education",
    c("Fertility", "Examination", "Catholic")
  ],
  nrow=1
)

sigma_BA <- matrix(
  sigma_swiss[
    c("Fertility", "Examination", "Catholic"),
    "Education"
  ],
  nrow=3
)

sigma_BB <- sigma_swiss[c("Fertility", "Examination", "Catholic"), c("Fertility", "Examination", "Catholic")]

x_B <- c(
  Fertility = 40,
  Examination = 20,
  Catholic = 12
)

cond_mean <- as.numeric(
  mu_A+sigma_AB %*% solve(sigma_BB) %*% (x_B - mu_B)
)

cond_var <- as.numeric(
  sigma_AA-sigma_AB %*% solve(sigma_BB) %*% sigma_BA
)

cond_mean
cond_var





#Q5: Conditional distribution of own example
mu_A <- mu_swiss["Fertility"]
mu_B <- mu_swiss[c("Education", "Catholic", "Infant.Mortality")]

sigma_AA <- matrix(
  sigma_swiss["Fertility", "Fertility"],
  nrow=1
)

sigma_AB <- matrix(
  sigma_swiss[
    "Fertility",
    c("Education", "Catholic", "Infant.Mortality")
  ],
  nrow=1
)

sigma_BA <- matrix(
  sigma_swiss[
    c("Education", "Catholic", "Infant.Mortality"),
    "Fertility"
  ],
  nrow=3
)

sigma_BB <- sigma_swiss[c("Education", "Catholic", "Infant.Mortality"), c("Education", "Catholic", "Infant.Mortality")]

x_B <- c(
  Agriculture = 40,
  Examination = 20,
  Education = 12
)

cond_mean <- as.numeric(
  mu_A+sigma_AB %*% solve(sigma_BB) %*% (x_B - mu_B)
)

cond_var <- as.numeric(
  sigma_AA-sigma_AB %*% solve(sigma_BB) %*% sigma_BA
)

cond_mean
cond_var


