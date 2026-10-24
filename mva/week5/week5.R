library(readxl)

wine <- read_excel("C:/Users/maths/Documents/Aadi_240957002/MVA/week5/wine.xlsx")

head(wine)

str(wine)

dim(wine)
  
names(wine)

wine$Class <- as.factor(wine$Class)

levels(wine$Class)

table(wine$Class)

colSums(is.na(wine))

aggregate(
  cbind(Alcohol, Malic_Acid, Flavanoids) ~ Class,
  data = wine,
  FUN = mean
)

fit <- manova(
  cbind(Alcohol, Malic_Acid, Flavanoids) ~ Class,
  data = wine
)

fit

summary(
  fit,
  test="Wilks"
)


summary(
  fit,
  test="Pillai"
)

summary(
  fit,
  test="Roy"
)

summary.aov(fit)

anova_results <- summary.aov(fit)
anova_results

aggregate(
  cbind(Alcohol, Malic_Acid, Flavanoids) ~ Class,
  data = wine,
  FUN = mean
)

par(mfrow = c(1,3))

boxplot(
  Alcohol ~ Class,
  data = wine,
  main = "Alcohol by Class",
  xlab = "Class",
  ylab = "Alcohol"
)

boxplot(
  Malic_Acid ~ Class,
  data = wine,
  main = "Malic_Acid by Class",
  xlab = "Class",
  ylab = "Malic_Acid"
)

boxplot(
  Flavanoids ~ Class,
  data = wine,
  main = "Flavanoids by Class",
  xlab = "Class",
  ylab = "Flavanoids"
)

par(mfrow = c(1,1))

cor(
  wine[,c(
    "Alcohol",
    "Malic_Acid",
    "Flavanoids"
  )]
)

cov(
  wine[wine$Class == 1,
       c("Alcohol", "Malic_Acid", "Flavanoids")]
)

cov(
  wine[wine$Class == 2,
       c("Alcohol", "Malic_Acid", "Flavanoids")]
)

cov(
  wine[wine$Class == 3,
       c("Alcohol", "Malic_Acid", "Flavanoids")]
)

anova_malic <- aov(
  Malic_Acid ~ Class,
  data = wine
)

summary(anova_malic)
TukeyHSD(anova_malic)

anova_flavanoids <- aov(
  Flavanoids ~ Class,
  data = wine
)


summary(anova_flavanoids)
TukeyHSD(anova_flavanoids)


############################# PRACTICE #######################################
fit <- manova(
  cbind(Ash, Magnesium, Total_Phenols) ~ Class,
  data = wine
)

fit

summary(
  fit,
  test="Wilks"
)


summary(
  fit,
  test="Pillai"
)

anova_results <- summary.aov(fit)
anova_results

par(mfrow=c(1,3))

boxplot(
  Ash ~ Class,
  data = wine,
  main = "Ash by Class",
  xlab = "Class",
  ylab = "Ash"
)

boxplot(
  Magnesium ~ Class,
  data = wine,
  main = "Magnesium by Class",
  xlab = "Class",
  ylab = "Magnesium"
)

boxplot(
  Total_Phenols ~ Class,
  data = wine,
  main = "Total_Phenols by Class",
  xlab = "Class",
  ylab = "Total_Phenols"
)


