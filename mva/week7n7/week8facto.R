# ==============================================================================
# Factor Analysis
# ==============================================================================

# 1. Load dataset
data(mtcars)

# 2. Select variables
data_fa <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

# 3. Standardize data
data_scaled <- scale(data_fa)

# 4. Correlation matrix
cor_matrix <- cor(data_scaled)
print("Correlation Matrix:")
print(cor_matrix)

# 5. Eigenvalues
eigen_values <- eigen(cor_matrix)$values
print("Eigenvalues:")
print(eigen_values)

# 6. Scree Plot
plot(
  eigen_values,
  type = "b",
  main = "Scree Plot",
  xlab = "Factor Number",
  ylab = "Eigenvalue"
)

# 7. Perform Factor Analysis
fa_result <- factanal(data_scaled, factors = 2, rotation = "varimax")
print(fa_result)

# 8. Extract factor loadings
loadings_matrix <- as.matrix(fa_result$loadings)

# 9. Compute communalities
communalities <- rowSums(loadings_matrix^2)

# 10. Extract specific variance (uniqueness)
specific_variance <- fa_result$uniquenesses

# 11. Create final factor analysis table
fa_table <- cbind(
  loadings_matrix,
  Communality = communalities,
  Specific_Variance = specific_variance
)

print("Final Factor Analysis Table:")
print(round(fa_table, 3))


# ==============================================================================
# Practice Questions
# ==============================================================================

data(USArrests)
View(USArrests)
data_fa <- USArrests[, c("Murder", "Assault", "UrbanPop","Rape")]
data_scaled <- scale(data_fa)
cor_matrix <- cor(data_scaled)
print("Correlation Matrix:")
print(cor_matrix)
eigen_values <- eigen(cor_matrix)$values
print("Eigenvalues:")
print(eigen_values)
plot(
  eigen_values,
  type = "b",
  main = "Scree Plot",
  xlab = "Factor Number",
  ylab = "Eigenvalue"
)
fa_result <- factanal(data_scaled, factors = 1, rotation = "varimax")
print(fa_result)
loadings_matrix <- as.matrix(fa_result$loadings)
communalities <- rowSums(loadings_matrix^2)
specific_variance <- fa_result$uniquenesses
fa_table <- cbind(
  loadings_matrix,
  Communality = communalities,
  Specific_Variance = specific_variance
)

print("Final Factor Analysis Table:")
print(round(fa_table, 3))


#-------------------------------------------------------------------------------------

data=read_excel("C:\\Users\\maths\\Documents\\Aadi_240957002\\MVA\\week7n8\\big_five_personality_teaching_dataset.xlsx")
data_fa=data[,-1]
data_scaled <- scale(data_fa)
cor_matrix <- cor(data_scaled)
print("Correlation Matrix:")
print(cor_matrix)
eigen_values <- eigen(cor_matrix)$values
print("Eigenvalues:")
print(eigen_values)
plot(
  eigen_values,
  type = "b",
  main = "Scree Plot",
  xlab = "Factor Number",
  ylab = "Eigenvalue"
)
fa_result <- factanal(data_scaled, factors = 9, rotation = "varimax")
print(fa_result)
loadings_matrix <- as.matrix(fa_result$loadings)
communalities <- rowSums(loadings_matrix^2)
specific_variance <- fa_result$uniquenesses
fa_table <- cbind(
  loadings_matrix,
  Communality = communalities,
  Specific_Variance = specific_variance
)

print("Final Factor Analysis Table:")
print(round(fa_table, 3))



