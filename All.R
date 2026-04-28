library(glmnet)
library(pls)
library(pROC)

#read data
vars <- read.table("regression_variables.txt", header = TRUE, sep = "\t", na.strings = "nan")

# setting variables
y <- as.numeric(vars$highBP)

# making the matrix and remove highBP since it's Y
X <- as.matrix(vars[, !(names(vars) %in% c("id", "highBP"))])

# all regression variables
xy <- vars[, !(names(vars) %in% "id")]

#set seed but will change, best alpha for 123 was 0.9 for 67 it was 1 and 0.7 for 777
set.seed(777) #seed 1 also has best alpha = 1

#Set ALPHA
cv_model <- cv.glmnet(X, y, family = "binomial", alpha = 0) 
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1) 
cv_enet  <- cv.glmnet(X, y, family = "binomial", alpha = 0.5) 

nc = 10
model_pcr <- pcr(highBP ~ ., ncomp = nc, data = xy, scale = TRUE, validation = "CV")

#predictions
pred_ridge <- predict(cv_model, newx = X, s = "lambda.min", type = "link")
pred_lasso <- predict(cv_lasso, newx = X, s = "lambda.min", type = "link")
pred_enet  <- predict(cv_enet,  newx = X, s = "lambda.min", type = "link")
pred_pcr   <- predict(model_pcr, xy, ncomp = nc)

#Roc & Auc
roc_ridge <- roc(y, as.vector(pred_ridge))
roc_lasso <- roc(y, as.vector(pred_lasso))
roc_enet  <- roc(y, as.vector(pred_enet))
roc_pcr   <- roc(xy$highBP, pred_pcr)

auc_ridge <- auc(roc_ridge)
auc_lasso <- auc(roc_lasso)
auc_enet  <- auc(roc_enet)
auc_pcr   <- auc(roc_pcr)

#Betas
ridge_coefs <- as.matrix(coef(cv_model, s = "lambda.min"))
lasso_coefs <- as.matrix(coef(cv_lasso, s = "lambda.min"))
enet_coefs  <- as.matrix(coef(cv_enet, s = "lambda.min"))

ridge_sparsity <- sum(abs(ridge_coefs[-1, ]) > 1e-6)
lasso_sparsity <- sum(lasso_coefs != 0) - 1
enet_sparsity  <- sum(enet_coefs != 0) - 1

#compare performance
model_comparison <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net"),
  AUC = c(auc_ridge, auc_lasso, auc_enet),
  
  Lambda_Min = c(
    cv_model$lambda.min,
    cv_lasso$lambda.min,
    cv_enet$lambda.min
  ),
  
  Lambda_1SE = c(
    cv_model$lambda.1se,
    cv_lasso$lambda.1se,
    cv_enet$lambda.1se
  ),
  
  NonZero_Coefficients = c(
    ridge_sparsity,
    lasso_sparsity,
    enet_sparsity
  )
)

print(model_comparison)

#ridge
cat("\n===== RIDGE =====\n")
cat("Lambda min:", cv_model$lambda.min, "\n")
cat("AUC:", auc_ridge, "\n")
print(ridge_coefs)

plot(roc_ridge, col = "blue", lwd = 2, main = "ROC - Ridge")

#lasso
cat("\n===== LASSO =====\n")
cat("Lambda min:", cv_lasso$lambda.min, "\n")
cat("AUC:", auc_lasso, "\n")
print(lasso_coefs)

cat("\nNon-zero Lasso coefficients:\n")
print(lasso_coefs[lasso_coefs != 0, , drop = FALSE])

plot(roc_lasso, col = "red", lwd = 2, main = "ROC - Lasso")

#elasticnet 0.5
cat("\n===== ELASTIC NET =====\n")
cat("Lambda min:", cv_enet$lambda.min, "\n")
cat("AUC:", auc_enet, "\n")
print(enet_coefs)

cat("\nNon-zero Elastic Net coefficients:\n")
print(enet_coefs[enet_coefs != 0, , drop = FALSE])

plot(roc_enet, col = "darkgreen", lwd = 2, main = "ROC - Elastic Net")

#PCR
cat("\n===== PRINCIPAL COMPONENT REGRESSION =====\n")
cat("AUC:", auc_pcr, "\n")
plot(roc_pcr, col = "darkorange", lwd = 2, main = "ROC - PCR")

#ROC Curves overlay
plot(roc_ridge, col="blue", lwd=2)
plot(roc_lasso, col="red", lwd=2, add=TRUE)
plot(roc_enet, col="darkgreen", lwd=2, add=TRUE)
plot(roc_pcr, col="darkorange", lwd=2, add=TRUE)

legend("bottomright",
       legend=c("Ridge","Lasso","Elastic Net", "Principle Component Regression"),
       col=c("blue","red","darkgreen", "darkorange"),
       lwd=2)


#testing best alpha
alphas <- seq(0, 1, by = 0.1)

cv_results <- lapply(alphas, function(a) {
  cv.glmnet(X, y, alpha = a, family = "binomial")
})

cv_errors <- sapply(cv_results, function(model) min(model$cvm))

best_alpha <- alphas[which.min(cv_errors)]
best_alpha


#show lambdas
lambda_table <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net 0.5"),
  Lambda_Min = c(cv_model$lambda.min, cv_lasso$lambda.min, cv_enet$lambda.min),
  Lambda_1SE = c(cv_model$lambda.1se, cv_lasso$lambda.1se, cv_enet$lambda.1se)
)

print(lambda_table)

#AUCs
cat("\n===== AUC COMPARISON =====\n")
cat("Ridge AUC:", auc_ridge, "\n")
cat("Lasso AUC:", auc_lasso, "\n")
cat("Elastic Net (0.5) AUC:", auc_enet, "\n")
#Ridge is the best with the highest AUC


#lasso removed vars
lasso_summary <- data.frame(
  Variable = rownames(lasso_coefs),
  Coefficient = lasso_coefs[,1],
  Selected = ifelse(lasso_coefs[,1] != 0, "Kept", "Removed")
)

print(lasso_summary)

#elastic net removed vars
enet05_coefs <- as.matrix(coef(cv_enet, s = "lambda.min"))

enet05_removed <- rownames(enet05_coefs)[enet05_coefs[,1] == 0]
enet05_removed <- enet05_removed[enet05_removed != "(Intercept)"]

enet05_removed

comparison_removed <- list(
  ElasticNet_0.5 = enet05_removed
)

comparison_removed

# Extract ridge coefficients
ridge_coefs <- as.matrix(coef(cv_model, s = "lambda.min"))

ridge_table <- data.frame(
  Variable = rownames(ridge_coefs),
  Coefficient = ridge_coefs[, 1],
  Abs_Coefficient = abs(ridge_coefs[, 1])
)

ridge_table <- ridge_table[order(-ridge_table$Abs_Coefficient), ]

print(ridge_table)

cat("Max coefficient (absolute):", max(ridge_table$Abs_Coefficient), "\n")
cat("Mean coefficient (absolute):", mean(ridge_table$Abs_Coefficient), "\n")
cat("Median coefficient (absolute):", median(ridge_table$Abs_Coefficient), "\n")

top_ridge <- head(ridge_table, 20)
print(top_ridge)


# Unregularized logistic regression
glm_model <- glm(y ~ ., data = data.frame(y = y, X), family = binomial)
glm_coefs <- coef(glm_model)

comparison <- data.frame(
  Variable = names(glm_coefs),
  Unregularized = glm_coefs,
  Ridge = as.numeric(ridge_coefs[names(glm_coefs), ])
)

print(comparison)

#AUC again
library(pROC)

ridge_prob <- predict(cv_model, newx = as.matrix(X), s = "lambda.min", type = "response")

roc_obj <- roc(y, as.vector(ridge_prob))
auc(roc_obj)

plot(roc_obj, main = "Ridge Logistic Regression ROC Curve")

cv_model$lambda.min
cv_model$lambda.1se
summary(abs(ridge_table$Coefficient))

plot(ridge_table$Abs_Coefficient, type = "h",
     main = "Ridge Coefficient Magnitudes",
     xlab = "Variables", ylab = "Absolute Coefficient")

pred_class <- ifelse(ridge_prob > 0.5, 1, 0)

table(Predicted = pred_class, Actual = y)

cv_model$cvm
cv_model$lambda.min

