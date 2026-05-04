library(glmnet)
library(pROC)
#install.packages("grpreg")
library(grpreg)

vars <- read.table("regression_variables.txt", header = TRUE, sep = "\t", na.strings = "nan")

# delete specified variables
vars <- vars[, !(names(vars) %in% c(
  "stroke",
  "heartAttack",
  "coronaryHD",
  "chestPain",
  "emphysema",
  "COPD",
  "liverCondition",
  "systolicBP",
  "diastolicBP"
))]

# check remaining variables
names(vars)
dim(vars)


y <- as.numeric(vars$highBP)

X <- vars[, !(names(vars) %in% c("id", "highBP"))]
X <- as.matrix(X)

set.seed(777)

idx_0 <- which(y == 0)
idx_1 <- which(y == 1)

train_0 <- sample(idx_0, size = 0.7 * length(idx_0))
train_1 <- sample(idx_1, size = 0.7 * length(idx_1))

train_index <- c(train_0, train_1)

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

lasso_model <- cv.glmnet(X_train, y_train,
                         alpha = 1,
                         family = "binomial",
                         nfolds = 5)

test_pred <- predict(lasso_model,
                     newx = X_test,
                     s = "lambda.min",
                     type = "response")

roc_obj <- roc(y_test, as.vector(test_pred))
auc_val <- auc(roc_obj)

cat("Final Test AUC:", auc_val, "\n")

plot(roc_obj, main = "LASSO ROC Curve (Test Set)", col = "pink", lwd = 2)

train_pred <- predict(lasso_model,
                      newx = X_train,
                      s = "lambda.min",
                      type = "response")

roc_train <- roc(y_train, as.vector(train_pred))
auc_train <- auc(roc_train)

cat("Training AUC:", auc_train, "\n")

pred_class <- ifelse(test_pred > 0.5, 1, 0)

table(Predicted = pred_class, Actual = y_test)

coefs <- as.matrix(coef(lasso_model, s = "lambda.min"))

nonzero <- coefs[coefs != 0, , drop = FALSE]

cat("\nNon-zero coefficients:\n")
print(nonzero)

cat("Lambda (min):", lasso_model$lambda.min, "\n")
cat("Lambda (1se):", lasso_model$lambda.1se, "\n")

cat("LASSO AUC:", auc(roc_obj), "\n")

coefs_lasso <- as.matrix(coef(lasso_model, s = "lambda.min"))

# remove intercept
coefs_no_intercept <- coefs_lasso[-1, , drop = FALSE]

num_zero <- sum(coefs_no_intercept == 0)
num_nonzero <- sum(coefs_no_intercept != 0)
total <- length(coefs_no_intercept)

cat("LASSO Sparsity:\n")
cat("Non-zero:", num_nonzero, "\n")
cat("Zero:", num_zero, "\n")
cat("Sparsity (% zero):", round(100 * num_zero / total, 2), "%\n\n")


# GROUP LASSO

race_vars <- c("black", "hispanic", "asian", "otherRace", "mexAmer")

group <- 1:ncol(X_train)
col_names <- colnames(X_train)

# assign same group to race variables
race_group_id <- max(group) + 1
group[col_names %in% race_vars] <- race_group_id

# fit group lasso
group_lasso_model <- cv.grpreg(
  X_train, y_train,
  group = group,
  family = "binomial",
  penalty = "grLasso"
)

# predictions
test_pred_grp <- predict(
  group_lasso_model,
  X_test,
  lambda = group_lasso_model$lambda.min,
  type = "response"
)

# AUC
roc_grp <- roc(y_test, as.vector(test_pred_grp))
auc_grp <- auc(roc_grp)

cat("Group LASSO Test AUC:", round(auc_grp, 4), "\n")

# plot ROC
plot(roc_grp,
     main = "Group LASSO ROC Curve (Test Set)",
     col = "green", lwd = 2)

# coefficients
coefs_grp <- as.matrix(coef(
  group_lasso_model,
  lambda = group_lasso_model$lambda.min
))

# print ONLY non-zero coefficients
nonzero_grp <- coefs_grp[coefs_grp != 0, , drop = FALSE]

cat("\nGroup LASSO Non-zero Coefficients:\n")
print(nonzero_grp)

# sparsity counts (excluding intercept)
coef_no_intercept <- coefs_grp[-1, , drop = FALSE]

num_zero <- sum(coef_no_intercept == 0)
num_nonzero <- sum(coef_no_intercept != 0)
total <- length(coef_no_intercept)

cat("\nGroup LASSO Sparsity:\n")
cat("Non-zero:", num_nonzero, "\n")
cat("Zero:", num_zero, "\n")
cat("Sparsity (% zero):",
    round(100 * num_zero / total, 2), "%\n")

# lambda values
cat("\nLambda Min:", group_lasso_model$lambda.min, "\n")

# full coefficient matrix
coefs_grp <- as.matrix(coef(
  group_lasso_model,
  lambda = group_lasso_model$lambda.min
))

# remove intercept
coefs_no_intercept <- coefs_grp[-1, , drop = FALSE]

# -------------------------------
# DELETED VARIABLES (ZERO COEFS)
# -------------------------------
zero_idx <- which(coefs_no_intercept == 0)

deleted_vars <- rownames(coefs_no_intercept)[zero_idx]

cat("\nGroup LASSO Deleted Variables (Coefficient = 0):\n")
print(deleted_vars)
