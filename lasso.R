library(glmnet)
library(pROC)
#install.packages("grpreg")
library(grpreg)

vars <- read.table("C:/Users/Ko Ago/Downloads/regression_variables (1).txt",
                   header = TRUE, sep = "\t", na.strings = "nan")

# convert sedentary from minutes to hours
vars$sedentary <- vars$sedentary / 60

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

plot(roc_obj, main = "LASSO ROC Curve (Test Set)", col = "blue", lwd = 2)

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

# GROUP LASSO

race_vars <- c("black", "hispanic", "asian", "otherrace", "mexAmer")

group <- 1:ncol(X_train)
col_names <- colnames(X_train)

# assign same group to race variables
race_group_id <- max(group) + 1
group[col_names %in% race_vars] <- race_group_id

# fit group lasso
group_lasso_model <- cv.grpreg(X_train, y_train,
                               group = group,
                               family = "binomial",
                               penalty = "grLasso")

# predictions
test_pred_grp <- predict(group_lasso_model,
                         X_test,
                         lambda = group_lasso_model$lambda.min,
                         type = "response")

# AUC
roc_grp <- roc(y_test, as.vector(test_pred_grp))
auc_grp <- auc(roc_grp)

cat("Group LASSO Test AUC:", auc_grp, "\n")

# plot ROC
plot(roc_grp, main = "Group LASSO ROC Curve (Test Set)", col = "green", lwd = 2)

# coefficients
coefs_grp <- coef(group_lasso_model, lambda = group_lasso_model$lambda.min)

cat("\nGroup LASSO Coefficients:\n")
print(coefs_grp)
