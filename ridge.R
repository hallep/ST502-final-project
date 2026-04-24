library(glmnet)
library(pROC)

vars <- read.table("C:/Users/Ko Ago/Downloads/regression_variables (1).txt",
                   header = TRUE, sep = "\t", na.strings = "nan")

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

ridge_model <- cv.glmnet(X_train, y_train,
                         alpha = 0,          # <-- THIS makes it ridge
                         family = "binomial",
                         nfolds = 5)         # <-- ADDED (5-fold CV)

test_pred <- predict(ridge_model,
                     newx = X_test,
                     s = "lambda.min",
                     type = "response")

roc_obj <- roc(y_test, as.vector(test_pred))
auc_val <- auc(roc_obj)

cat("Final Test AUC:", auc_val, "\n")

plot(roc_obj, main = "Ridge ROC Curve (Test Set)", col = "red", lwd = 2)

train_pred <- predict(ridge_model,
                      newx = X_train,
                      s = "lambda.min",
                      type = "response")

roc_train <- roc(y_train, as.vector(train_pred))
auc_train <- auc(roc_train)

cat("Training AUC:", auc_train, "\n")

pred_class <- ifelse(test_pred > 0.5, 1, 0)

table(Predicted = pred_class, Actual = y_test)

coefs <- as.matrix(coef(ridge_model, s = "lambda.min"))

cat("\nCoefficients:\n")
print(coefs)

cat("Lambda (min):", final_model$lambda.min, "\n")
cat("Lambda (1se):", final_model$lambda.1se, "\n")
