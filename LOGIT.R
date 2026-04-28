library(pROC)

vars <- read.table("regression_variables.txt", header = TRUE, sep = "\t", na.strings = "nan")

# response variable
y <- as.numeric(vars$highBP)

# predictors
X <- vars[, !(names(vars) %in% c("id", "highBP"))]

set.seed(777)

# stratified train/test split
idx_0 <- which(y == 0)
idx_1 <- which(y == 1)

train_0 <- sample(idx_0, size = 0.7 * length(idx_0))
train_1 <- sample(idx_1, size = 0.7 * length(idx_1))

train_index <- c(train_0, train_1)

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

# create data frames
train_df <- as.data.frame(X_train)
train_df$highBP <- y_train

test_df <- as.data.frame(X_test)

# STANDARD LOGISTIC REGRESSION
logit_model <- glm(highBP ~ .,
                   data = train_df,
                   family = binomial)

# test predictions
test_pred <- predict(logit_model,
                     newdata = test_df,
                     type = "response")

# test AUC
roc_obj <- roc(y_test, test_pred)
auc_val <- auc(roc_obj)

cat("Final Test AUC:", auc_val, "\n")

plot(roc_obj,
     main = "Standard Logistic Regression ROC Curve (Test Set)",
     col = "blue",
     lwd = 2)

# training predictions
train_pred <- predict(logit_model,
                      newdata = train_df,
                      type = "response")

roc_train <- roc(y_train, train_pred)
auc_train <- auc(roc_train)

cat("Training AUC:", auc_train, "\n")

# confusion matrix
pred_class <- ifelse(test_pred > 0.5, 1, 0)

table(Predicted = pred_class, Actual = y_test)

# coefficients
cat("\nModel Coefficients:\n")
print(coef(logit_model))

# full summary with p-values
cat("\nModel Summary:\n")
summary(logit_model)

