library(glmnet)
library(pROC)
library(grpreg)
library(pls)

# Regression Data
data = read.table("regression_variables.txt", sep="\t", header = TRUE, row.names = "id", na.strings = "nan")

vars = data[, !(names(data) %in% "id")]
y = as.numeric(vars$highBP)
X = vars[, !(names(data) %in% "highBP")]

# split into train vs. test
set.seed(777)

idx_0 = which(y == 0)
idx_1 = which(y == 1)

train_0 = sample(idx_0, size = 0.7 * length(idx_0))
train_1 = sample(idx_1, size = 0.7 * length(idx_1))

train_idx <- c(train_0, train_1)

# Training Data
X_train = X[train_idx, ]
y_train = y[train_idx]
train_df = vars[train_idx, ]

# Testing Data
X_test = as.data.frame(X[-train_idx, ])
y_test = y[-train_idx]
test_df = vars[-train_idx, ]

# Figure Specifications
W = 500
H = 500

# ===== Standard Logistic Model ===== #
logit_model = glm(highBP ~ ., data = train_df, family = binomial)
logit_pred = predict(logit_model, newData = X_test, type = "response")
logit_roc = roc(y_train, logit_pred)

png("roc_curves/standard_logistic_roc_test.png", width = W, height = H)
plot(logit_roc, col = "blue", lwd = 2)
dev.off()

# ===== LASSO Model ===== #
lasso_model = cv.glmnet(as.matrix(X_train), y_train, alpha = 1, family = "binomial", nfolds = 5)
lasso_pred = predict(lasso_model, newx = as.matrix(X_test), s = "lambda.min", type = "response")
lasso_roc = roc(y_test, as.vector(lasso_pred))

png("roc_curves/lasso_roc_test.png", width = W, height = H)
plot(lasso_roc, col = "hotpink", lwd = 2)
dev.off()

# Group
race_vars = c("black", "hispanic", "asian", "otherrace", "mexAmer")
group = 1:ncol(X_train)
col_names = colnames(X_train)

race_group_id = max(group) + 1
group[col_names %in% race_vars] = race_group_id

group_lasso_model = cv.grpreg(as.matrix(X_train), y_train, group = group, family = "binomial", penalty = "grLasso")
group_lasso_pred = predict(group_lasso_model, as.matrix(X_test), lambda = group_lasso_model$lambda.min, type = "response")
group_lasso_roc = roc(y_test, as.vector(group_lasso_pred))

png("roc_curves/group_lasso_roc_test.png", width = W, height = H)
plot(group_lasso_roc, col = "darkgreen", lwd = 2)
dev.off()

# ===== Ridge Model ===== #
ridge_model = cv.glmnet(as.matrix(X_train), y_train, alpha = 0, family = "binomial", nfolds = 5)
ridge_pred = predict(ridge_model, newx = as.matrix(X_test), s = "lambda.min", type = "response")
ridge_roc = roc(testData$highBP, as.vector(ridge_pred))

png("roc_curves/ridge_roc_test.png", width = W, height = H)
plot(ridge_roc, col = "red", lwd = 2)
dev.off()

# ===== PCR Model ===== #
n = ncol(vars) - 1
pcr_model = pcr(highBP ~ ., data = trainData, ncomp = n, scale = TRUE, validation = "CV")
pcr_pred = predict(pcr_model, testData, ncomp=n)
roc_pcr = roc(testData$highBP, pcr_pred)

png("roc_curves/pcr_roc_test.png", width = W, height = H)
plot(roc_pcr, col = "darkorange", lwd = 2)
dev.off()

# ===== All ===== #
png("roc_curves/all_roc_test.png", width = W, height = H)

plot(logit_roc, col = "blue", lwd = 2)
plot(lasso_roc, col = "hotpink", lwd = 2, add = TRUE)
plot(group_lasso_roc, col = "darkgreen", lwd = 2, add = TRUE)
plot(ridge_roc, col = "red", lwd = 2, add = TRUE)
plot(roc_pcr, col = "darkorange", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c("Standard","LASSO","Group LASSO", "Ridge", "PCR"),
       col = c("blue","hotpink","darkgreen", "red", "darkorange"),
       lwd = 2)

dev.off()
