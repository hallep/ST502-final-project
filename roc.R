library(pROC)
library(glmnet)
library(grpreg)
library(pls)

# Regression Data
vars = read.table("regression_variables.txt", header = TRUE, row.names = "id", sep = "\t", na.strings = "nan")
y = as.numeric(vars$highBP)
X = vars[, !(names(vars) %in% "highBP")]

# split into train vs. test
set.seed(777)

idx_0 = which(y == 0)
idx_1 = which(y == 1)

train_0 = sample(idx_0, size = 0.7 * length(idx_0))
train_1 = sample(idx_1, size = 0.7 * length(idx_1))

train_idx = c(train_0, train_1)

# Training Data
X_train = X[train_idx, ]
y_train = y[train_idx]
train_df = vars[train_idx, ]

# Test Data
X_test = as.data.frame(X[-train_idx, ])
y_test = y[-train_idx]

train_df <- as.data.frame(X_train)
train_df$highBP <- y_train

test_df <- as.data.frame(X_test)

# Figure Sizes
W = H = 500

# ===== Standard Logistic Model ===== #
logit_model = glm(highBP ~ ., data = train_df, family = binomial)
test_pred = predict(logit_model, newdata = X_test, type = "response")
logit_roc <- roc(y_test, test_pred)
auc(logit_roc)

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

# ===== Group LASSO Model ===== #
race_vars = c("black", "hispanic", "asian", "otherRace", "mexAmer")
group = 1:ncol(X_train)
group[colnames(X_train) %in% race_vars] = max(group) + 1

group_lasso_model = cv.grpreg(as.matrix(X_train), y_train, group = group, family = "binomial", penalty = "grLasso")
group_lasso_pred = predict(group_lasso_model, as.matrix(X_test), lambda = group_lasso_model$lambda.min, type = "response")
group_lasso_roc = roc(y_test, as.vector(group_lasso_pred))
auc(group_lasso_roc)

png("roc_curves/group_lasso_roc_test.png", width = W, height = H)
plot(group_lasso_roc, col = "darkgreen", lwd = 2)
dev.off()

# ===== Ridge Model ===== #
ridge_model = cv.glmnet(as.matrix(X_train), y_train, alpha = 0, family = "binomial", nfolds = 5)
ridge_pred = predict(ridge_model, newx = as.matrix(X_test), s = "lambda.min", type = "response")
ridge_roc = roc(y_test, as.vector(ridge_pred))

png("roc_curves/ridge_roc_test.png", width = W, height = H)
plot(ridge_roc, col = "red", lwd = 2)
dev.off()

# ===== PCR Model ===== #
pca = prcomp(X_train, center = TRUE, scale = TRUE)
X_test_pca = as.data.frame(predict(pca, newdata = X_test))

train_pca_all_df = data.frame(y = y_train, pca$x)
pcr_model_all = glm(y ~ ., data = train_pca_all_df, family = binomial)
pcr_pred_all = predict(pcr_model_all, newdata = X_test_pca, type = "response")
pcr_roc_all = roc(y_test, pcr_pred_all)

train_pca_9_df = data.frame(y = y_train, pca$x[, 1:9])
pcr_model_9 = glm(y ~ ., data = train_pca_9_df, family = binomial)
pcr_pred_9 = predict(pcr_model_9, newdata = X_test_pca[1:9], type = "response")
pcr_roc_9 = roc(y_test, pcr_pred_9)

png("roc_curves/pcr_roc_test.png", width = 500, height = 500)
plot(pcr_roc_all, col = "brown", lwd = 2)
plot(pcr_roc_9, col = "darkorange", lwd = 2, add = TRUE)
legend("bottomright", legend = c("All Components", paste(m, "Components")),
       col = c("brown", "darkorange"), lwd = 2)
dev.off()

# ===== All ===== #
png("roc_curves/all_roc_test.png", width = W, height = H)

plot(logit_roc, col = "blue", lwd = 2)
plot(lasso_roc, col = "hotpink", lwd = 2, add = TRUE)
plot(group_lasso_roc, col = "darkgreen", lwd = 2, add = TRUE)
plot(ridge_roc, col = "red", lwd = 2, add = TRUE)
plot(pcr_roc_9, col = "darkorange", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c("Standard","LASSO","Group LASSO", "Ridge", "PCR"),
       col = c("blue","hotpink","darkgreen", "red", "darkorange"), lwd = 2)

dev.off()

# ===== AUC ===== #
test_aucs = data.frame(
  auc = round(c(auc(logit_roc), auc(lasso_roc), auc(group_lasso_roc), auc(ridge_roc), auc(pcr_roc_all), auc(pcr_roc_9)), 4),
  row.names = c("Logit", "LASSO", "group LASSO", "Ridge", "PCR (All)", "PCR (9)")
)
write.table(test_aucs, "tables/test_auc_values.txt", sep = "\t", quote = FALSE)
