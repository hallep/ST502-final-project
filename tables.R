library(glmnet)
library(pROC)
library(grpreg)
library(ggplot2)
library(patchwork)

# Regression Data
data = read.table("regression_variables.txt", sep="\t", header = TRUE, row.names = "id", na.strings = "nan")
n = ncol(data)

vars = data[, !(names(data) %in% "id")]
y = as.numeric(vars$highBP)
X = vars[, !(names(vars) %in% "highBP")]

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
nrow(train_df)
nrow(test_df)
# Testing Data
X_test = as.data.frame(X[-train_idx, ])
y_test = y[-train_idx]
test_df = vars[-train_idx, ]

# Variables
var_names = c("age", "gender", "black", "mexAmer", "hispanic", "asian", "otherRace", "someHS", "HSGrad", "someCollege", "collegeGrad", "married", "widowed", "divorced", "separated", "livingWithPartner", "incomePoverty", "dailyAlc", "weeklyAlc", "monthlyAlc", "yearlyAlc", "smokeEveryDay", "smokeSomeDays", "fairDiet", "goodDiet", "veryGoodDiet", "excellentDiet", "sleepWeekdays", "sleepWeekends", "vigWork", "modWork", "vigRec", "modRec", "sedentary", "bmi", "pulse", "cholesterol", "ferritin", "diabetes", "thyroidProblem")
var_labels = c("Age", "Gender", "Black", "Mexican American", "Hispanic", "Asian", "Other/mixed race", "Some high school", "High school graduate", "Some college", "College graduate", "Married", "Widowed", "Divorced", "Separated", "Living with partner", "Income-poverty ratio", "Drink daily", "Drink weekly", "Drink monthly", "Drink yearly", "Smoke every day", "Smoke some days", "Fair diet", "Good diet", "Very good diet", "Excellent diet", "Sleep on weekdays", "Sleep on weekends", "Vigorous work activity", "Moderate work activity", "Vigorous recreational activity", "Moderate recreational activity", "Sedentary activity", "BMI", "Pulse", "Cholesterol", "Ferritin", "Diabetes", "Thyroid problems")

row_names = c("(Intercept)", var_names)
row_labels = c("Intercept", var_labels)

# ===== Standard Logistic Model ===== #
logit_model = glm(highBP ~ ., data = train_df, family = binomial)
coefs = summary(logit_model)$coefficients[row_names, ]

logit_coefs = data.frame(
  Est = round(coefs[, "Estimate"], 3),
  Sd.Err = round(coefs[, "Std. Error"], 3),
  Odds.Ratio = round(exp(coefs[, "Estimate"]), 3),
  Pr = signif(coefs[, "Pr(>|z|)"], 3)
)
write.table(logit_coefs, "tables/logit_coefficients.txt", sep = "\t", quote = FALSE)

# ===== LASSO Model ===== #
lasso_model = cv.glmnet(as.matrix(X_train), y_train, alpha = 1, family = "binomial", nfolds = 5)
lasso_coefs = data.frame(lambda.min = coef(lasso_model, s = "lambda.min")[row_names, ], row.names = row_labels)
lasso_lambda_min = round(lasso_coefs[lasso_coefs$lambda.min != 0, "lambda.min", drop = FALSE], 3)

write.table(lasso_lambda_min, "tables/lasso_lambda_min.txt", sep = "\t", quote = FALSE)

cat("Lambda (min):", round(lasso_model$lambda.min,4), "\n")
cat("Lambda (1se):", round(lasso_model$lambda.1se, 3), "\n")

no_intercept = lasso_coefs[-1, , drop = FALSE]
num_zero = sum(no_intercept == 0)
total = nrow(no_intercept)

cat("Non-zero:", sum(no_intercept != 0), "\n")
cat("Zero:", num_zero, "\n")
cat("Sparsity (% zero):", round(100 * num_zero / total, 2), "%\n")

# ===== Group LASSO Model ===== #
race_vars = c("black", "hispanic", "asian", "otherRace", "mexAmer")
group = 1:ncol(X_train)
group[colnames(X_train) %in% race_vars] = max(group) + 1

group_lasso_model = cv.grpreg(as.matrix(X_train), y_train, group = group, family = "binomial", penalty = "grLasso")
group_lasso_coefs = data.frame(lambda.min = coef(group_lasso_model, s = "lambda.min")[row_names], row.names = row_labels)
group_lasso_lambda_min = as.data.frame(round(group_lasso_coefs[group_lasso_coefs$lambda.min != 0, "lambda.min", drop = FALSE], 3))
write.table(group_lasso_lambda_min, "tables/group_lasso_lambda_min.txt", sep = "\t", quote = FALSE)

cat("Lambda (min):", round(group_lasso_model$lambda.min,4), "\n")

no_intercept = group_lasso_coefs[-1, , drop = FALSE]
num_zero = sum(no_intercept == 0)
total = nrow(no_intercept)

cat("Non-zero:", sum(no_intercept != 0), "\n")
cat("Zero:", num_zero, "\n")
cat("Sparsity (% zero):", round(100 * num_zero / total, 2), "%\n")

# ===== Ridge Model ===== #
ridge_model = cv.glmnet(as.matrix(X_train), y_train, alpha = 0, family = "binomial", nfolds = 5)
ridge_coefs = data.frame(lambda.min = as.matrix(coef(ridge_model, s = "lambda.min")), row.names = row_labels)
ridge_lambda_min = round(ridge_coefs[ridge_coefs != 0, , drop = FALSE], 3)
write.table(ridge_lambda_min, "tables/ridge_lambda_min.txt", sep = "\t", quote = FALSE)

cat("Lambda (min):", round(ridge_model$lambda.min,4), "\n")
cat("Lambda (1se):", round(ridge_model$lambda.1se, 3), "\n")

# ===== Heat Map ===== #
coef_data = data.frame(
  model = rep(c("Standard Logistic", "LASSO", "Group LASSO", "Ridge"), each = n),
  coef = rep(row_labels, 4),
  value = c(logit_coefs$Est, lasso_coefs$lambda.min, group_lasso_coefs$lambda.min, ridge_coefs$lambda.min)
)
lmin = c(lasso_coefs$lambda.min, group_lasso_coefs$lambda.min, ridge_coefs$lambda.min)
lmin_labels = ifelse(lmin != 0, signif(lmin, 2), NA)
coef_labels = paste(logit_coefs$Est, ifelse(logit_coefs$Pr < 0.05, "*", ""))
coef_data$label = c(coef_labels, lmin_labels)

coef_data = coef_data[coef_data$coef != "Intercept", ]

png("plots/coefficient_heatmap.png", width = 850, height = 600)
ggplot(coef_data, aes(x = factor(model, levels = c("Standard Logistic", "LASSO", "Group LASSO", "Ridge")), 
                      y = factor(coef, levels = row_labels), fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(name = "", low = "steelblue3", mid = "white", high = "brown3", midpoint = 0,
                       na.value = "grey85", guide = "colourbar", aesthetics = "fill") + 
  labs(x = "", y = "") +
  scale_y_discrete(limits = rev) +
  geom_vline(xintercept = 1.5, color = "black", linewidth = 0.2) + 
  geom_vline(xintercept = c(2.5, 3.5), color = "black", linewidth = 0.25) + 
  geom_text(aes(label = label)) +
  theme_classic()
dev.off()
