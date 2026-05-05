library(glmnet)
library(pROC)
library(ggplot2)
library(patchwork)
library(performance)

# Regression Data
vars = read.table("regression_variables.txt", sep="\t", header = TRUE, row.names = "id", na.strings = "nan")
n = ncol(vars) - 1

X = vars[, !(names(vars) %in% "highBP")]
y = factor(as.numeric(vars$highBP))

# split into train vs. test
set.seed(777)

idx_0 = which(vars$highBP == 0)
idx_1 = which(vars$highBP == 1)

train_0 = sample(idx_0, size = 0.7 * length(idx_0))
train_1 = sample(idx_1, size = 0.7 * length(idx_1))

trainIdx <- c(train_0, train_1)

# Training Data
X_train = X[train_idx, ]
y_train = y[train_idx]

# Test Data
X_test = X[-train_idx, ]
y_test = y[-train_idx]

# ===== Principal Component Regression ===== #

m = 9

# ===== Principal Component Analysis ===== #
pca = prcomp(X_train, center = TRUE, scale = TRUE)
comps = pca$x

eigen = pca$rotation
X_test_pca = as.data.frame(predict(pca, newdata = X_test))
import = summary(pca)$importance

# ----- Component Composition ----- #
var_names = c("age", "gender", "black", "mexAmer", "hispanic", "asian", "otherRace", "someHS", "HSGrad", "someCollege", "collegeGrad", "married", "widowed", "divorced", "separated", "livingWithPartner", "incomePoverty", "dailyAlc", "weeklyAlc", "monthlyAlc", "yearlyAlc", "smokeEveryDay", "smokeSomeDays", "fairDiet", "goodDiet", "veryGoodDiet", "excellentDiet", "sleepWeekdays", "sleepWeekends", "vigWork", "modWork", "vigRec", "modRec", "sedentary", "bmi", "pulse", "cholesterol", "ferritin", "diabetes", "thyroidProblem")
var_labels = c("Age", "Gender", "Black", "Mexican American", "Hispanic", "Asian", "Other/mixed race", "Some high school", "High school graduate", "Some college", "College graduate", "Married", "Widowed", "Divorced", "Separated", "Living with partner", "Income-poverty ratio", "Drink daily", "Drink weekly", "Drink monthly", "Drink yearly", "Smoke every day", "Smoke some days", "Fair diet", "Good diet", "Very good diet", "Excellent diet", "Sleep on weekdays", "Sleep on weekends", "Vigorous work activity", "Moderate work activity", "Vigorous recreational activity", "Moderate recreational activity", "Sedentary activity", "BMI", "Pulse", "Cholesterol", "Ferritin", "Diabetes", "Thyroid problems")

l = eigen[rev(var_names), 1:n]
load = data.frame(
  "variable" = factor(rep(rownames(l), ncol(l)), levels = as.vector(rownames(l))),
  "component" = rep(1:n, each = nrow(l)),
  "load" = stack(as.data.frame(l))$values
)

pLoad = ggplot(load, aes(x = component, y = variable, fill = load)) + 
  geom_tile() + 
  scale_fill_gradient2(name = "Load", low = "steelblue3", mid = "white", high = "brown3", midpoint = 0,
                       na.value = "grey85", guide = "colourbar", aesthetics = "fill") + 
  labs(title = "Component Composition", x = "Component", y = "") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_discrete(labels = rev(var_labels)) + coord_fixed() +
  theme_classic()
pLoad

# ----- Variance in X ----- #
varX = data.frame(
  ncomps = 1:n,
  variance = import["Proportion of Variance", ],
  cumvar = import["Cumulative Proportion", ] / 16
)

pX = ggplot(varX, aes(x = ncomps)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(aes(y = variance, color = "Explained Variance", linetype = "Explained Variance"), linewidth = 0.6) +
  geom_line(aes(y = cumvar, color = "Cumulative Variance", linetype = "Cumulative Variance"), linewidth = 0.8) +
  scale_y_continuous(
    name = "Variance (%)",
    sec.axis = sec_axis(~.*16, name = "Cumulative Variance (%)")
  ) +
  scale_color_manual(name = "", breaks = c("Explained Variance", "Cumulative Variance"),
                     values = c("Explained Variance" = "black", "Cumulative Variance" = "dodgerblue")) + 
  scale_linetype_manual(name = "", breaks = c("Explained Variance", "Cumulative Variance"),
                        values = c("Explained Variance" = "dashed", "Cumulative Variance" = "solid")) + 
  labs(title = "Variance Explained by Components", x = "Component") +
  theme_classic() + theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0.25, 1),
                          legend.background = element_rect(fill = "transparent", color = NA))
pX

# ===== Logistic Regression ===== #

x = c()
y_rmse = c()
y_auc = c()

roc_n = NULL
roc_m = NULL

for (i in n:1) {
  x = c(x, i)
  
  if (i == 1) {
    train_data = data.frame(y = y_train, PC1 = comps[, 1])
    test_data = data.frame(PC1 = X_test_pca[, 1])
  } else {
    train_data = data.frame(y = y_train, comps[, 1:i])
    test_data = X_test_pca[, 1:i]
  }
  
  # train model
  model = glm(y ~ ., data = train_data, family = binomial)
  y_rmse = c(y_rmse, rmse(model))
  
  # predict
  pred = predict(model, newdata = test_data, type = "response")
  roc_obj = roc(y_test, pred)
  y_auc = c(y_auc, auc(roc_obj))
  
  if (i == n) {
    roc_n = roc_obj
  } else if (i == m) {
    roc_m = roc_obj
  }
  
}

varY = data.frame(
  ncomps = x,
  rmse = y_rmse,
  auc = y_auc
)

# ----- Variance in Y ----- #
pRMSE = ggplot(varY, aes(x = ncomps, y = rmse)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(color = "darkorchid", linewidth = 0.65) +
  labs(title = "Root Mean Square Error of Prediction", y = "RMSE", x = "Number of Components") +
  theme_classic()
pRMSE

# ----- Prediction ----- #
png("roc_curves/pcr_roc_test.png", width = 500, height = 500)
plot(roc_n, col = "brown", lwd = 2)
plot(roc_m, col = "darkorange", lwd = 2, add = TRUE)

legend("bottomright", legend = c("All Components", paste(m, "Components")),
       col = c("brown", "darkorange"), lwd = 2)
dev.off()

pAUC = ggplot(varY, aes(x = ncomps, y = auc)) + 
  geom_vline(xintercept = m, color = "dimgray", linewidth = 0.5, linetype = "dashed") +
  geom_line(color = "hotpink", linewidth = 0.8) + 
  labs(title = "Area Under Curve", x = "Number of Components", y = "AUC") +
  theme_classic()
pAUC

# ===== Plots ===== #

png("plots/pcr_summary.png", width = 1100, height = 550)
pLoad + (pX / pRMSE / pAUC) + plot_layout(widths = c(3, 2))
dev.off()
