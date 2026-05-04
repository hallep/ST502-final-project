library(pls)
library(glmnet)
library(pROC)
library(ggplot2)
library(patchwork)

# Regression Data
data = read.table("regression_variables.txt", sep="\t", header = TRUE, row.names = "id", na.strings = "nan")
vars = data[, !(names(data) %in% "id")]
n = ncol(vars) - 1

# split into train vs. test
set.seed(777)

idx_0 = which(vars$highBP == 0)
idx_1 = which(vars$highBP == 1)

train_0 = sample(idx_0, size = 0.7 * length(idx_0))
train_1 = sample(idx_1, size = 0.7 * length(idx_1))

trainIdx <- c(train_0, train_1)

trainData = vars[trainIdx, ]
testData = vars[-trainIdx, ]

# ===== PCR model ===== #
model = pcr(highBP ~ ., data = trainData, ncomp = n, scale = TRUE, validation = "CV")

m = 9

# Regression Explanatory Power
rmse = RMSEP(model)$val
varY = data.frame(
  "ncomps" = 0:n,
  "RMSE.CV" = rmse["CV", , ],
  "RMSE.adjCV" = rmse["adjCV", , ],
  "R2" = R2(model)$val["CV", , ]
)

pErr = ggplot(varY, aes(x = ncomps)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(aes(y = RMSE.CV, color = "RMSE", linetype = "RMSE"), linewidth = 0.65) +
  geom_line(aes(y = RMSE.adjCV, color = "Adjusted RMSE", linetype = "Adjusted RMSE"), linewidth = 0.65) +
  labs(title = "Root Mean Square Error of Prediction", x = "Number of Components", y = "RMSE") +
  scale_color_manual(name = "", breaks = c("RMSE", "Adjusted RMSE"), 
                     values = c("RMSE" = "black", "Adjusted RMSE" = "red")) + 
  scale_linetype_manual(name = "", breaks = c("RMSE", "Adjusted RMSE"), 
                        values = c("RMSE" = "solid", "Adjusted RMSE" = "dashed")) +
  theme_classic() + theme(legend.title = element_blank(), legend.justification = c(1, 1), legend.position = c(1, 1),
                          legend.background = element_rect(fill = "transparent", color = NA))

pR2 = ggplot(varY, aes(x = ncomps, y = R2)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(color = "mediumorchid", linewidth = 0.8) +
  labs(title = bquote("Coefficient of Determination" ~ (R^2)), 
       x = "Number of Components", y = expression(R^2)) +
  theme_classic()

# Components
varX = data.frame(
  "ncomps" = 1:n,
  "exp" = explvar(model),
  "cum" = cumsum(explvar(model))/16
)
explvar(model)
pX = ggplot(varX, aes(x = ncomps)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(aes(y = exp, color = "Explained Variance", linetype = "Explained Variance"), linewidth = 0.6) +
  geom_line(aes(y = cum, color = "Cumulative Variance", linetype = "Cumulative Variance"), linewidth = 0.8) +
  scale_y_continuous(
    name = "Variance (%)",
    sec.axis = sec_axis(~.*16, name = "Cumulative Variance (%)")
  ) +
  scale_color_manual(name = "", breaks = c("Explained Variance", "Cumulative Variance"),
                     values = c("Explained Variance" = "black", "Cumulative Variance" = "dodgerblue")) + 
  scale_linetype_manual(name = "", breaks = c("Explained Variance", "Cumulative Variance"),
                        values = c("Explained Variance" = "dashed", "Cumulative Variance" = "solid")) + 
  labs(title = "Variance Explained by Components", x = "Component") +
  theme_classic() + theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0.1, 1),
                          legend.background = element_rect(fill = "transparent", color = NA))

var_names = c("age", "gender", "black", "mexAmer", "hispanic", "asian", "otherRace", "someHS", "HSGrad", "someCollege", "collegeGrad", "married", "widowed", "divorced", "separated", "livingWithPartner", "incomePoverty", "dailyAlc", "weeklyAlc", "monthlyAlc", "yearlyAlc", "smokeEveryDay", "smokeSomeDays", "fairDiet", "goodDiet", "veryGoodDiet", "excellentDiet", "sleepWeekdays", "sleepWeekends", "vigWork", "modWork", "vigRec", "modRec", "sedentary", "bmi", "pulse", "cholesterol", "ferritin", "diabetes", "thyroidProblem")
var_labels = c("Age", "Gender", "Black", "Mexican American", "Hispanic", "Asian", "Other/mixed race", "Some high school", "High school graduate", "Some college", "College graduate", "Married", "Widowed", "Divorced", "Separated", "Living with partner", "Income-poverty ratio", "Drink daily", "Drink weekly", "Drink monthly", "Drink yearly", "Smoke every day", "Smoke some days", "Fair diet", "Good diet", "Very good diet", "Excellent diet", "Sleep on weekdays", "Sleep on weekends", "Vigorous work activity", "Moderate work activity", "Vigorous recreational activity", "Moderate recreational activity", "Sedentary activity", "BMI", "Pulse", "Cholesterol", "Ferritin", "Diabetes", "Thyroid problems")

l = model$loadings[rev(var_names), 1:n]
load = data.frame(
  "variable" = factor(rep(rownames(l), ncol(l)), levels = as.vector(rownames(l))),
  "component" = rep(1:n, each = nrow(l)),
  "load" = stack(as.data.frame(l))$values
)

pLoad = ggplot(load, aes(x = component, y = variable, fill = load)) + 
  geom_tile() + 
  scale_fill_gradient2(name = "Load", low = "steelblue3", mid = "white", high = "brown3", midpoint = 0,
                       na.value = "grey85", guide = "colourbar", aesthetics = "fill") + 
  labs(x = "Component", y = "Variable") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_discrete(labels = rev(var_labels)) + coord_fixed() +
  theme_classic()
pLoad

# ===== Predict ===== #

pred_pcr = predict(model, testData, ncomp=m)

# ROC
roc_pcr = roc(testData$highBP, pred_pcr)
auc_pcr = auc(roc_pcr)

curve = data.frame(
  "Specificity" = roc_pcr$specificities,
  "Sensitivity" = roc_pcr$sensitivities
)

pROC = ggplot(curve, aes(x = Specificity, y = Sensitivity)) + 
  geom_abline(intercept = 1, slope = 1, color = "dimgray", linewidth = 0.5) +
  geom_line(color = "darkorange", linewidth = 0.8) + 
  labs(title = "ROC Curve (Test Set)", x = "Specificity", y = "Sensitivity") +
  scale_x_reverse() + theme_classic()

# AUC
model_full = pcr(highBP ~ ., data = trainData, ncomp = n, scale = TRUE, validation = "CV")

x = c()
y = c()

for (i in n:1) {
  pred = predict(model_full, testData, ncomp=i)
  roc_obj = roc(testData$highBP, pred)
  
  x = c(x, i)
  y = c(y, auc(roc_obj))
}

area = data.frame("ncomps" = x, "auc" = y)

pAUC = ggplot(area, aes(x = ncomps, y = auc)) + 
  geom_vline(xintercept = m, color = "dimgray", linewidth = 0.5, linetype = "dashed") +
  geom_line(color = "hotpink", linewidth = 0.8) + 
  labs(title = "Area Under Curve", x = "Number of Components", y = "AOC") +
  theme_classic()

# ===== Plots ===== #

png("plots/group_lasso_roc_test.png", width = 1500, height = 750)
design = c(
  area(1, 1, 2, 2), area(3, 2), 
  area(1, 3), area(2, 3), area(3, 3))
free(pLoad) + pX + pErr + pR2 + pAUC + plot_layout(design = design, widths = c(1, 3, 3))
dev.off()
