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

m = 4

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
  "ncomps" = rep(1:n, 2),
  "exp" = explvar(model),
  "cum" = cumsum(explvar(model))/16
)

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

l = model$loadings[, 1:n]
load = data.frame(
  "variable" = factor(rep(rownames(l), ncol(l)), levels = as.vector(rownames(l))),
  "component" = rep(1:n, each = nrow(l)),
  "load" = stack(as.data.frame(l))$values
)

pLoad = ggplot(load, aes(x = component, y = variable, fill = load)) + 
  geom_tile() + 
  scale_fill_gradient2(name = "Load", low = "steelblue3", mid = "white", high = "brown3", midpoint = 0,
                       na.value = "grey85", guide = "colourbar", aesthetics = "fill") + 
  labs(title = "Component Composition", x = "Component", y = "Variable") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_discrete(limits = rev) +
  coord_fixed() +
  theme_classic() + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

# ===== Predict ===== #

pred_pcr = predict(model, testData, ncomp=n)

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
  labs(title = "Area Under Curve", x = "Number of Components", y = "Area Under Curve") +
  theme_classic()

# ===== Plots ===== #

(pErr + pR2) / (pROC + pAUC)
pX
pLoad
