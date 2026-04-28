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
model <- pcr(highBP ~ ., data = trainData, ncomp = n, scale = TRUE, validation = "CV")

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
  scale_color_manual(name = "", breaks = c("RMSE", "Adjusted RMSE"), values = c("RMSE" = "black", "Adjusted RMSE" = "red")) + 
  scale_linetype_manual(name = "", breaks = c("RMSE", "Adjusted RMSE"), values = c("RMSE" = "solid", "Adjusted RMSE" = "dashed")) +
  theme_classic() + theme(legend.justification = c(1, 1), legend.position = c(0.9, 1),
                          legend.background = element_rect(fill = "transparent", color = NA))
  
pR2 = ggplot(varY, aes(x = ncomps, y = R2)) + 
  geom_vline(xintercept = m, color="dimgrey", linewidth = 0.5, linetype = "dashed") + 
  geom_line(color = "mediumorchid", linewidth = 0.8) +
  labs(title = bquote("Coefficient of Determination" ~ (R^2)), x = "Number of Components", y = expression(R^2)) +
  theme_classic()

# Components
varX = data.frame(
  "ncomps" = rep(1:n, 2),
  "exp" = explvar(model),
  "cum" = cumsum(explvar(model))/16
)

pX = ggplot(varX, aes(x = ncomps)) + 
  geom_line(aes(y = exp, color = "Explained Variance"), linewidth = 0.8) +
  geom_line(aes(y = cum, color = "Cumulative Variance"), linewidth = 0.8) +
  scale_y_continuous(
    name = "Variance (%)",
    sec.axis = sec_axis(~.*16, name = "Cumulative Variance (%)")
  ) +
  scale_color_manual(breaks = c("Explained Variance", "Cumulative Variance"), values = c("Explained Variance" = "black", "Cumulative Variance" = "red")) + 
  labs(title = "Variance of Variables Explained by Components", x = "Component") +
  theme_classic() + theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0.1, 1),
                          legend.background = element_rect(fill = "transparent", color = NA))

load = data.frame(
  "Variable" = 1:(ncol(vars) - 1),
  "c1" = model$loadings[, 1],
  "c2" = model$loadings[, 2],
  "c3" = model$loadings[, 3]
)

pLoad = ggplot(load, aes(x = Variable)) + 
  geom_line(aes(y = c1, color = "Component 1"), linewidth = 0.6) +
  geom_line(aes(y = c2, color = "Component 2"), linewidth = 0.6) +
  geom_line(aes(y = c3, color = "Component 3"), linewidth = 0.6) +
  labs(title = "Transformation of Variables into Components", x = "Variable", y = "Loading") +
  scale_color_manual(name = "", values = c("Component 1" = "steelblue4", "Component 2" = "darkorange", "Component 3" = "green4")) + 
  theme_classic() + theme(legend.justification = c(1, 0), legend.position = c(1, 0),
                          legend.background = element_rect(fill = "transparent", color = NA))

# Organize
(pErr + pR2) / (pX + pLoad)

# pErr
# pR2
# pX
# pLoad

# ===== Predict ===== #

pred_pcr = predict(model, testData, ncomp=m)

# ROC
roc_pcr = roc(testData$highBP, pred_pcr)
auc_pcr = auc(roc_pcr)
plot(roc_pcr, main = "Principle Component Regression ROC Curve (Test Set)", col = "darkorange", lwd = 2)

