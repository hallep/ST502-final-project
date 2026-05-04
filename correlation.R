library(tidyverse)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)

# Regression Data
vars = read.table("regression_variables.txt", header = TRUE, row.names = "id", sep = "\t", na.strings = "nan")
vars$highBP = factor(vars$highBP, levels = c(0, 1), labels = c("No highBP", "HighBP"))

# Variables
num_names = c("age", "incomePoverty", "sleepWeekdays", "sleepWeekends", "vigWork", "modWork", "vigRec", "modRec", "sedentary", "bmi", "pulse", "cholesterol", "ferritin")
num_labels = c("Age", "Income-poverty ratio", "Sleep on weekdays", "Sleep on weekends", "Vigorous work activity", "Moderate work activity", "Vigorous recreational activity", "Moderate recreational activity", "Sedentary activity", "BMI", "Pulse", "Cholesterol", "Ferritin")
num_vars = df[, num_names]

cat_names = c("gender", "black", "mexAmer", "hispanic", "asian", "otherRace", "someHS", "HSGrad", "someCollege", "collegeGrad", "married", "widowed", "divorced", "separated", "livingWithPartner", "dailyAlc", "weeklyAlc", "monthlyAlc", "yearlyAlc", "smokeEveryDay", "smokeSomeDays", "fairDiet", "goodDiet", "veryGoodDiet", "excellentDiet", "diabetes", "thyroidProblem")
cat_labels = c("Gender", "Black", "Mexican American", "Hispanic", "Asian", "Other/mixed race", "Some high school", "High school graduate", "Some college", "College graduate", "Married", "Widowed", "Divorced", "Separated", "Living with partner", "Drink daily", "Drink weekly", "Drink monthly", "Drink yearly", "Smoke every day", "Smoke some days", "Fair diet", "Good diet", "Very good diet", "Excellent diet", "Diabetes", "Thyroid problems")
cat_vars = df[, cat_names]

# ========== Summary ========== #

# Summary Counts
counts = c(nrow(vars), sum(vars$highBP == "HighBP"), sum(vars$highBP == "No highBP"))
data.frame(num = counts, prop = round(counts / nrow(vars) * 100, 1), row.names = c("total", "highBP", "no highBP"))

# ----- Numerical Summary Statistics ----- #
num_summary = vars %>% 
  group_by(highBP) %>% 
  summarise(across(
    all_of(names(cont_vars)),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = -highBP,
    names_to = c("variable", "statistic"),
    names_pattern = "(.+)_(mean|sd|median|min|max)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

write.table(num_summary, "tables/numeric_summary_by_highBP.txt", sep = "\t", row.names = FALSE, quote = FALSE)

png("plots/correlation_matrix.png", width = 750, height = 750)
cor_matrix <- cor(num_vars, use = "complete.obs")
colnames(cor_matrix) = num_labels
rownames(cor_matrix) = num_labels
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")
dev.off()

# ========== Statistical Tests ========== #

# ----- Categorical Chi-Squared ----- #
chi_results = list()

for (i in 1:(length(cat_names)-1)) {
  for (j in (i+1):length(cat_names)) {
    test = chisq.test(vars[[cat_names[i]]], vars[[cat_names[j]]])
    chi_results[[paste(cat_labels[i], " v. ", cat_labels[j], sep = "")]] = test$p.value
  }
}

# Convert to table
chi_pvalues = data.frame(
  pair = names(chi_results),
  p_value = unlist(chi_results)
)

chi_sig = chi_pvalues %>%
  filter(p_value < 0.05) %>%
  mutate(log_p = -log10(p_value)) %>%
  arrange(desc(log_p)) %>%
  slice_head(n = 20)

png("plots/cat_cat_chi_squared.png", width = 750, height = 450)
ggplot(chi_sig, aes(x = reorder(pair, log_p), y = log_p)) +
  geom_col() + coord_flip() +
  labs(title = "", x = "", y = "-log10(p-value)") +
  theme_classic()
dev.off()

# ----- Numerical vs. Categorical T-Test ----- #
results <- data.frame()

for (i in 1:length(num_names)) {
  for (j in 1:length(cat_names)) {
    
    # only do test if categorical has 2 levels
    if (length(unique(df[[cat_names[j]]])) == 2) {
      test <- t.test(df[[num_names[i]]] ~ df[[cat_names[j]]])
      
      results = rbind(results, data.frame(
        numeric = num_labels[i],
        categorical = cat_labels[j],
        p_value = test$p.value
      ))
    }
  }
}

t_sig = results %>%
  filter(p_value < 0.05) %>%
  mutate(log_p = -log10(p_value)) %>%
  arrange(desc(log_p)) %>%
  slice_head(n = 20)

png("plots/num_cat_ttest.png", width = 750, height = 450)
ggplot(t_sig, aes(x = reorder(paste(numeric, categorical, sep = " v. "), log_p), y = log_p)) +
  geom_col() + coord_flip() +
  labs(title = "", x = "", y = "-log10(p-value)") +
  theme_classic()
dev.off()

