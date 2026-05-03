library(tidyverse)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_tsv("regression_variables.txt")   

# ========== Step 1 ===========
# Numeric continuous variables only
num_vars <- df[, sapply(df, function(x) {
  is.numeric(x) && length(unique(na.omit(x))) > 2
})]
# remove specific variables
num_vars <- num_vars[, !(names(num_vars) %in% c("id"))]

# Categorical variables
cat_vars <- df[, sapply(df, function(x) {
  is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 2
})]

# ========== Summary ==========
# Overall sample size
nrow(df)

# Sample size by hypertension status
table(df$highBP)

# Percent by hypertension status
round(prop.table(table(df$highBP)) * 100, 1)

# --- Make sure highBP is treated as categorical ---
df$highBP <- factor(df$highBP, levels = c(0, 1), labels = c("No highBP", "HighBP"))
# Numeric summary statistics by highBP
numeric_summary_long <- df %>%
  group_by(highBP) %>%
  summarise(across(
    all_of(names(num_vars)),
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

write.csv(numeric_summary_long,
          "numeric_summary_by_highBP.csv",
          row.names = FALSE)

# --- Categorical summary statistics by highBP --- 
# Remove outcome from categorical predictors 
cat_vars_summary <- cat_vars[, !(names(cat_vars) %in% c("highBP"))]

cat_summary <- data.frame()

for (v in names(cat_vars_summary)) {
  
  tab <- table(df[[v]], df$highBP, useNA = "ifany")
  percent <- prop.table(tab, margin = 2) * 100
  
  temp <- as.data.frame(tab)
  names(temp) <- c("level", "highBP", "count")
  
  temp$percent <- as.vector(percent)
  temp$variable <- v
  
  cat_summary <- rbind(cat_summary, temp)
}

cat_summary <- cat_summary %>%
  select(variable, level, highBP, count, percent) %>%
  arrange(variable, level, highBP)

cat_summary_clean <- cat_summary %>%
  filter(level == 1) %>%
  mutate(
    count_percent = paste0(count, " (", round(percent, 1), "%)")
  ) %>%
  select(variable, highBP, count_percent) %>%
  pivot_wider(
    names_from = highBP,
    values_from = count_percent
  )

# ==== numeric vs numeric Correlation matrix ==== 
cor_matrix <- cor(num_vars, use = "complete.obs")


corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.8,
         addCoef.col = "black")

# ==== categorical vs categorical ==== 
cat_names <- names(cat_vars)

chi_results <- list()

for (i in 1:(length(cat_names)-1)) {
  for (j in (i+1):length(cat_names)) {
    var1 <- cat_names[i]
    var2 <- cat_names[j]
    
    test <- chisq.test(df[[var1]], df[[var2]])
    
    chi_results[[paste(var1, var2, sep = "_")]] <- test$p.value
  }
}

# Convert to table
chi_pvalues <- data.frame(
  pair = names(chi_results),
  p_value = unlist(chi_results)
)

# Show significant ones
subset(chi_pvalues, p_value < 0.05)

# Filter meaningful pairs
subset(chi_pvalues,
       p_value < 0.05 &
         grepl("highBP", pair))

chi_top <- chi_pvalues %>%
  filter(p_value < 0.05) %>%
  mutate(log_p = -log10(p_value)) %>%
  arrange(desc(log_p)) %>%
  slice_head(n = 20)

ggplot(chi_top, aes(x = reorder(pair, log_p), y = log_p)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Categorical-Categorical Associations",
    x = "Variable Pair",
    y = "-log10(p-value)"
  ) +
  theme_minimal()

# ==== numeric vs categorical t-test ==== 
num_names <- names(num_vars)
cat_names <- names(cat_vars)

results <- data.frame()

for (num in num_names) {
  for (cat in cat_names) {
    
    # only do test if categorical has 2 levels
    if (length(unique(df[[cat]])) == 2) {
      test <- t.test(df[[num]] ~ df[[cat]])
      
      results <- rbind(results, data.frame(
        numeric = num,
        categorical = cat,
        p_value = test$p.value
      ))
    }
  }
}

# Show important ones
subset(results, p_value < 0.05)


# only look at our Y-related
highBP_num_results <- subset(results, categorical == "highBP")
# including p_value > 0.05
highBP_num_results[order(highBP_num_results$p_value), ]

t_top <- results %>%
  filter(p_value < 0.05) %>%
  mutate(log_p = -log10(p_value)) %>%
  arrange(desc(log_p)) %>%
  slice_head(n = 20)

ggplot(t_top, aes(x = reorder(paste(numeric, categorical, sep = " vs "), log_p),
                  y = log_p)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Numeric-Categorical Associations",
    x = "Variable Pair",
    y = "-log10(p-value)"
  ) +
  theme_minimal()
