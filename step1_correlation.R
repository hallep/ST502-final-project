library(tidyverse)
library(corrplot)
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
#subset(chi_pvalues, p_value < 0.05)

# Filter meaningful pairs
subset(chi_pvalues,
       p_value < 0.05 &
         grepl("highBP", pair))



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
#subset(results, p_value < 0.05)


# only look at our Y-related
highBP_num_results <- subset(results, categorical == "highBP")
# including p_value > 0.05
highBP_num_results[order(highBP_num_results$p_value), ]

