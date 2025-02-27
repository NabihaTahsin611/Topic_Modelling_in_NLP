library(dplyr)

data <- read.csv("C:/Users/NABIHA TAHSIN/Documents/Data Science/diamonds_data.csv")

str(data)

Null_Value <- colSums(is.na(data))
Null_Value

target_attribute <- "price"

#ANOVA
#ANOVA for numeric variables for target attribute
anova_results <- lapply(names(data), function(var) {
  if (is.numeric(data[[var]]) && var != target_attribute) {
    formula <- as.formula(paste(target_attribute, "~", var))
    anova_result <- summary(aov(formula, data = data))
    return(data.frame(
      Variable = var,
      P_Value = anova_result[[1]]["Pr(>F)"][1]
    ))
  }
}) %>% bind_rows()

print(anova_results)

#ANOVA for categorical values
anova_cut <- aov(price ~ cut, data = data)
print(anova_cut)
anova_p_cut <- summary(anova_cut)[[1]]["cut", "Pr(>F)"]
anova_color <- aov(price ~ color, data = data)
print(anova_color)
anova_p_color <- summary(anova_color)[[1]]["color", "Pr(>F)"]
anova_clarity <- aov(price ~ clarity, data = data)
print(anova_clarity)
anova_p_clarity <- summary(anova_clarity)[[1]]["clarity", "Pr(>F)"]

anova_p <- rbind(anova_p_cut, anova_p_color, anova_p_clarity)
print(anova_p)


# Kendall's, Spearman's, and Pearson's correlations
correlation_results <- lapply(names(data), function(var) {
  if (is.numeric(data[[var]]) && var != target_attribute) {
    kendall_corr <- cor.test(data[[var]], data[[target_attribute]], method = "kendall")
    spearman_corr <- cor.test(data[[var]], data[[target_attribute]], method = "spearman")
    pearson_corr <- cor.test(data[[var]], data[[target_attribute]], method = "pearson")
    return(data.frame(
      Variable = var,
      Kendall_P_Value = kendall_corr$p.value,
      Spearman_P_Value = spearman_corr$p.value,
      Pearson_P_Value = pearson_corr$p.value
    ))
  }
}) %>% bind_rows()

print(correlation_results)

