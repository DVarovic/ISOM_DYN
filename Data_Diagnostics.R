#Exploratory analyses to ensure data does not violate model assumptions:

#MID-THIGH

# Check normality and variability of data
boxplot(muscle.thickness~condition, data=data_mid_thigh)

# Check for approximately linear relationship between response variable and covariate
ggplot(data_mid_thigh, aes(y = muscle.thickness, x = Pre, group = condition)) + geom_point() + geom_smooth(method = "lm")

# Check for homogeneity of regression slope: 
data_mid_thigh %>% anova_test(muscle.thickness ~ condition*Pre) #Is the interaction between the covariate and grouping variable statistically significant? NO.

#SIDE-THIGH
boxplot(muscle.thickness~condition, data=data_side_thigh) 

ggplot(data_side_thigh, aes(y = muscle.thickness, x = Pre, group = condition)) + geom_point() + geom_smooth(method = "lm")

data_side_thigh %>% anova_test(muscle.thickness ~ condition*Pre) #NO.



#Leave one out sensitivity analysis suggests no major outliers, 
#however, by visually inspecting individual plots, it looks there are more
loo_results <- loo(MT_model)
print(loo_results)
plot(loo_results) 
high_k <- which(loo_results$diagnostics$pareto_k > 0.7)
print(high_k)


# Extract residuals (posterior median)
data_with_res <- cbind(
  data_mid_thigh,
  residuals = residuals(MT_model)[, "Estimate"],
  fitted = fitted(MT_model)[, "Estimate"]       
)

# Plot residuals vs fitted values
ggplot(data_with_res, aes(x = fitted, y = residuals)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )



# Scatterplot for muscle thickness vs Pre (stratified by condition and site)
ggplot(data_mid_thigh, aes(x = Pre, y = muscle.thickness, color = condition)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~ site, scales = "free") +
  theme_minimal() +
  labs(
    title = "Muscle Thickness vs Pre (Stratified by Site)",
    x = "Pre",
    y = "Muscle Thickness"
  )

# Calculate the threshold for outliers (3 SD from the mean of residuals)
threshold <- 3 * sd(data_with_res$residuals)
data_with_res <- data_with_res %>%
  mutate(outlier = abs(residuals) > threshold)

# Display observations flagged as outliers
print(data_with_res %>% filter(outlier == TRUE))

ggplot(data_with_res, aes(y = residuals)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Boxplot of Residuals",
    y = "Residuals"
  )


# Check normality and variability of data
boxplot(muscle.thickness~condition, data=mid_side_thigh_master)

# Check for approximately linear relationship between response variable and covariate
ggplot(mid_side_thigh_master, aes(y = muscle.thickness, x = Pre, group = condition)) + geom_point() + geom_smooth(method = "lm")

# Check for homogeneity of regression slope: 
mid_side_thigh_master %>% anova_test(muscle.thickness ~ condition*Pre) #Is the interaction between the covariate and grouping variable statistically significant? NO.

