# Function to calculate correlation coefficients
calculate_correlation <- function(pre, post) {
  if(length(pre) > 1 && length(post) > 1) {
    return(cor(pre, post, use = "pairwise.complete.obs"))
  } else {
    return(NA) # Return NA if not enough data
  }
}

# Calculate correlations by condition, muscle, and site
correlations <- mid_side_thigh_master %>%
  group_by(condition, muscle, site) %>%
  summarise(
    correlation = calculate_correlation(Pre, Post),
    .groups = "drop"
  )

# View the results
print(correlations)


# MID-THIGH DATA FRAME
MT_data <- data.frame(
  Condition = c("DYN", "DYN", "DYN", "ISOM", "ISOM", "ISOM"),
  Site = c(30, 50, 70, 30, 50, 70),
  Mean_Pre = c(55.17391304, 44.55217391, 31.21521739, 55.36086957, 44.20869565, 30.98695652),
  SD_Pre = c(8.772424118, 8.738817247, 8.757924136, 9.828055349, 8.761455516, 7.460170261),
  Mean_Post = c(55.62173913, 45.19782609, 31.38478261, 56.91956522, 44.97826087, 31.98478261),
  SD_Post = c(10.06004503, 10.25813954, 8.636029638, 10.03955517, 9.099735587, 7.725730839),
  Sample_N = c(23, 23, 23, 23, 23, 23),
  Corr_R = c(0.936, 0.942, 0.965, 0.932, 0.939, 0.924)
)

# Calculate mean change
MT_data$Mean_Change <- data$Mean_Post - data$Mean_Pre

# Calculate effect sizes using escalc with Hedges' g correction
results <- escalc(
  measure = "SMCRH", 
  m1i = Mean_Post, 
  m2i = Mean_Pre, 
  sd1i = SD_Post, 
  sd2i = SD_Pre, 
  ni = Sample_N, 
  ri = Corr_R, 
  data = MT_data
)

# Add 95% CIs to the results
results$ci.lb <- results$yi - 1.96 * sqrt(results$vi) # Lower bound
results$ci.ub <- results$yi + 1.96 * sqrt(results$vi) # Upper bound

# Add Mean Change to the results
results$Mean_Change <- data$Mean_Change

# View results with Hedges' g, Mean Change, and 95% CIs
print(results)

#------------------------ ATTEMPT TO MODEL STANDARDIZED MEAN CHANGES ------------------------
#PLOT EFFECT SIZES DO NOT ALIGN WITH THOSE THAT WERE CALCULATED VIA ESCALC in SMD.r file

MT_model_SMD <- brm(standardized.change ~ 1 + Pre + condition + condition:site + (1 | id),
                    data = data_mid_thigh,
                    family = gaussian(),
                    chains = 4,
                    seed = 123,
                    warmup = 2000,
                    iter = 4000,
                    control = list(adapt_delta=0.99))


MT_SMD_effects_condition_site <- emmeans(MT_model_SMD, ~ condition | site, weights = "prop")
hpd.summary(MT_SMD_effects_condition_site, point.est = mean) 

MT_SMD_effects_GG_condition_site <- MT_SMD_effects_condition_site %>% gather_emmeans_draws()

ggplot(MT_SMD_effects_GG_condition_site, aes(x = .value, y = site, fill = condition)) + 
  stat_slabinterval(
    aes(fill = condition),
    .width = 0.95,
    alpha = 0.8,
    slab_outline = TRUE,
    slab_color = "black",  
    slab_size = 0.7       
  ) +
  scale_y_discrete(limits = c("70", "50", "30")) +
  labs(
    title = "Posterior Distributions of Conditions by Site for Mid-thigh",
    x = "Standardized Mean Difference",
    y = element_blank()
  ) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, family = "Helvetica"),
    axis.line = element_line(size = .8, colour = "black"),
    axis.title.x = element_text(size = 12, family = "Helvetica"),
    axis.title.y = element_text(size = 12, family = "Helvetica"),
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, family = "Helvetica"),
    panel.background = element_blank(),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  ) 
