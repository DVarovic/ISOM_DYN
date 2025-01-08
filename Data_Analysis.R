#muscle.thickness ~ 1 + Pre + condition + condition:site + (1 | id) #This model was specified in pre-reg 
                                                                    #But, I also mentioned that both participant and site 
                                                                    #will be added as random intercepts, so these did not align from the very start

#muscle.thickness ~ 1 + Pre + muscle:condition:site + (1 | id) #Three-way interaction, chains did not mix well, 
                                                               #and model did not converge well

# ----------------------------- MODEL FITTING -----------------------------
model <- brm(muscle.thickness ~ 1 + 
                  (condition * muscle) + 
                  (condition:site ) + 
                  (muscle * site + Pre ) + (1 | id),
                data = mid_side_thigh_master,
                family = gaussian(),
                chains = 4,
                seed = 123,
                warmup = 2000,
                iter = 4000,
                control = list(adapt_delta=0.99)) #This model still did not converge well as well


#To make it easier, two models were generated, one for each muscle, and I went with a model you (James) suggested
data_mid_thigh <- mid_side_thigh_master %>% filter(muscle == "Mid-thigh")

MT_model <- brm(muscle.thickness ~ 1 + Pre + condition + condition:site + (1 | id),
                data = data_mid_thigh,
                family = gaussian(),
                chains = 4,
                seed = 123,
                warmup = 2000,
                iter = 4000,
                control = list(adapt_delta=0.99))

summary(MT_model)
plot(MT_model)
pp = brms::pp_check(MT_model)
pp + theme_bw()


data_side_thigh <- mid_side_thigh_master %>% filter(muscle == "Side-thigh")

ST_model <- brm(muscle.thickness ~ 1 + Pre + condition + condition:site + (1 | id),
                data = data_side_thigh,
                family = gaussian(),
                chains = 4,
                seed = 123,
                warmup = 2000,
                iter = 4000,
                control = list(adapt_delta=0.99))

# ----------------------------- EXTRACT MODEL EFFECTS -----------------------------
#MID THIGH--------------------------------------------------------------------
MT_effects <- emmeans(MT_model, ~ condition, weights = "prop")
hpd.summary(MT_effects, point.est = mean)

MT_effects_GG <- emmeans(MT_model, spec = ~condition, weights = "prop") %>% gather_emmeans_draws() 

MT_effects_contrast <- pairs(emmeans(MT_model, ~ condition, weights = "prop"))
hpd.summary(MT_effects_contrast, point.est = mean)

MT_effects_contrast_GG <- pairs(emmeans(MT_model, ~ condition, weights = "prop")) %>% gather_emmeans_draws()

# By condition and site
MT_effects_condition_site <- emmeans(MT_model, ~ condition | site, weights = "prop")
hpd.summary(MT_effects_condition_site, point.est = mean) 

MT_effects_GG_condition_site <- MT_effects_condition_site %>% gather_emmeans_draws()

#Contrast for each condition at specific site
MT_effects_contrast_site <- pairs(MT_effects_condition_site)
hpd.summary(MT_effects_contrast_site, point.est = mean) 

MT_effects_GG_contrast_site <- MT_effects_contrast_site %>% gather_emmeans_draws()

#SIDE THIGH--------------------------------------------------------------------
ST_effects <- emmeans(ST_model, ~ condition, weights = "prop")
hpd.summary(ST_effects, point.est = mean)
ST_effects_GG <- emmeans(ST_model, spec = ~condition, weights = "prop") %>% gather_emmeans_draws() 

ST_effects_contrast <- pairs(emmeans(ST_model, ~ condition, weights = "prop"))
hpd.summary(ST_effects_contrast, point.est = mean)
ST_effects_contrast_GG <- pairs(emmeans(ST_model, ~ condition, weights = "prop")) %>% gather_emmeans_draws()

# By condition and site
ST_effects_condition_site <- emmeans(ST_model, ~ condition | site, weights = "prop")
hpd.summary(ST_effects_condition_site, point.est = mean) 
ST_effects_GG_condition_site <- ST_effects_condition_site %>% gather_emmeans_draws()

#Contrast for each condition at specific site
ST_effects_contrast_site <- pairs(ST_effects_condition_site)
hpd.summary(ST_effects_contrast_site, point.est = mean) 
ST_effects_GG_contrast_site <- ST_effects_contrast_site %>% gather_emmeans_draws()

# ----------------------------- EFFECT ESTIMATE PROBABILITY  -----------------------------

#MID-THIGH--------------------------------------------------------------------
# By condition
emmeans(MT_model, ~ condition, weights = "prop")%>%
  gather_emmeans_draws()%>%
  group_by(condition)%>%
  summarise(pd=round(mean(.value>0,na.rm=TRUE)*100,0))

# By condition over TE
emmeans(MT_model, ~ condition, weights = "prop")%>%
  gather_emmeans_draws()%>%
  group_by(condition)%>%
  summarise(pd=round(mean(.value>TE_midthigh_comb,na.rm=TRUE)*100,0))

# By condition and site over TE
emmeans(MT_model, ~ condition|site, weights = "prop")%>%
  gather_emmeans_draws()%>%
  summarise(pd=round(mean(.value>TE_midthigh_comb,na.rm=TRUE)*100,0))

# Contrast for condition (>o)
pairs(emmeans(MT_model, ~condition, weights = "prop"))%>%
  gather_emmeans_draws()%>%
  group_by(contrast)%>%
  summarise(pd=round(mean(.value>0,na.rm=TRUE)*100,0))

# Contrast for condition (>TE)
pairs(emmeans(MT_model, ~condition, weights = "prop"))%>%
  gather_emmeans_draws()%>%
  group_by(contrast)%>%
  summarise(pd=round(mean(.value>TE_midthigh_comb,na.rm=TRUE)*100,0))

#SIDE-THIGH--------------------------------------------------------------------
# By condition
emmeans(ST_model, ~ condition, weights = "prop")%>%
  gather_emmeans_draws()%>%
  group_by(condition)%>%
  summarise(pd=round(mean(.value>0,na.rm=TRUE)*100,0))

# By condition over TE
emmeans(ST_model, ~ condition, weights = "prop")%>%
  gather_emmeans_draws()%>%
  group_by(condition)%>%
  summarise(pd=round(mean(.value>TE_sidethigh_comb,na.rm=TRUE)*100,0))

# By condition and site over TE
emmeans(ST_model, ~ condition|site, weights = "prop")%>%
  gather_emmeans_draws()%>%
  summarise(pd=round(mean(.value>TE_sidethigh_comb,na.rm=TRUE)*100,0))

# Contrast for condition (>o)
pairs(emmeans(ST_model, ~condition, weights = "prop"))%>%
  gather_emmeans_draws()%>%
  group_by(contrast)%>%
  summarise(pd=round(mean(.value>0,na.rm=TRUE)*100,0))

# Contrast for condition (>TE)
pairs(emmeans(ST_model, ~condition, weights = "prop"))%>%
  gather_emmeans_draws()%>%
  group_by(contrast)%>%
  summarise(pd=round(mean(.value>TE_sidethigh_comb,na.rm=TRUE)*100,0))

#---------------------------- FATIGUE DATA ANALYSIS------------------------------------------------------------
first_set_stats <- data_fatigue %>%
  filter(set_number == 1) %>%  # Filter for first sets
  group_by(condition, week) %>%  # Group by condition and week
  summarise(
    mean_first = mean(torque, na.rm = TRUE),  
    sd_first = sd(torque, na.rm = TRUE),  
    .groups = "drop"
  )

#Join mean and SD back to the original data
data_fatigue <- data_fatigue %>%
  left_join(first_set_stats, by = c("condition", "week")) %>% 
  mutate(
    z = (torque - mean_first) / sd_first  # Calculate z-score for all sets
  )

fatigue_model <- brm(
  z ~ set_number + set_number:condition + week + set_number:week + 
    (set_number + set_number:condition 
     + week + set_number:week | participant_id),
  data = data_fatigue,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list ((adapt_delta=0.99), (max_treedepth=15))
  
)

summary(fatigue_model)

# Generate new data for predictions
new_data <- expand.grid(
  set_number = 1:5,  # Maximum number of sets
  condition = unique(data_fatigue$condition),
  week = unique(data_fatigue$week)
)

# Define the number of sets per week
sets_per_week <- c(3, 3, 4, 4, 5, 5)
weeks <- unique(data_fatigue$week)

# Filter new_data to include only the valid set numbers per week
new_data <- new_data %>%
  mutate(max_sets = sets_per_week[match(week, weeks)]) %>%
  filter(set_number <= max_sets) %>%
  select(-max_sets)

# Predict posterior samples
predicted <- posterior_epred(fatigue_model, newdata = new_data, re_formula = NA)

# Create an HPD summary table
hpd_summary <- as.data.frame(predicted) %>%
  mutate(row = seq_len(nrow(predicted))) %>%  
  pivot_longer(
    cols = -row,  
    names_to = "sample",
    values_to = "value"
  ) %>%
  group_by(row) %>%
  summarise(
    mean_emmean = mean(value, na.rm = TRUE),
    lower_HPD = quantile(value, probs = 0.025, na.rm = TRUE),
    upper_HPD = quantile(value, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    set_number = new_data$set_number[row],
    condition = new_data$condition[row],
    week = new_data$week[row]
  ) %>%
  select(-row)


new_data <- new_data %>%
  left_join(hpd_summary, by = c("set_number", "condition", "week"))


FT1 <- ggplot(new_data, aes(x = set_number, y = mean_emmean, color = condition, group = condition)) +
  geom_line(size = 1.2) +  # Line for the mean
  geom_ribbon(aes(ymin = lower_HPD, ymax = upper_HPD, fill = condition), 
              alpha = 0.2, color = NA) +  # Ribbon for 95% HDI
  facet_wrap(~ week, ncol = 3, labeller = labeller(week = function(w) paste("Week", w)), scales = "free_x") +
  scale_color_manual(values = c(DYN_color, ISOM_color)) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  labs(
    title = "Regression Slopes for Torque Across Sets and Weeks",
    x = "Set Number",
    y = "Standardized Peak Torque Values (z)",
    color = "Condition",
    fill = "Condition"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(
      fill = 'white',
      color = 'black',
      linewidth = 1.6
    ),
    strip.background = element_rect(
      color = "black",
      fill = "black",
      linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.4, "cm"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

# Display plot
print(FT1)
