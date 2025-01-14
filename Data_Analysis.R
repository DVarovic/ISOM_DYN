#muscle.thickness ~ 1 + Pre + condition + condition:site + (1 | id) #This model was specified in pre-reg 
                                                                    #But, I also mentioned that both participant and site 
                                                                    #will be added as random intercepts, so these did not align from the very start

#muscle.thickness ~ 1 + Pre + muscle:condition:site + (1 | id) #Three-way interaction, chains did not mix well, 
                                                               #and model did not converge well

# ----------------------------- MODEL FITTING -----------------------------
#To make it easier, two models were generated, one for each muscle, and I went with a model parameters as James suggested
data_mid_thigh <- mid_side_thigh_master %>% filter(muscle == "Mid-thigh")
data_side_thigh <- mid_side_thigh_master %>% filter(muscle == "Side-thigh")

MT_model <- brm(muscle.thickness ~ 1 + Pre + condition + condition:site + (1 | id),
                data = data_mid_thigh,
                family = gaussian(),
                chains = 4,
                seed = 123,
                warmup = 2000,
                iter = 4000,
                control = list(adapt_delta=0.99))

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
  group_by(condition, week) %>% 
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

## Week + Condition + Set Number
fatigue_effects <- emmeans(
  fatigue_model,
  ~ week + condition + set_number,
  at = list(week = 1:6, set_number = seq(1, 5, by = 1)),
  weights = "prop"
)
hpd.summary(fatigue_effects, point.est = mean)

fatigue_effects_GG <- fatigue_effects %>%
  gather_emmeans_draws()  


fatigue_effects_GG <- fatigue_effects_GG %>%
  mutate(max_sets = case_when(
    week == 1 ~ 3, week == 2 ~ 3, week == 3 ~ 4,
    week == 4 ~ 4,week == 5 ~ 5, week == 6 ~ 5)) %>%
  filter(set_number <= max_sets)  # Keep only rows where set_number is within the allowed range


ggplot(fatigue_effects_GG, aes(x = set_number, y = .value, color = condition, fill = condition)) +
  facet_wrap(~ week, scales = "free", ncol = 3, labeller = labeller(week = function(w) paste("Week", w))) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dashed', linewidth = 0.8) +
  stat_lineribbon(point_interval = "mean_hdci", .width = .95) +
  scale_color_manual(values = c(DYN_color, ISOM_color)) +
  scale_fill_manual(values = alpha(c(DYN_color, ISOM_color), 0.6)) +
  labs(
    x = "Set Number",
    y = "Standardized Peak Torque (z-score)"
  ) +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "black", fill = "black", linetype = "solid"),  
    strip.text.y = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"),  
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    strip.text.x = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"), 
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = 'white',
      color = 'black',
      linewidth = 1.6
    ), panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

#I can't seem to get trend slopes from 1st to last week to work, literally stuck without options
#The above plot is using emmeans to plot changes in each week from first to last set, not sure if trend would be needed there as well

weekly_trends <- emtrends(
  fatigue_model,
  ~ condition,
  var = "week", 
  at = list(week = 1:6)
)