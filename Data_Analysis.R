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

#Slopes for sets
set_fatigue <- emmeans(
  fatigue_model, 
  specs = ~ set_number * condition * week,
  at = list(
    set_number = 1:5, week = 1:6), #This is wrong as we had a gradual increase in weeks, and can't seem to get it to work when I tried setting them as a factor
  type = "response"
)

hpd.summary(set_fatigue, point.est = mean)
fatigue_slopes_GG <- set_fatigue %>% gather_emmeans_draws()

set_fatigue_summary <- fatigue_slopes_GG %>%
  group_by(set_number, condition, week) %>%
  summarise(
    mean_emmean = mean(.value, na.rm = TRUE),
    lower_HPD = quantile(.value, probs = 0.025, na.rm = TRUE),
    upper_HPD = quantile(.value, probs = 0.975, na.rm = TRUE)
  )

#Slopes for set_number by condition and week
fatigue_condition_week <- emtrends(
  fatigue_model,
  ~ condition | week,
  var = "set_number",
  at = list(week = 1:6),  #Strictly define weeks
  weights = "prop"
)

hpd.summary(fatigue_condition_week, point.est = mean)
slopes_GG <- fatigue_condition_week %>% gather_emmeans_draws()

slopes_summary <- slopes_GG %>%
  group_by(condition, week) %>%
  summarise(
    mean_slope = mean(.value, na.rm = TRUE),
    lower_HPD = quantile(.value, probs = 0.025, na.rm = TRUE),
    upper_HPD = quantile(.value, probs = 0.975, na.rm = TRUE)
  )