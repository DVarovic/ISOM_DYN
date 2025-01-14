Main_model <- brm(muscle.thickness ~ 1 + Pre + condition * muscle * site + (1 | id),
                  data = mid_side_thigh_master,
                  family = gaussian(),
                  chains = 4,
                  seed = 123,
                  warmup = 2000,
                  iter = 4000,
                  control = list(adapt_delta=0.99)) 

Main_model_two_way <- brm(muscle.thickness ~ 1 + Pre + 
                            condition * muscle + 
                            condition * site + 
                            muscle * site + (1 | id),
                          data = mid_side_thigh_master,
                          family = gaussian(),
                          chains = 4,
                          seed = 123,
                          warmup = 2000,
                          iter = 4000,
                          control = list(adapt_delta=0.99))
pp_check(Main_model)

save(Main_model,file = "Main_model.RData")

#Within and between changes by condition (average of 2 muscles and 3 sites)
muscle_thickness_eff <- emmeans(Main_model, ~ condition, weights="prop")
hpd.summary(muscle_thickness_eff, point.est = mean)

muscle_thickness_eff_contrasts <- pairs(emmeans(Main_model, ~ condition, weights ="prop"))
hpd.summary(muscle_thickness_eff_contrasts, point.est = mean)

#Within and between-condition changes for each muscle across 3 sites
muscle_thickness_eff_muscle <- emmeans(Main_model, ~ condition | muscle, weights = "prop")
hpd.summary(muscle_thickness_eff_muscle, point.est = mean)

muscle_thickness_eff_muscle_contrasts <- pairs(emmeans(Main_model, ~ condition | muscle, weights = "prop"))
hpd.summary(muscle_thickness_eff_muscle_contrasts, point.est = mean)

#Within and between-condition changes in each muscle across three sites
muscle_thickness_eff_muscle_site <- emmeans(Main_model, ~ condition | muscle * site, weights ="prop")
hpd.summary(muscle_thickness_eff_muscle_site, point.est = mean)

muscle_thickness_eff_muscle_site_contrasts <- pairs(emmeans(Main_model, ~ muscle | condition * site, weights = "prop"))
hpd.summary(muscle_thickness_eff_muscle_site_contrasts, point.est = mean)
summary(muscle_thickness_eff_muscle_site)


#GG Plot Extraction------------------------------------------------------------------------------------------------

#Within and between changes by condition (average of 2 muscles and 3 sites)
muscle_thickness_eff_GG <- emmeans(Main_model, spec = ~condition, weights = "prop") %>% 
  gather_emmeans_draws() 
muscle_thickness_eff_contrasts_GG <- pairs(emmeans(Main_model, ~ condition, weights = "prop")) %>% 
  gather_emmeans_draws()

#Within and between-condition changes for each muscle across 3 sites
muscle_thickness_eff_muscle_GG <- emmeans(Main_model, spec = ~ condition | muscle, weights = "prop") %>%
  gather_emmeans_draws()
muscle_thickness_eff_muscle_contrasts_GG <- pairs(emmeans(Main_model, ~ condition | muscle, weights ="prop")) %>%
  gather_emmeans_draws()

#Within and between-condition changes in each muscle across three sites
muscle_thickness_eff_muscle_site_GG <- emmeans(Main_model, spec = ~ condition | muscle | site, weights = "prop") %>%
  gather_emmeans_draws()
muscle_thickness_eff_muscle_site_contrasts_GG <- pairs(emmeans(Main_model, ~ condition | muscle | site, weights ="prop")) %>%
  gather_emmeans_draws()

#Calculations of Probabilities being greater than pd (null effect) or ROPE (TE values)-----------------

#Within changes by condition (average of 2 muscles and 3 sites)
emmeans(Main_model, ~ condition, weights = "prop") %>%
  gather_emmeans_draws() %>%
  group_by(condition) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = ifelse(mean(.value) > 0,
                  round(mean(.value > TE_midthigh_comb, na.rm = TRUE) * 100, 0),
                  round(mean(.value < -TE_midthigh_comb, na.rm = TRUE) * 100, 0))
  )

#Within condition changes for each muscle across 3 sites
emmeans(Main_model, ~ condition | muscle, weights = "prop") %>%
  gather_emmeans_draws() %>%
  group_by(condition, muscle) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = ifelse(
      muscle == "Mid-thigh",
      ifelse(mean(.value) > 0,
             round(mean(.value > TE_midthigh_comb, na.rm = TRUE) * 100, 0),
             round(mean(.value < -TE_midthigh_comb, na.rm = TRUE) * 100, 0)),
      ifelse(mean(.value) > 0,
             round(mean(.value > TE_sidethigh_comb, na.rm = TRUE) * 100, 0),
             round(mean(.value < -TE_sidethigh_comb, na.rm = TRUE) * 100, 0))
    )
  ) %>%
  ungroup() %>%
  filter(muscle %in% c("Mid-thigh", "Side-thigh")) %>%
  distinct()

#Within condition changes in each muscle across three sites
emmeans(Main_model, ~ condition | muscle | site, weights = "prop") %>%
  gather_emmeans_draws() %>%
  group_by(condition, muscle, site) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = case_when(
      muscle == "Mid-thigh" & site == "30" ~ ifelse(mean(.value) > 0,
                                                       round(mean(.value > TE_midthigh_30, na.rm = TRUE) * 100, 0),
                                                       round(mean(.value < -TE_midthigh_30, na.rm = TRUE) * 100, 0)),
      muscle == "Mid-thigh" & site == "50" ~ ifelse(mean(.value) > 0,
                                                       round(mean(.value > TE_midthigh_50, na.rm = TRUE) * 100, 0),
                                                       round(mean(.value < -TE_midthigh_50, na.rm = TRUE) * 100, 0)),
      muscle == "Mid-thigh" & site == "70" ~ ifelse(mean(.value) > 0,
                                                       round(mean(.value > TE_midthigh_70, na.rm = TRUE) * 100, 0),
                                                       round(mean(.value < -TE_midthigh_70, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "30" ~ ifelse(mean(.value) > 0,
                                                        round(mean(.value > TE_sidethigh_30, na.rm = TRUE) * 100, 0),
                                                        round(mean(.value < -TE_sidethigh_30, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "50" ~ ifelse(mean(.value) > 0,
                                                        round(mean(.value > TE_sidethigh_50, na.rm = TRUE) * 100, 0),
                                                        round(mean(.value < -TE_sidethigh_50, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "70" ~ ifelse(mean(.value) > 0,
                                                        round(mean(.value > TE_sidethigh_70, na.rm = TRUE) * 100, 0),
                                                        round(mean(.value < -TE_sidethigh_70, na.rm = TRUE) * 100, 0)),
      TRUE ~ NA_real_ 
    )
  ) %>%
  ungroup() %>%
  filter(muscle %in% c("Mid-thigh", "Side-thigh")) %>%
  distinct()


#Between (Contrast) changes by condition (average of 2 muscles and 3 sites)
pairs(emmeans(Main_model, ~ condition, weights = "prop")) %>%
  gather_emmeans_draws() %>%
  group_by(contrast) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = ifelse(mean(.value) > 0,
                  round(mean(.value > TE_midthigh_comb, na.rm = TRUE) * 100, 0),
                  round(mean(.value < -TE_midthigh_comb, na.rm = TRUE) * 100, 0))
  )

#Between (Contrast) condition changes for each muscle across 3 sites
pairs(emmeans(Main_model, ~ condition | muscle, weights = "prop")) %>%
  gather_emmeans_draws() %>%
  group_by(contrast, muscle) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = ifelse(
      muscle == "Mid-thigh",
      ifelse(mean(.value) > 0,
             round(mean(.value > TE_midthigh_comb, na.rm = TRUE) * 100, 0),
             round(mean(.value < -TE_midthigh_comb, na.rm = TRUE) * 100, 0)),
      ifelse(mean(.value) > 0,
             round(mean(.value > TE_sidethigh_comb, na.rm = TRUE) * 100, 0),
             round(mean(.value < -TE_sidethigh_comb, na.rm = TRUE) * 100, 0))
    )
  ) %>%
  ungroup() %>%
  filter(muscle %in% c("Mid-thigh", "Side-thigh")) %>%
  distinct()

#Between (Contrast) condition changes in each muscle across three sites
pairs(emmeans(Main_model, ~ condition | muscle | site, weights = "prop")) %>%
  gather_emmeans_draws() %>%
  group_by(contrast, muscle, site) %>%
  summarise(
    pd = ifelse(mean(.value) > 0,
                round(mean(.value > 0, na.rm = TRUE) * 100, 0),
                round(mean(.value < 0, na.rm = TRUE) * 100, 0)),
    rope = case_when(
      muscle == "Mid-thigh" & site == "30" ~ ifelse(mean(.value) > 0,
                                                    round(mean(.value > TE_midthigh_30, na.rm = TRUE) * 100, 0),
                                                    round(mean(.value < -TE_midthigh_30, na.rm = TRUE) * 100, 0)),
      muscle == "Mid-thigh" & site == "50" ~ ifelse(mean(.value) > 0,
                                                    round(mean(.value > TE_midthigh_50, na.rm = TRUE) * 100, 0),
                                                    round(mean(.value < -TE_midthigh_50, na.rm = TRUE) * 100, 0)),
      muscle == "Mid-thigh" & site == "70" ~ ifelse(mean(.value) > 0,
                                                    round(mean(.value > TE_midthigh_70, na.rm = TRUE) * 100, 0),
                                                    round(mean(.value < -TE_midthigh_70, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "30" ~ ifelse(mean(.value) > 0,
                                                     round(mean(.value > TE_sidethigh_30, na.rm = TRUE) * 100, 0),
                                                     round(mean(.value < -TE_sidethigh_30, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "50" ~ ifelse(mean(.value) > 0,
                                                     round(mean(.value > TE_sidethigh_50, na.rm = TRUE) * 100, 0),
                                                     round(mean(.value < -TE_sidethigh_50, na.rm = TRUE) * 100, 0)),
      muscle == "Side-thigh" & site == "70" ~ ifelse(mean(.value) > 0,
                                                     round(mean(.value > TE_sidethigh_70, na.rm = TRUE) * 100, 0),
                                                     round(mean(.value < -TE_sidethigh_70, na.rm = TRUE) * 100, 0)),
      TRUE ~ NA_real_ 
    )
  ) %>%
  ungroup() %>%
  filter(muscle %in% c("Mid-thigh", "Side-thigh")) %>%
  distinct()

# Extract posterior samples for individuals (random effects)
posterior_samples <- posterior_samples(Main_model, pars = "^r_id\\[") %>%
  pivot_longer(everything(), names_to = "id_condition", values_to = "posterior_sample") %>%
  separate(id_condition, into = c("id", "condition"), sep = "\\[|,|\\]", extra = "drop") %>%
  mutate(id = as.factor(id), condition = gsub("_.*", "", condition))

# Combine with emmeans
emmeans_data <- as.data.frame(muscle_thickness_eff_muscle_site) %>%
  rename(muscle_group = muscle, site = site, condition = condition) %>%
  mutate(site = factor(site, levels = c("30%", "50%", "70%")))

# Merge with posterior samples
plot_data <- left_join(posterior_samples, emmeans_data, by = "condition")

# Generate the plot
ggplot(plot_data, aes(x = site, y = posterior_sample, group = id)) +
  geom_line(color = "grey", alpha = 0.6) + # Individual lines
  geom_point(aes(y = emmean), shape = 21, size = 4, fill = "black") + # Emmeans points
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.2, size = 0.7) + # HDI intervals
  facet_wrap(~ muscle_group + condition, scales = "free_y") +
  labs(
    y = "Muscle Thickness (Estimated from Model)",
    x = "Site"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

