#This is mainly for backup and reference if anything breaks - to see the original code that worked

#Condition contrasts plot
ggplot(data = MT_effects_GG_contrast_site,
       aes(x=.value,
           y=contrast, fill = after_stat(x < 0))) +
  facet_grid(~site,scales = "free")+
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="30"),
            aes(xmin=-TE_midthigh_30,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="30"),
            aes(xmin=TE_midthigh_30,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="50"),
            aes(xmin=-TE_midthigh_50,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="50"),
            aes(xmin=TE_midthigh_50,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="70"),
            aes(xmin=-TE_midthigh_70,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = MT_effects_GG_contrast_site%>%
              filter(site=="70"),
            aes(xmin=TE_midthigh_70,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="30"),
             aes(xintercept = TE_midthigh_30),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="30"),
             aes(xintercept = -TE_midthigh_30),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8)+
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="50"),
             aes(xintercept = TE_midthigh_50),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="50"),
             aes(xintercept = -TE_midthigh_50),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="70"),
             aes(xintercept = TE_midthigh_70),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT_effects_GG_contrast_site%>%
               filter(site=="70"),
             aes(xintercept = -TE_midthigh_70),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) +
  stat_pointinterval(point_interval = "mean_hdci",.width = .95) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  geom_vline(xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-4, -2, 0, 2, 4)) +
  coord_cartesian(xlim = c(-4.5, 4.5)) +
  labs (
    y = element_blank(),
    x = element_blank()
  ) +
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
    ),panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )


#Plot displaying posterior distributions of each condition by each site
ggplot(MT_effects_GG_condition_site, aes(x = .value, y = site, fill = condition)) +
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
    x = "Muscle thickness",
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
