model <- brm(muscle.thickness ~ 1 + Pre + condition + condition:site + muscle + condition:muscle + condition:muscle:site + (1 | id),
             data = mid_side_thigh_master,
             family = gaussian(),
             chains = 4,
             seed = 123,
             warmup = 2000,
             iter = 4000,
             control = list(adapt_delta=0.99)) 

# By protocol only
MT2_effects <- emmeans(model, ~ condition, weights = "prop")
hpd.summary(MT2_effects, point.est = mean) # Display mean

MT2_effects_contrast <- pairs(emmeans(model, ~ condition, weights = "prop"))
hpd.summary(MT2_effects_contrast, point.est = mean) # Display mean

MT2_effects_GG <- emmeans(model, spec = ~condition, weights = "prop") %>% gather_emmeans_draws() # For ggplot

MT2_effects_contrast_GG <- pairs(emmeans(model, ~ condition, weights = "prop")) %>% gather_emmeans_draws() # For ggplot

# By protocol and muscle and contrast for each muscle
MT2_effects_muscle <- emmeans(model, ~ condition|muscle, weights = "prop")
hpd.summary(MT2_effects_muscle, point.est = mean) # Display mean

MT2_effects_contrast_muscle <- pairs(emmeans(model, ~ condition|muscle, weights = "prop"))
hpd.summary(MT2_effects_contrast_muscle, point.est = mean) # Display mean

MT2_effects_GG_muscle <- emmeans(model, spec = ~condition|muscle, weights = "prop") %>% gather_emmeans_draws() # For ggplot

MT2_effects_GG_contrast_muscle <- pairs(emmeans(model, ~ condition|muscle, weights = "prop")) %>% gather_emmeans_draws() # For ggplot


# By condition, muscle nd site
MT2_effects_condition_site <- emmeans(model, ~ condition | muscle | site, weights = "prop")
hpd.summary(MT2_effects_condition_site, point.est = mean) 

MT2_effects_GG_condition_site <- MT2_effects_condition_site %>% gather_emmeans_draws()

#Contrast for each condition at specific muscle and site
MT2_effects_contrast_site <- pairs(MT2_effects_condition_site)
hpd.summary(MT2_effects_contrast_site, point.est = mean) 

MT2_effects_GG_contrast_site <- MT2_effects_contrast_site %>% gather_emmeans_draws()

ggplot(MT2_effects_GG, aes(x = .value, y = condition, fill = condition)) + 
  geom_rect(data=NULL,aes(xmin = -TE_midthigh_comb, xmax = TE_midthigh_comb,ymin=-Inf,ymax=Inf),
            fill=TE_color, alpha = 0.3) +
  geom_vline(
    xintercept = 0 + TE_midthigh_comb, 
    colour = 'grey',                  
    linetype = 'dashed',
    linewidth = .8) + 
  geom_vline(
    xintercept = 0,
    colour = 'black',
    linetype = 'dashed',
    linewidth = .8) +
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-2, -1, 0, 1, 2, 3, 4)) +
  labs(
    x = paste ("Within-Condition Change in Quadriceps Muscle Thickness (mm)")
  ) +
  coord_cartesian(xlim = c(-2, 3.5)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(size = .8, colour = "black"),
    axis.title.y = element_blank(),
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

ggplot(data = MT2_effects_contrast_GG,
       aes(x=.value,
           y=contrast, fill = after_stat(x < 0))) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  geom_rect(aes(xmin = -TE_midthigh_comb, xmax = TE_midthigh_comb,ymin=stage("DYN - ISOM", after_scale = ymin-Inf), ymax=stage("DYN - ISOM", after_scale = ymax+Inf)),
            fill=TE_color, alpha = 0.3) +
  geom_rect(aes(xmin = -TE_midthigh_comb, xmax = TE_midthigh_comb,ymin=stage("DYN - ISOM", after_scale = ymin-Inf), ymax=stage("DYN - ISOM", after_scale = ymax+Inf)),
            fill=TE_color, alpha = 0.3) +
  geom_vline(aes(xintercept = TE_midthigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(aes(xintercept = -TE_midthigh_comb),
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
  labs(
    x = "Between-Condition Change in Quadriceps Femoris Muscle Thickness (mm)"
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-2.5, 0, 2.5)) +
  coord_cartesian(xlim = c(-2.5, 2.5)) +
  theme(
    strip.background = element_rect(
      color = "white",
      fill = "white",
      linetype = "solid"
    ),
    strip.text.x = element_text(
      family = "Helvetica",
      size = 12,
      color = "white",
      face = "bold"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(size = .8, colour = "black"),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    panel.grid = element_blank(),panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

ggplot(MT2_effects_GG_muscle, aes(x = .value, y = condition, fill = condition)) + 
  facet_grid(~muscle,scales = "free")+
  geom_rect(data = MT2_effects_GG_muscle%>%
              filter(muscle=="Mid-thigh"),
            aes(xmin=TE_midthigh_comb,xmax=0.415,ymin=stage("DYN", after_scale = ymin-0.38),ymax=stage("DYN", after_scale = ymin+2.08)),
            fill="#F5F5F5") +
  geom_rect(data = MT2_effects_GG_muscle%>%
              filter(muscle=="Side-thigh"),
            aes(xmin=TE_sidethigh_comb,xmax=0.415,ymin=stage("DYN", after_scale = ymin-0.38),ymax=stage("DYN", after_scale = ymin+2.08)),
            fill="#F5F5F5") +
  geom_vline(data = MT2_effects_GG_muscle%>%
               filter(muscle=="Mid-thigh"),
             xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = MT2_effects_GG_muscle%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = TE_midthigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT2_effects_GG_muscle%>%
               filter(muscle=="Side-thigh"),
             xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = MT2_effects_GG_muscle%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = TE_sidethigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(xlim = c(-3, 3)) +
  theme(
    strip.background = element_rect(
      color = "black",
      fill = "black",
      linetype = "solid"
    ),
    strip.text.x = element_text(
      family = "Helvetica",
      size = 13,
      color = "white",
      face = "bold"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      color = 'black',
      linewidth = 1.6
    ), panel.spacing = unit(.8, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

ggplot(data = MT2_effects_GG_contrast_muscle,
       aes(x=.value,
           y=contrast, fill = after_stat(x < 0))) +
  facet_grid(~muscle,scales = "free")+
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  geom_rect(data = MT2_effects_GG_contrast_muscle%>%
              filter(muscle=="Mid-thigh"),
            aes(xmin=-TE_midthigh_comb,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.57), ymax=stage("DYN - ISOM", after_scale = ymax+1.37)),
            fill="#F5F5F5") +
  geom_rect(data = MT2_effects_GG_contrast_muscle%>%
              filter(muscle=="Mid-thigh"),
            aes(xmin=TE_midthigh_comb,xmax=0.226,ymin=stage("DYN - ISOM", after_scale = ymin-0.57),ymax=stage("DYN - ISOM", after_scale = ymax+1.37)),
            fill="#F5F5F5") +
  geom_rect(data = MT2_effects_GG_contrast_muscle%>%
              filter(muscle=="Side-thigh"),
            aes(xmin=-TE_sidethigh_comb,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.57),ymax=stage("DYN - ISOM", after_scale = ymax+1.37)),
            fill="#F5F5F5") +
  geom_rect(data = MT2_effects_GG_contrast_muscle%>%
              filter(muscle=="Side-thigh"),
            aes(xmin=TE_sidethigh_comb,xmax=0.226,ymin=stage("DYN - ISOM", after_scale = ymin-0.57),ymax=stage("DYN - ISOM", after_scale = ymax+1.37)),
            fill="#F5F5F5") +
  geom_vline(data = MT2_effects_GG_contrast_muscle%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = TE_midthigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT2_effects_GG_contrast_muscle%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = -TE_midthigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8)+
  geom_vline(data = MT2_effects_GG_contrast_muscle%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = TE_sidethigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = MT2_effects_GG_contrast_muscle%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = -TE_sidethigh_comb),
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
  geom_vline(data = MT2_effects_GG_contrast_muscle%>%
               filter(muscle=="Side-thigh"),
             xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8)+
  scale_x_continuous(expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(xlim = c(-3, 3)) +
  theme(
    strip.background = element_rect(
      color = "white",
      fill = "white",
      linetype = "solid"
    ),
    strip.text.x = element_text(
      family = "Helvetica",
      size = 12,
      color = "white",
      face = "bold"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = 'white',
      color = 'black',
      linewidth = 1.6
    ),panel.spacing = unit(.8, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

TE_values <- tibble::tibble(
  muscle = c("Mid-thigh", "Mid-thigh", "Mid-thigh", 
             "Side-thigh", "Side-thigh", "Side-thigh"),
  site = c("30", "50", "70", 
           "30", "50", "70"),
  ROPE_xmin = c(-TE_midthigh_30, -TE_midthigh_50, -TE_midthigh_70, 
                -TE_sidethigh_30, -TE_sidethigh_50, -TE_sidethigh_70),
  ROPE_xmax = c(TE_midthigh_30, TE_midthigh_50, TE_midthigh_70, 
                TE_sidethigh_30, TE_sidethigh_50, TE_sidethigh_70)
)

plot_data <- MT2_effects_GG_condition_site %>%
  left_join(TE_values, by = c("muscle", "site"))

ggplot(plot_data, aes(x = .value, fill = condition)) +
  geom_rect(aes(xmin = ROPE_xmin, xmax = ROPE_xmax, ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3, inherit.aes = FALSE) +
  geom_vline(aes(xintercept = ROPE_xmax), colour = 'grey', linetype = 'dashed', linewidth = .8) + 
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  facet_grid(muscle ~ site, scales = "free", labeller = labeller(
    muscle = c("Mid-Thigh" = "Mid-Thigh", "Side-Thigh" = "Side-Thigh"),
    site = function(x) paste0(x, "%")
  )) +
  stat_slabinterval(
    aes(fill = condition),
    .width = 0.95,
    alpha = 0.85,
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  labs(
    title = "Within-condition posterior distributions by Site and Muscle",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
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

plot_data2 <- MT2_effects_GG_contrast_site %>%
  left_join(TE_values, by = c("muscle", "site"))

ggplot(data = plot_data2,
       aes(x = .value, y = contrast, fill = after_stat(x < 0))) +
  
  geom_rect(aes(xmin = ROPE_xmin, xmax = ROPE_xmax, ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3, inherit.aes = FALSE) +
  geom_vline(aes(xintercept = ROPE_xmax), colour = 'grey', linetype = 'dashed', linewidth = .8) + 
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  facet_grid(muscle ~ site, scales = "free", labeller = labeller(
    muscle = c("Mid-Thigh" = "Mid-Thigh", "Side-Thigh" = "Side-Thigh"),
    site = function(x) paste0(x, "%")
  )) +
  
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) +
  stat_pointinterval(point_interval = "mean_hdci", .width = .95) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-4, -2, 0, 2, 4)) +
  coord_cartesian(xlim = c(-4.5, 4.5)) +
  labs(
    title = "Between-condition posterior distributions by Muscle and Site",
    x = element_blank(),
    y = element_blank()
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
    ), panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )
