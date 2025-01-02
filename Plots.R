#----------- WITHIN PROTOCOL PRE-POST CHANGE AND INDIVIDUAL DATA POINTS -----------
plot_muscle_group <- function(data, muscle_group_name) {
  data %>%
    filter(muscle_group == muscle_group_name) %>% 
    ggplot(aes(x = factor(time, levels = c("Pre", "Post")), y = muscle.thickness)) + 
    facet_wrap(~ site + condition, scales = "free_y", ncol= 6) +
    #facet_grid(site ~ condition, scales = "free_y") +
    stat_summary(fun = 'mean', geom = 'crossbar', width = 0.6, linewidth = 1.5) +
    geom_line(aes(group = id), size = 0.7, colour = 'grey') +
    geom_point(
      aes(group = id, fill = condition),
      shape = 21,
      size = 4,
      stroke = 1,
      position = position_dodge(width = 0)
    ) +
    scale_fill_manual(values = c(DYN_color, ISOM_color)) +
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
      panel.spacing = unit(.4, "cm"),
      axis.title.x = element_blank(),
      axis.ticks = element_line(linewidth = .5, colour = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text = element_text(
        family = "Helvetica",
        size = 12,
        colour = "black"
      ),
      legend.position = "none",
      strip.text.x = element_text(
        family = "Helvetica",
        size = 13,
        color = "white",
        face = "bold"
      )
    )
}

# Generate plots for each muscle group
plot_muscle_group(mt_master_long, "Mid-thigh")
plot_muscle_group(mt_master_long, "Side-thigh")

#----------- WITHIN PROTOCOL PRE-POST PERCENTAGE CHANGE PLOTS -----------
precentage_diff_plots <- function(master_file, diff_files, sheets, muscle_groups, muscle_sites) {
  mt_master_long <- bind_rows(
    lapply(seq_along(muscle_groups), function(i) {
      process_muscle_group(master_file, sheets[[i]], muscle_groups[i], muscle_sites)
    })
  ) %>%
    pivot_longer(
      cols = 6:7,
      names_to = "time",
      values_to = "muscle.thickness"
    )
  
  #Compute percentage changes
  mt_diff_avg <- mt_master_long %>%
    tidyr::spread(key = time, value = muscle.thickness) %>%
    mutate(
      absolute_change = Post - Pre,
      percent_change = (Post - Pre) / Pre * 100
    )
  
  # Step 3: Generate combined plots for each muscle group
  plot_list <- list()
  
  for (group in unique(mt_diff_avg$muscle_group)) {
    data_filtered <- mt_diff_avg %>%
      filter(muscle_group == group)
    
    plot <- ggplot(data_filtered, aes(x = factor(condition, levels = c("DYN", "ISOM")), y = percent_change)) +
      facet_wrap(~ site, scales = "free_y", ncol = 3) +  # Facet by site
      stat_summary(fun = 'mean', geom = 'crossbar', width = 0.6, linewidth = 1.5) +
      geom_point(
        aes(group = id, fill = condition),
        shape = 21,
        size = 4,
        stroke = 1,
        position = position_dodge(width = 0.3)
      ) +
      scale_fill_manual(values = c(DYN_color, ISOM_color)) +
      scale_y_continuous(expand = c(0.05, 0.05)) +
      labs(
        y = paste(group, "Muscle Thickness (%)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', color = 'black', linewidth = 1.6),
        strip.background = element_rect(color = "black", fill = "black", linetype = "solid"),  
        strip.text.y = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(  
          family = "Helvetica",
          size = 12,
          colour = "black",
          face = "bold"
        ),
        axis.ticks = element_line(linewidth = .8, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(family = "Helvetica", size = 12, colour = "black"),
        strip.text.x = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"), 
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm")
      )
    
    plot_list[[group]] <- plot
  }
  
  return(plot_list)
}

combined_plots <- precentage_diff_plots(
  master_file = "MT_master_avg.xlsx",
  diff_files = c("mid_thigh_diff_analysis_avg.xlsx", "side_thigh_diff_analysis_avg.xlsx"),
  sheets = list(mid_thigh_sheets, side_thigh_sheets),
  muscle_groups = c("Mid-thigh", "Side-thigh"),
  muscle_sites = c("30%", "50%", "70%")
)

combined_plots[["Mid-thigh"]]
combined_plots[["Side-thigh"]]


#--------------------------------- BAYSEAN ANALYSIS WITHIN PROTOCOL CHANGES ---------------------------------
#MID-THIGH--------------------------------------------------------------------
ggplot(MT_effects_GG, aes(x = .value, y = condition, fill = condition)) + 
  geom_rect(data=NULL,aes(xmin=TE_midthigh_comb,xmax=1,ymin=-Inf,ymax=Inf),
            fill=TE_color) +
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
  coord_cartesian(xlim = c(-3, 4.5)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(size = .8, colour = "black"),
    axis.title.x = element_blank(),
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

#Plot displaying condition contrasts for each site
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

#SIDE-THIGH--------------------------------------------------------------------
ggplot(ST_effects_GG, aes(x = .value, y = condition, fill = condition)) + 
  geom_rect(data=NULL,aes(xmin=TE_sidethigh_comb,xmax=1,ymin=-Inf,ymax=Inf),
            fill=TE_color) +
  geom_vline(
    xintercept = 0 + TE_sidethigh_comb,
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
  coord_cartesian(xlim = c(-3, 4.5)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(size = .8, colour = "black"),
    axis.title.x = element_blank(),
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

#Plot displaying posterior distributions of each condition by each site
ggplot(ST_effects_GG_condition_site, aes(x = .value, y = site, fill = condition)) + 
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
    title = "Posterior Distributions of Conditions by Site for Side-thigh",
    x = "Effect Size",
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

#Plot displaying condition contrasts for each site
ggplot(data = ST_effects_GG_contrast_site,
       aes(x=.value,
           y=contrast, fill = after_stat(x < 0))) +
  facet_grid(~site,scales = "free")+
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="30"),
            aes(xmin=-TE_sidethigh_30,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="30"),
            aes(xmin=TE_sidethigh_30,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="50"),
            aes(xmin=-TE_sidethigh_50,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="50"),
            aes(xmin=TE_sidethigh_50,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="70"),
            aes(xmin=-TE_sidethigh_70,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_rect(data = ST_effects_GG_contrast_site%>%
              filter(site=="70"),
            aes(xmin=TE_sidethigh_70,xmax=-0.216,ymin=stage("DYN - ISOM", after_scale = ymin-0.591), ymax=stage("DYN - ISOM", after_scale = ymax+0.99)),
            fill=TE_color) +
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="30"),
             aes(xintercept = TE_sidethigh_30),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="30"),
             aes(xintercept = -TE_sidethigh_30),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8)+
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="50"),
             aes(xintercept = TE_sidethigh_50),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="50"),
             aes(xintercept = -TE_sidethigh_50),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="70"),
             aes(xintercept = TE_sidethigh_70),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = ST_effects_GG_contrast_site%>%
               filter(site=="70"),
             aes(xintercept = -TE_sidethigh_70),
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
    ),panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )


#Plot for fatigue - across sets
ggplot(set_fatigue_summary, aes(x = set_number, y = mean_emmean, color = condition, group = condition)) +
  geom_line(size = 1.2) +  # Line for the mean
  geom_ribbon(aes(ymin = lower_HPD, ymax = upper_HPD, fill = condition), 
              alpha = 0.2, color = NA) +  # Ribbon for 95% HDI
  facet_wrap(~ week, ncol = 3, labeller = labeller(week = function(w) paste("Week", w))) +
  scale_color_manual(values = c(DYN_color, ISOM_color)) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  labs(
    title = "Regression Slopes with 95% HDIs for Torque Across Sets and Weeks",
    x = "Set Number",
    y = "Standardized Peak Torque values (z)",
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
    panel.spacing = unit(.4, "cm"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(linewidth = .5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

#Plot for fatigue - set number slopes across weeks
ggplot(slopes_summary, aes(x = week, y = mean_slope, color = condition, group = condition)) +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin = lower_HPD, ymax = upper_HPD, fill = condition), alpha = 0.2) + 
  scale_x_continuous(breaks = 1:6, labels = paste("Week", 1:6)) +
  scale_color_manual(values = c(DYN_color, ISOM_color)) +
  scale_fill_manual(values = alpha(c(DYN_color, ISOM_color), 0.6)) +
  labs(
    title = "Slopes for Sets by Condition Across Weeks",
    x = "Week",
    y = "Slope of Fatigue (z-Scores)",
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
    panel.spacing = unit(.4, "cm"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(linewidth = .5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    ))
