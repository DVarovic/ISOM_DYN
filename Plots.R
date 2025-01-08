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
    labs(
      y = paste(muscle_group_name, "Muscle Thickness (mm)")
    ) +
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
MT1 <- plot_muscle_group(mt_master_long, "Mid-thigh")
ST1 <- plot_muscle_group(mt_master_long, "Side-thigh")

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
        strip.text.y = element_text(family = "Helvetica", size = 13, color = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(  
          family = "Helvetica",
          size = 12,
          colour = "black"
        ),
        axis.ticks = element_line(linewidth = .8, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(family = "Helvetica", size = 12, colour = "black"),
        strip.text.x = element_text(family = "Helvetica", size = 13, color = "white", face ="bold"), 
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

MT2 <- combined_plots[["Mid-thigh"]]
ST2 <- combined_plots[["Side-thigh"]]


#--------------------------------- BAYSEAN ANALYSIS WITHIN PROTOCOL CHANGES ---------------------------------
# Data frame for TE values for each site and muscle
TE_values <- data.frame(
  site = c("30", "50", "70"),
  MT_TE_low = c(-TE_midthigh_30, -TE_midthigh_50, -TE_midthigh_70),
  MT_TE_high = c(TE_midthigh_30, TE_midthigh_50, TE_midthigh_70),
  ST_TE_low = c(-TE_sidethigh_30, -TE_sidethigh_50, -TE_sidethigh_70),
  ST_TE_high = c(TE_sidethigh_30, TE_sidethigh_50, TE_sidethigh_70)
)


#MID-THIGH--------------------------------------------------------------------
MT3 <- ggplot(MT_effects_GG, aes(x = .value, y = condition, fill = condition)) + 
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
    x = paste ("Within-Condition Change in Mid-thigh Muscle Thickness (mm)")
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

MT4 <- ggplot(data = MT_effects_contrast_GG,
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
    x = "Between-Condition Change in Mid-thigh Muscle Thickness (mm)"
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


#Plot displaying posterior distributions of each condition by each site
MT5 <- ggplot(MT_effects_GG_condition_site, aes(x = .value, y = site, fill = condition)) +
  geom_rect(aes(xmin = -TE_midthigh_comb, xmax = TE_midthigh_comb, ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
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
  facet_wrap(~site, 
             scales = "free", 
             labeller = as_labeller(function(x) paste0(x, "%"))) +
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
    title = "Within-condition posterior distributions by Site for Mid-thigh",
    x = NULL,
    y = NULL
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
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
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




#Plot displaying condition contrasts at each site
MT6 <- ggplot(data = MT_effects_GG_contrast_site,
       aes(x = .value, y = contrast, fill = after_stat(x < 0))) +
  facet_grid(~site, scales = "free", labeller = as_labeller(function(x) paste0(x, "%"))) +
  
  geom_rect(data = TE_values,
            aes(xmin = MT_TE_low, xmax = MT_TE_high, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = TE_color, alpha = 0.3) +
  
  geom_vline(data = TE_values,
             aes(xintercept = MT_TE_low), colour = 'grey', linetype = 'dashed', linewidth = .8) +
  geom_vline(data = TE_values,
             aes(xintercept = MT_TE_high), colour = 'grey', linetype = 'dashed', linewidth = .8) +
  
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
    title = "Between-condition posterior distributions by Site for Mid-thigh",
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

MT.plots1 = (MT1 / MT2) +
           plot_annotation(tag_levels = c("A", "B"))

MT.plots2 = (MT3 + MT4) +
  plot_annotation(tag_levels = c("A", "B"))

MT.plots3 = (MT5 / MT6) +
  plot_annotation(tag_levels = c("A", "B"))


print(MT.plots1)
print(MT.plots2)
print(MT.plots3)
ggsave("plots/MT_plots1.pdf", plot = MT.plots1, width = 16, height = 10)
ggsave("plots/MT_plots2.pdf", plot = MT.plots2, width = 12, height = 5)
ggsave("plots/MT_plots3.pdf", plot = MT.plots3, width = 8, height = 7)

#SIDE-THIGH--------------------------------------------------------------------
ST3 <- ggplot(ST_effects_GG, aes(x = .value, y = condition, fill = condition)) + 
  geom_rect(data=NULL,aes(xmin = -TE_sidethigh_comb, xmax = TE_sidethigh_comb,ymin=-Inf,ymax=Inf),
            fill=TE_color, alpha = 0.3) +
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
  labs(
    x = paste ("Within-Condition Change in Side-thigh Muscle Thickness (mm)")
  ) +
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
ST4 <- ggplot(data = ST_effects_contrast_GG,
       aes(x=.value,
           y=contrast, fill = after_stat(x < 0))) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  geom_rect(aes(xmin = -TE_sidethigh_comb, xmax = TE_sidethigh_comb,ymin=stage("DYN - ISOM", after_scale = ymin-Inf), ymax=stage("DYN - ISOM", after_scale = ymax+Inf)),
            fill=TE_color, alpha = 0.3) +
  geom_rect(aes(xmin = -TE_sidethigh_comb, xmax = TE_sidethigh_comb,ymin=stage("DYN - ISOM", after_scale = ymin-Inf), ymax=stage("DYN - ISOM", after_scale = ymax+Inf)),
            fill=TE_color, alpha = 0.3) +
  geom_vline(aes(xintercept = TE_sidethigh_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(aes(xintercept = -TE_sidethigh_comb),
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
    x = "Between-Condition Change in Side-thigh Muscle Thickness (mm)"
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-1.5, 0, 1.5)) +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
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

#Plot displaying posterior distributions of each condition by each site
ST5 <- ggplot(ST_effects_GG_condition_site, aes(x = .value, y = site, fill = condition)) +
  geom_rect(aes(xmin = -TE_sidethigh_comb, xmax = TE_sidethigh_comb, ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
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
  facet_wrap(~site, 
             scales = "free", 
             labeller = as_labeller(function(x) paste0(x, "%"))) + 
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
    title = "Within-condition posterior distributions by Site for Side-thigh",
    x = NULL,
    y = NULL
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
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
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

#Plot displaying condition contrasts for each site
ST6 <- ggplot(data = ST_effects_GG_contrast_site,
              aes(x = .value, y = contrast, fill = after_stat(x < 0))) +
  facet_grid(~site, scales = "free", labeller = as_labeller(function(x) paste0(x, "%"))) +
  
  geom_rect(data = TE_values,
            aes(xmin = ST_TE_low, xmax = ST_TE_high, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = TE_color, alpha = 0.3) +
  
  geom_vline(data = TE_values,
             aes(xintercept = ST_TE_low), colour = 'grey', linetype = 'dashed', linewidth = .8) +
  geom_vline(data = TE_values,
             aes(xintercept = ST_TE_high), colour = 'grey', linetype = 'dashed', linewidth = .8) +
  
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
    title = "Between-condition posterior distributions by Site for Side-thigh",
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

ST.plots1 = (ST1 / ST2) +
  plot_annotation(tag_levels = c("A", "B"))

ST.plots2 = (ST3 + ST4) +
  plot_annotation(tag_levels = c("A", "B"))

ST.plots3 = (ST5 / ST6) +
  plot_annotation(tag_levels = c("A", "B"))

print(ST.plots1)
print(ST.plots2)
print(ST.plots3)

ggsave("plots/ST_plots1.pdf", plot = ST.plots1, width = 16, height = 10)
ggsave("plots/ST_plots2.pdf", plot = ST.plots2, width = 12, height = 5)
ggsave("plots/ST_plots3.pdf", plot = ST.plots3, width = 8, height = 7)

