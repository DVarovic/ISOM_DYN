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

Mid_thigh_raw_plot <- plot_muscle_group(mt_master_long, "Mid-thigh")
Side_thigh_raw_plot <- plot_muscle_group(mt_master_long, "Side-thigh")
print(Mid_thigh_raw_plot)
print(Side_thigh_raw_plot)
#-----------PRE-POST PERCENTAGE CHANGE PLOTS -----------
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
  
  mt_diff_avg <- mt_master_long %>%
    tidyr::spread(key = time, value = muscle.thickness) %>%
    mutate(
      absolute_change = Post - Pre,
      percent_change = (Post - Pre) / Pre * 100
    )
  
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
  master_file = "Data/MT_master_avg.xlsx",
  diff_files = c("Data/mid_thigh_diff_analysis_avg.xlsx", "Data/side_thigh_diff_analysis_avg.xlsx"),
  sheets = list(mid_thigh_sheets, side_thigh_sheets),
  muscle_groups = c("Mid-thigh", "Side-thigh"),
  muscle_sites = c("30%", "50%", "70%")
)

Mid_thigh_percentage_plot <- combined_plots[["Mid-thigh"]]
Side_thigh_percentage_plot <- combined_plots[["Side-thigh"]]


Quadriceps_plot <- ggplot(muscle_thickness_eff_GG, aes(x = .value, y = condition, fill = condition)) + 
  geom_rect(data=NULL,aes(xmin = -QF_comb, xmax = QF_comb,ymin=-Inf,ymax=Inf),
            fill=TE_color, alpha = 0.3) +
  geom_vline(
    xintercept = 0 + -QF_comb, 
    colour = 'grey',                  
    linetype = 'dashed',
    linewidth = .8) +
  geom_vline(
    xintercept = 0 + QF_comb, 
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

Quadriceps_contrast_plot <- ggplot(data = muscle_thickness_eff_contrasts_GG,
                                   aes(x=.value,
                                       y=contrast, fill = after_stat(x < 0))) +
  scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  geom_rect(aes(xmin = -QF_comb, xmax = QF_comb,ymin = -Inf, ymax = Inf),
            fill=TE_color, alpha = 0.3) +
  geom_rect(aes(xmin = -QF_comb, xmax = QF_comb,ymin = -Inf, ymax = Inf),
            fill=TE_color, alpha = 0.3) +
  geom_vline(aes(xintercept = -QF_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(aes(xintercept = QF_comb),
             colour = 'grey',
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(
    xintercept = 0,
    colour = 'black',
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

QF.plots = (Quadriceps_plot / Quadriceps_contrast_plot) + plot_annotation (tag_levels = c("a","b"))
print(QF.plots)

Muscles_plot <- ggplot(muscle_thickness_eff_muscle_GG, aes(x = .value, y = condition, fill = condition)) + 
  facet_grid(~muscle,scales = "free") +
  geom_rect(data = muscle_thickness_eff_muscle_GG%>%
              filter(muscle=="Mid-thigh"),
            aes(xmin=-TE_midthigh_comb,xmax=TE_midthigh_comb,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_GG%>%
              filter(muscle=="Side-thigh"),
            aes(xmin=-TE_sidethigh_comb,xmax=TE_sidethigh_comb,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Mid-thigh"),
             xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = -TE_midthigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = TE_midthigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Side-thigh"),
             xintercept = 0,
             colour = 'black',
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = -TE_sidethigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_GG%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = TE_sidethigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  labs(
    x = "Within-Condition Change in Individual Muscles"
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
    ), panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )


Muscles_contrast_plot <- ggplot(data = muscle_thickness_eff_contrasts_GG,
                                aes(x=.value,
                                    y=contrast, fill = after_stat(x < 0))) + 
  facet_grid(~muscle,scales = "free") +
  
  geom_rect(data = muscle_thickness_eff_muscle_contrasts_GG%>%
              filter(muscle=="Mid-thigh"),
            aes(xmin=-TE_midthigh_comb,xmax=TE_midthigh_comb,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_contrasts_GG%>%
              filter(muscle=="Side-thigh"),
            aes(xmin=-TE_sidethigh_comb,xmax=TE_sidethigh_comb,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_contrasts_GG%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = -TE_midthigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_contrasts_GG%>%
               filter(muscle=="Mid-thigh"),
             aes(xintercept = TE_midthigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_contrasts_GG%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = -TE_sidethigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_contrasts_GG%>%
               filter(muscle=="Side-thigh"),
             aes(xintercept = TE_sidethigh_comb),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(xlim = c(-3, 3)) +
  labs(
    x = "Between-Condition Change in Individual Muscles"
  ) +
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
    ), panel.spacing = unit(.4, "cm"),
    axis.text = element_text(
      family = "Helvetica",
      size = 12,
      colour = "black"
    )
  )

Muscle.plots = (Muscles_plot / Muscles_contrast_plot) + plot_annotation (tag_levels = c("a","b"))
print(Muscle.plots)

Regions_plot <- ggplot(muscle_thickness_eff_muscle_site_GG, aes(x = .value, fill = condition)) + 
  facet_grid(muscle ~ site, scales = "free", labeller = labeller(
    muscle = c("Mid-Thigh" = "Mid-Thigh", "Side-Thigh" = "Side-Thigh"),
    site = function(x) paste0(x, "%")
  )) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle =="Mid-thigh" & site == "30"),
            aes(xmin=-TE_midthigh_30,xmax=TE_midthigh_30,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle == "Mid-thigh" & site == "50"),
            aes(xmin=-TE_midthigh_50,xmax=TE_midthigh_50,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle == "Mid-thigh" & site == "70"),
            aes(xmin=-TE_midthigh_70,xmax=TE_midthigh_70,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle == "Side-thigh" & site == "30"),
            aes(xmin=-TE_sidethigh_30,xmax=TE_sidethigh_30,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle == "Side-thigh" & site == "50"),
            aes(xmin=-TE_sidethigh_50,xmax=TE_sidethigh_50,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_GG%>%
              filter(muscle == "Side-thigh" & site == "70"),
            aes(xmin=-TE_sidethigh_70,xmax=TE_sidethigh_70,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "30"),
             aes(xintercept = TE_midthigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "50"),
             aes(xintercept = TE_midthigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "70"),
             aes(xintercept = TE_midthigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "30"),
             aes(xintercept = -TE_midthigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "50"),
             aes(xintercept = -TE_midthigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Mid-thigh" & site == "70"),
             aes(xintercept = -TE_midthigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "30"),
             aes(xintercept = TE_sidethigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "50"),
             aes(xintercept = TE_sidethigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "70"),
             aes(xintercept = TE_sidethigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "30"),
             aes(xintercept = -TE_sidethigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) + 
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "50"),
             aes(xintercept = -TE_sidethigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_GG%>%
               filter(muscle =="Side-thigh" & site == "70"),
             aes(xintercept = -TE_sidethigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  labs(
    title = "Within-Condition Change in Individual Muscles Across 3 sites",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "black", fill = "black", linetype = "solid"),  
    strip.text.y = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"),  
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
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

Regions_contrast_plot <- ggplot(data = muscle_thickness_eff_muscle_site_contrasts_GG,
                                aes(x=.value,
                                    y=contrast, fill = after_stat(x < 0))) + 
  facet_grid(muscle ~ site, scales = "free", labeller = labeller(
    muscle = c("Mid-Thigh" = "Mid-Thigh", "Side-Thigh" = "Side-Thigh"),
    site = function(x) paste0(x, "%")
  )) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle =="Mid-thigh" & site == "30"),
            aes(xmin=-TE_midthigh_30,xmax=TE_midthigh_30,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle == "Mid-thigh" & site == "50"),
            aes(xmin=-TE_midthigh_50,xmax=TE_midthigh_50,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle == "Mid-thigh" & site == "70"),
            aes(xmin=-TE_midthigh_70,xmax=TE_midthigh_70,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle == "Side-thigh" & site == "30"),
            aes(xmin=-TE_sidethigh_30,xmax=TE_sidethigh_30,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle == "Side-thigh" & site == "50"),
            aes(xmin=-TE_sidethigh_50,xmax=TE_sidethigh_50,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_rect(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
              filter(muscle == "Side-thigh" & site == "70"),
            aes(xmin=-TE_sidethigh_70,xmax=TE_sidethigh_70,ymin = -Inf, ymax = Inf),
            fill = TE_color, alpha = 0.3) +
  geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed', linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "30"),
             aes(xintercept = TE_midthigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "50"),
             aes(xintercept = TE_midthigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "70"),
             aes(xintercept = TE_midthigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "30"),
             aes(xintercept = -TE_midthigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "50"),
             aes(xintercept = -TE_midthigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Mid-thigh" & site == "70"),
             aes(xintercept = -TE_midthigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "30"),
             aes(xintercept = TE_sidethigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "50"),
             aes(xintercept = TE_sidethigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "70"),
             aes(xintercept = TE_sidethigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "30"),
             aes(xintercept = -TE_sidethigh_30),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "50"),
             aes(xintercept = -TE_sidethigh_50),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  geom_vline(data = muscle_thickness_eff_muscle_site_contrasts_GG%>%
               filter(muscle =="Side-thigh" & site == "70"),
             aes(xintercept = -TE_sidethigh_70),
             colour = 'grey',                  
             linetype = 'dashed',
             linewidth = .8) +
  stat_halfeye(.width = 0.95, point_interval = mean_hdi) + scale_fill_manual(values = c(DYN_color, ISOM_color)) +
  stat_slabinterval(
    slab_color = "black",
    slab_linetype = 1,
    slab_size = 1,
    point_size = 4.5
  ) +
  labs(
    title = "Between-Condition Change in Individual Muscles Across 3 sites",
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(xlim = c(-3, 3)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "black", fill = "black", linetype = "solid"),  
    strip.text.y = element_text(family = "Helvetica", size = 13, color = "white", face = "bold"),  
    axis.ticks = element_line(linewidth = .8, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
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
print(Regions_plot)
