library(pacman)

pacman::p_load(tidyverse,brms,tidybayes,bayesplot,ggridges,readxl,rstan,ggplot2,posterior,
               glue,ggpubr,bayestestR,emmeans,rstatix,gt,dplyr,ggh4x,datawizard,patchwork,
               lme4,performance,ggdist,marginaleffects,collapse,tidymodels,easystats,extrafont,SimplyAgree,metafor)

loadfonts(device = "win")

TE_color = "#E9F4F5"
ISOM_color ="#329BE7"
DYN_color ="#C7413F"