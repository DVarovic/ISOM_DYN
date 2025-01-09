# ISOM_DYN
Repository for a RCT study exploring the effects of isometric training at LML versus full ROM on muscle hypertrophy


Here is explanation for all the sheets and what they contain and current issues I'm not sure how to handle

START HERE:
1) Packages
2) Data_Preparation
3) Data_Analysis
4) Data_Diagnostics
5) Others are self-explanatory (Plots,SMD,Reliability...)

DATA SHEETS:

1) muscle_thickness_master_avg - Contains raw data for each condition,muscle and sites
2) muscle_thickness_means_avg - Mean and SD values for outcome (muscle.thickness) of the above

3) mid_thigh_diff_mean_avg - Mean and SD of differences (Post-Pre) between the two conditions at 3each site
4) side_thigh_diff_mean_avg - Mean and SD of differences (Post-Pre) between the two conditions at each site

5) mid_side_thigh_master - Essentially the same as diff_avg but also has raw data and contains both muscles
6) mid_side_thigh_means - Mean and SD values of the above

7) data_reliability - Test-retest values and specified ROPE values
8) data_fatigue - Fatigue /Peak Torque data

ISSUES:

1) Posterior draws for 70% at mid-thigh is somewhat confusing me. 
   Is it possible for DYN and ISOM to be -0.714 and -0.100 considering there was a 1mm increase for ISOM Pre-Post?

   Also, 30% emmeans seem fairly high-ish for ISOM (3.132) and that was a ~1.5mm change Pre-Post?.
   
2) Not fully confident in fatigue plot (across sets) in each respective week, and can't seem to come up with a working solution to plot the trend of slopes for both conditions throughout the whole intervention

3) For muscle thickness I tried calculating outcome as SMD's but posterior draws via emmeans do not correspond to SMD values that I got - see SMD.xlsx and SMD.R
   
