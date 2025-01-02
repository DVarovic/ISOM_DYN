#--------- READ DATA ------------
muscle_thickness_master_avg <- read_excel ("MT_master_avg.xlsx")
muscle_thickness_means_avg <- read_excel("MT_master_avg.xlsx", "MT Means") 

mid_thigh_diff_mean_avg <- read_excel("mid_thigh_diff_analysis_avg.xlsx", "Diff_Means") 
side_thigh_diff_mean_avg <- read_excel("side_thigh_diff_analysis_avg.xlsx", "Diff_Means") 

data_fatigue <- read_excel("Torque_Raw.xlsx","Fatigue_clean")

mid_side_thigh_master <- read_xlsx("mid_side_master.xlsx")%>%
  mutate_at(c("id","condition","muscle","site","sex"),factor)
mid_side_thigh_master=mid_side_thigh_master%>%
  group_by(muscle)%>%
  center(Pre)

mid_side_thigh_means <- read_xlsx("mid_side_master.xlsx", "Mid and Side Means")

mid_thigh_sheets <- c("MT30", "MT50", "MT70")
side_thigh_sheets <- c("ST30", "ST50", "ST70")

muscle_sites <- c("30%","50%","70%") 

process_muscle_group <- function(file_path, sheets, muscle_group, muscle_sites) {
  bind_rows(
    lapply(seq_along(sheets), function(i) {
      read_excel(file_path, sheet = sheets[i]) %>%
        mutate(
          site = muscle_sites[i],
          muscle_group = muscle_group  # Add a column to identify the muscle group
        )
    })
  )
}

mt_master_long <- bind_rows(
  process_muscle_group("MT_master_avg.xlsx", mid_thigh_sheets, "Mid-thigh", muscle_sites),
  process_muscle_group("MT_master_avg.xlsx", side_thigh_sheets, "Side-thigh", muscle_sites)
) %>%
  pivot_longer(
    cols = 6:7,
    names_to = "time",
    values_to = "muscle.thickness"
  )