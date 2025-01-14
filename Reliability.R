# ----------------------------- Reliability -----------------------------
muscles <- list("Mid-thigh" = 0, "Side-thigh" = 1)
sites <- list("30%" = 0, "50%" = 1, "70%" = 2)

# Initialize lists to store results
icc_results <- list()
sem_results <- list()

# Loop over muscles and sites
for (muscle_label in names(muscles)) {
  for (site_label in names(sites)) {
    # Read data
    data_reliability <- read_excel("Data/TRT.xlsx", sheet = "Mid and side comb")
    
    # Filter data
    filtered_data <- data_reliability %>%
      filter(muscle == muscles[[muscle_label]], site == sites[[site_label]])
    
    # Pivot data for reliability
    data_TRT <- filtered_data %>%
      select(id, scan, site, mt) %>%
      pivot_wider(names_from = "scan",
                  names_prefix = "scan.",
                  values_from = "mt")
    
    # Ensure required columns exist
    if (!all(c("scan.1", "scan.2") %in% colnames(data_TRT))) {
      message("Missing scan columns for ", muscle_label, " at site ", site_label)
      next
    }
    
    # Calculate reliability
    reliability <- reli_aov(data = data_TRT,
                            wide = TRUE,
                            col.names = c("scan.1", "scan.2"),
                            se_type = "ICC2")
    ICC <- reliability$icc %>% filter(type == "ICC2")
    SEM <- reliability$SEM %>% .$estimate
    
    # Store results
    key <- paste(muscle_label, site_label, sep = "_")
    icc_results[[key]] <- ICC
    sem_results[[key]] <- SEM
  }
}


# Print results for all muscles and sites
for (key in names(icc_results)) {
  cat("\nResults for:", key, "\n")
  cat("ICC:\n")
  print(icc_results[[key]])
  cat("SEM:\n")
  print(sem_results[[key]])
}

#SEM values used to define ROPE
TE_midthigh_30 <- 0.5112384
TE_midthigh_50 <- 0.4105061
TE_midthigh_70 <- 0.4865253
TE_midthigh_comb <- 0.4694233

#SEM values used to define ROPE
TE_sidethigh_30 <- 0.3672326
TE_sidethigh_50 <- 0.6038384
TE_sidethigh_70 <- 0.3978615
TE_sidethigh_comb <- 0.4563108


QF_comb <- 0.46286705 #average of midthigh and sidethigh comb
