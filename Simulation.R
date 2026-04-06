

library(lavaan)



###

############################################################
## 0. Setup
############################################################
library(lavaan)

set.seed(2025)

############################################################
## 1. Simulation parameters
############################################################
N <- 1000
group <- rep(0:1, each = N/2)   # 0 = White, 1 = Black

true_delta <- 0.5               # TRUE latent mean difference

############################################################
## 2. Generate latent variable
############################################################
Author <- rnorm(
  N,
  mean = true_delta * group,
  sd = 1
)

############################################################
## 3. Generate items with DIF
############################################################
lambda <- c(1, 1, 1, 1)

# Threshold DIF: auth_1 invariant, others biased
threshold_shift <- c(0, 0.7, 0.7, 0.7)

y1 <- lambda[1] * Author + rnorm(N)
y2 <- lambda[2] * Author + rnorm(N) + threshold_shift[2] * group
y3 <- lambda[3] * Author + rnorm(N) + threshold_shift[3] * group
y4 <- lambda[4] * Author + rnorm(N) + threshold_shift[4] * group

############################################################
## 4. Discretize into ordered categories
############################################################
cuts <- c(-Inf, -1, 0, 1, Inf)

sim_data <- data.frame(
  group = group,
  auth_1 = ordered(cut(y1, cuts)),
  auth_2 = ordered(cut(y2, cuts)),
  auth_3 = ordered(cut(y3, cuts)),
  auth_4 = ordered(cut(y4, cuts))
)

############################################################
## 5. RAW SCALE DIFFERENCE (BIASED)
############################################################
sim_data$auth_raw <- rowMeans(
  sapply(sim_data[paste0("auth_", 1:4)], as.numeric)
)

delta_raw <- mean(sim_data$auth_raw[group == 1]) -
  mean(sim_data$auth_raw[group == 0])

############################################################
## 6. METRIC-INVARIANCE CFA (BIASED)
############################################################
model <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

fit_metric <- cfa(
  model,
  data = sim_data,
  group = "group",
  ordered = paste0("auth_", 1:4),
  group.equal = "loadings",
  estimator = "WLSMV"
)

fs_metric <- lavPredict(fit_metric, type = "lv")

# Group 1 (reference: White)
mean_white <- mean(fs_metric[[1]][, "Author"], na.rm = TRUE)

# Group 2 (Black)
mean_black <- mean(fs_metric[[2]][, "Author"], na.rm = TRUE)

# Latent mean difference
delta_metric <- mean_black - mean_white
delta_metric

############################################################
## 7. PARTIAL SCALAR INVARIANCE CFA (UNBIASED)
############################################################
fit_partial <- cfa(
  model,
  data = sim_data,
  group = "group",
  ordered = paste0("auth_", 1:4),
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pe <- parameterEstimates(fit_partial)

lv_means <- subset(
  pe,
  lhs == "Author" & op == "~1",
  select = c(group, est, se)
)

delta_partial <- lv_means$est[lv_means$group == 2] -
  lv_means$est[lv_means$group == 1]

############################################################
## 8. Results summary
############################################################
cat("\n========================================\n")
cat("TRUE latent mean difference:", true_delta, "\n\n")

cat("Raw scale difference:", round(delta_raw, 3), "\n")
cat("Metric-only CFA difference:", round(delta_metric, 3), "\n")
cat("Partial scalar CFA difference:", round(delta_partial, 3), "\n")
cat("========================================\n")



######. Monte Carlo Simulation


library(lavaan)

set.seed(2025)

############################################################
## 1. Simulation parameters
############################################################
R <- 500            # number of replications
N <- 1000
true_delta <- 0.5

############################################################
## 2. Storage
############################################################
results <- data.frame(
  raw      = rep(NA, R),
  metric   = rep(NA, R),
  partial  = rep(NA, R)
)

############################################################
## 3. Monte Carlo loop
############################################################
for (r in 1:R) {
  
  group <- rep(0:1, each = N/2)
  
  ## latent variable
  Author <- rnorm(N, mean = true_delta * group)
  
  ## DIF via thresholds
  y1 <- Author + rnorm(N)
  y2 <- Author + rnorm(N) + 0.7 * group
  y3 <- Author + rnorm(N) + 0.7 * group
  y4 <- Author + rnorm(N) + 0.7 * group
  
  cuts <- c(-Inf, -1, 0, 1, Inf)
  
  sim_data <- data.frame(
    group = group,
    auth_1 = ordered(cut(y1, cuts)),
    auth_2 = ordered(cut(y2, cuts)),
    auth_3 = ordered(cut(y3, cuts)),
    auth_4 = ordered(cut(y4, cuts))
  )
  
  ##########################################################
  ## RAW difference
  ##########################################################
  sim_data$auth_raw <- rowMeans(
    sapply(sim_data[paste0("auth_", 1:4)], as.numeric)
  )
  
  results$raw[r] <- mean(sim_data$auth_raw[group == 1]) -
    mean(sim_data$auth_raw[group == 0])
  
  ##########################################################
  ## METRIC CFA
  ##########################################################
  model <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'
  
  fit_metric <- try(
    cfa(
      model,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:4),
      group.equal = "loadings",
      estimator = "WLSMV"
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_metric, "try-error")) {
    fs <- lavPredict(fit_metric, type = "lv")
    results$metric[r] <- mean(fs[[2]][, "Author"]) -
      mean(fs[[1]][, "Author"])
  }
  
  ##########################################################
  ## PARTIAL SCALAR CFA
  ##########################################################
  fit_partial <- try(
    cfa(
      model,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:4),
      meanstructure = TRUE,
      estimator = "WLSMV",
      group.equal = c("loadings", "thresholds"),
      group.partial = c(
        "auth_2|t1",
        "auth_3|t1",
        "auth_4|t1"
      )
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_partial, "try-error")) {
    pe <- parameterEstimates(fit_partial)
    lv <- subset(pe, lhs == "Author" & op == "~1")
    results$partial[r] <- lv$est[lv$group == 2] -
      lv$est[lv$group == 1]
  }
}

############################################################
## 4. Monte Carlo summary
############################################################
summarize <- function(x) {
  x <- x[!is.na(x)]
  c(
    Mean  = mean(x),
    Bias  = mean(x) - true_delta,
    RMSE  = sqrt(mean((x - true_delta)^2)),
    CI_Low = quantile(x, 0.025),
    CI_High= quantile(x, 0.975)
  )
}

mc_results <- rbind(
  Raw     = summarize(results$raw),
  Metric  = summarize(results$metric),
  Partial = summarize(results$partial)
)

round(mc_results, 3)




############.  Plot



library(ggplot2)

df_plot <- data.frame(
  Method = factor(
    c("Multi-Item scale", "Metric invariance", "Partial invariance"),
    levels = c("Multi-Item scale", "Metric invariance", "Partial invariance")
  ),
  Mean = c(0.688, -0.021, 0.356),
  CI_Low = c(0.593, -0.029, 0.229),
  CI_High = c(0.782, -0.013, 0.481)
)

ggplot(df_plot, aes(x = Mean, y = Method, color = Method)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = CI_Low, xmax = CI_High),
    height = 0.2
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(-0.3, 1)) +
  labs(
    x = "Simulated Difference",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background  = element_blank(),
    legend.position  = "none"
  )


##########.  New code 

library(lavaan)

set.seed(2025)

############################################################
## 1. Simulation parameters
############################################################
R <- 500
N <- 1000
true_delta <- 0.5

############################################################
## 2. Storage
############################################################
results <- data.frame(
  raw     = rep(NA, R),
  metric  = rep(NA, R),
  partial = rep(NA, R)
)

############################################################
## 3. Monte Carlo loop
############################################################
for (r in 1:R) {
  
  group <- rep(0:1, each = N/2)
  
  ## latent variable
  Author <- rnorm(N, mean = true_delta * group)
  
  ## continuous responses
  y1 <- Author + rnorm(N)                 # anchor item (no DIF)
  y2 <- Author + rnorm(N) + 0.7 * group   # DIF items
  y3 <- Author + rnorm(N) + 0.7 * group
  y4 <- Author + rnorm(N) + 0.7 * group
  
  ## thresholds
  cuts <- c(-Inf, -1, 0, 1, Inf)
  
  sim_data <- data.frame(
    group  = group,
    auth_1 = ordered(cut(y1, cuts)),
    auth_2 = ordered(cut(y2, cuts)),
    auth_3 = ordered(cut(y3, cuts)),
    auth_4 = ordered(cut(y4, cuts))
  )
  
  ##########################################################
  ## RAW difference
  ##########################################################
  sim_data$auth_raw <- rowMeans(
    sapply(sim_data[paste0("auth_", 1:4)], as.numeric)
  )
  
  results$raw[r] <-
    mean(sim_data$auth_raw[group == 1]) -
    mean(sim_data$auth_raw[group == 0])
  
  ##########################################################
  ## METRIC CFA (biased)
  ##########################################################
  model <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'
  
  fit_metric <- try(
    cfa(
      model,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:4),
      group.equal = "loadings",
      estimator = "WLSMV"
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_metric, "try-error")) {
    fs <- lavPredict(fit_metric, type = "lv")
    results$metric[r] <-
      mean(fs[[2]][, "Author"]) -
      mean(fs[[1]][, "Author"])
  }
  
  ##########################################################
  ## CORRECT PARTIAL SCALAR CFA (UNBIASED)
  ##########################################################
  fit_partial <- try(
    cfa(
      model,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:4),
      meanstructure = TRUE,
      estimator = "WLSMV",
      group.equal = c("loadings", "thresholds"),
      group.partial = c(
        paste0("auth_2|t", 1:3),
        paste0("auth_3|t", 1:3),
        paste0("auth_4|t", 1:3)
      )
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_partial, "try-error")) {
    pe <- parameterEstimates(fit_partial)
    lv <- subset(pe, lhs == "Author" & op == "~1")
    results$partial[r] <-
      lv$est[lv$group == 2] -
      lv$est[lv$group == 1]
  }
}

############################################################
## 4. Monte Carlo summary
############################################################
summarize <- function(x) {
  x <- x[!is.na(x)]
  c(
    Mean   = mean(x),
    Bias   = mean(x) - true_delta,
    RMSE   = sqrt(mean((x - true_delta)^2)),
    CI_Low = quantile(x, 0.025),
    CI_High= quantile(x, 0.975)
  )
}

mc_results <- rbind(
  Raw     = summarize(results$raw),
  Metric  = summarize(results$metric),
  Partial = summarize(results$partial)
)

round(mc_results, 3)




###### new code 
library(lavaan)

set.seed(2026)

############################################################
## 1. Parameters
############################################################
R <- 100
N <- 500
true_delta <- 0.5

J <- 8
dif_items    <- paste0("auth_", 1:4)   # DIF items
anchor_items <- paste0("auth_", 5:8)   # anchor items

cuts <- c(-Inf, -1, 0, 1, Inf)
n_thresh <- length(cuts) - 1

############################################################
## 2. Storage
############################################################
results <- data.frame(
  raw     = rep(NA, R),
  metric  = rep(NA, R),
  partial = rep(NA, R)
)

############################################################
## 3. Monte Carlo loop
############################################################
for (r in 1:R) {
  
  ## group indicator
  group <- rep(0:1, each = N / 2)
  
  ## latent variable with true mean difference
  Author <- rnorm(N, mean = true_delta * group)
  
  sim_data <- data.frame(group = group)
  
  ## ----------------------------
  ## Generate ordinal items
  ## ----------------------------
  for (j in 1:J) {
    
    item <- paste0("auth_", j)
    
    ## invariant factor loading
    lambda <- 0.6
    
    ## continuous response
    y <- lambda * Author + rnorm(N)
    
    ## threshold DIF for first 4 items
    if (item %in% dif_items) {
      y <- y + 0.05 * group
    }
    
    sim_data[[item]] <- ordered(cut(y, cuts))
  }
  
  ##########################################################
  ## RAW SCALE (biased)
  ##########################################################
  sim_data$raw <- rowMeans(
    sapply(sim_data[dif_items], as.numeric)
  )
  
  results$raw[r] <- mean(sim_data$raw[group == 1]) -
    mean(sim_data$raw[group == 0])
  
  ##########################################################
  ## METRIC CFA (biased)
  ##########################################################
  model_metric <- paste0(
    "Author =~ ",
    paste(dif_items, collapse = " + ")
  )
  
  fit_metric <- try(
    cfa(
      model_metric,
      data = sim_data,
      group = "group",
      ordered = dif_items,
      meanstructure = TRUE,      # needed to estimate latent means
      group.equal = "loadings",  # metric invariance
      estimator = "WLSMV"
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_metric, "try-error")) {
    pe <- parameterEstimates(fit_metric)
    lv <- subset(pe, lhs == "Author" & op == "~1")
    lv <- lv[order(lv$group), ]
    
    delta_metric <- lv$est[lv$group == 2] - lv$est[lv$group == 1]
    results$metric[r] <- delta_metric
  }
  
  
## I try this old version code, it works.   
  model <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'
  
  fit_metric <- try(
    cfa(
      model,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:4),
      group.equal = "loadings",
      meanstructure = TRUE, 
      estimator = "WLSMV"
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_metric, "try-error")) {
    fs <- lavPredict(fit_metric, type = "lv")
    results$metric[r] <- mean(fs[[2]][, "Author"]) -
      mean(fs[[1]][, "Author"])
  }
  
  ##########################################################
  ## PARTIAL SCALAR CFA (CORRECT, ANCHORED)
  ##########################################################
  model_full <- paste0(
    "Author =~ ",
    paste(paste0("auth_", 1:J), collapse = " + ")
  )
  
  ## free thresholds ONLY for DIF items
  free_thresholds <- as.vector(
    sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh))
  )
  
  fit_partial <- try(
    cfa(
      model_full,
      data = sim_data,
      group = "group",
      ordered = paste0("auth_", 1:J),
      meanstructure = TRUE,
      estimator = "WLSMV",
      group.equal = c("loadings", "thresholds", "lv.variances"),
      group.partial = free_thresholds
    ),
    silent = TRUE
  )
  
  if (!inherits(fit_partial, "try-error")) {
    pe <- parameterEstimates(fit_partial)
    lv <- subset(pe, lhs == "Author" & op == "~1")
    results$partial[r] <- lv$est[lv$group == 2] -
      lv$est[lv$group == 1]
  }
}

############################################################
## 4. Monte Carlo summary
############################################################
summarize <- function(x) {
  x <- x[!is.na(x)]
  c(
    Mean   = mean(x),
    Bias   = mean(x) - true_delta,
    RMSE   = sqrt(mean((x - true_delta)^2)),
    CI_Low = quantile(x, 0.025),
    CI_High= quantile(x, 0.975)
  )
}

mc_results <- rbind(
  Raw     = summarize(results$raw),
  Metric  = summarize(results$metric),
  Partial = summarize(results$partial)
)

round(mc_results, 3)







######. updated code. 


library(lavaan)

set.seed(2026)

############################################################
## 1. Parameters
############################################################
R <- 100          # Monte Carlo repetitions
N <- 500          # Sample size per group
true_delta <- 0.2 # True latent mean difference
J <- 8            # Total items
dif_items <- paste0("auth_", 1:4)  # Items with DIF
anchor_items <- paste0("auth_", 5:8) # Anchor items
cuts <- c(-Inf, -1, 0, 1, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9) # DIF magnitudes to test

results_all <- list()

############################################################
## 2. Monte Carlo loop over DIF magnitude
############################################################
for (dif_shift in dif_values) {
  
  results <- data.frame(
    raw     = rep(NA, R),
    partial = rep(NA, R)
  )
  
  for (r in 1:R) {
    # 2 groups
    group <- rep(0:1, each = N/2)
    Author <- rnorm(N, mean = true_delta * group)
    
    sim_data <- data.frame(group = group)
    
    # Generate items
    for (j in 1:J) {
      item <- paste0("auth_", j)
      lambda <- 0.6
      y <- lambda * Author + rnorm(N)
      
      # Add DIF for first 4 items
      if (item %in% dif_items) {
        y <- y + dif_shift * group
      }
      
      sim_data[[item]] <- ordered(cut(y, cuts))
    }
    
    # ---------------------
    # Raw mean difference
    # ---------------------
    sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
    results$raw[r] <- mean(sim_data$raw[group == 1]) -
      mean(sim_data$raw[group == 0])
    
    # ---------------------
    # Partial invariance CFA
    # ---------------------
    model_full <- paste0(
      "Author =~ ",
      paste(paste0("auth_", 1:J), collapse = " + ")
    )
    
    free_thresholds <- as.vector(
      sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh))
    )
    
    fit_partial <- try(
      cfa(
        model_full,
        data = sim_data,
        group = "group",
        ordered = paste0("auth_", 1:J),
        meanstructure = TRUE,
        estimator = "WLSMV",
        group.equal = c("loadings", "thresholds"),
        group.partial = free_thresholds
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_partial, "try-error")) {
      pe <- parameterEstimates(fit_partial)
      lv <- subset(pe, lhs == "Author" & op == "~1")
      # order by group to ensure group 0 first, group 1 second
      lv <- lv[order(lv$group), ]
      results$partial[r] <- lv$est[2] - lv$est[1]
    }
  }
  
  results_all[[paste0("DIF_", dif_shift)]] <- results
}

############################################################
## 3. Summarize
############################################################
summarize <- function(x, true_delta) {
  x <- x[!is.na(x)]
  c(
    Mean   = mean(x),
    Bias   = mean(x) - true_delta,
    RMSE   = sqrt(mean((x - true_delta)^2)),
    CI_Low = quantile(x, 0.025),
    CI_High= quantile(x, 0.975)
  )
}

summary_table <- do.call(rbind, lapply(names(results_all), function(nm) {
  res <- results_all[[nm]]
  rbind(
    Raw     = summarize(res$raw, true_delta),
    Partial = summarize(res$partial, true_delta)
  )
}))

round(summary_table, 3)






############.  Line plot


library(ggplot2)

# Prepare data for plotting
plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
  dif_level <- as.numeric(sub("DIF_", "", nm))
  res <- results_all[[nm]]
  data.frame(
    DIF = dif_level,
    Method = rep(c("Raw", "Partial"), each = nrow(res)),
    Estimate = c(res$raw, res$partial)
  )
}))

# Compute mean estimates per DIF level
plot_summary <- aggregate(Estimate ~ DIF + Method, data = plot_data, FUN = mean)

# Line plot
ggplot(plot_summary, aes(x = DIF, y = Estimate, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = "Performance of Raw vs Partial Invariance CFA Across DIF Levels",
    subtitle = paste0("Dashed line = True latent mean difference (", true_delta, ")")
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "darkorange"))





####



library(ggplot2)

# Example DIF levels corresponding to your table rows
dif_levels <- c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9)  # adjust if different

# Convert summary table to long format
summary_long <- data.frame(
  DIF = rep(dif_levels, each = 2),
  Method = rep(c("Raw", "Partial"), times = length(dif_levels)),
  Mean = c(
    0.133, 0.097,
    0.181, 0.106,
    0.336, 0.097,
    0.496, 0.100,
    0.635, 0.102,
    0.779, 0.099
  ),
  CI_Low = c(
    0.040, -0.035,
    0.071, 0.012,
    0.213, -0.023,
    0.393, -0.026,
    0.535, -0.007,
    0.682, -0.026
  ),
  CI_High = c(
    0.240, 0.193,
    0.284, 0.250,
    0.454, 0.197,
    0.603, 0.235,
    0.739, 0.214,
    0.892, 0.223
  )
)


summary_long$Method <- recode(summary_long$Method,
                              Raw = "Multi-item scale",
                              Partial = "Partial invariance")

ggplot(summary_long, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_Low, ymax = CI_High, fill = Method), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = "Partial Invariance Vs Multi-Item Scale Across DIF Levels",
    subtitle = "Dashed line = True latent mean difference (0.2)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "darkorange")) +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  scale_x_continuous(breaks = seq(0.05, 0.9, by = 0.1))



###

library(dplyr)
library(tidyr)
library(ggplot2)

# Example: add Metric results (replace with your actual metric results)
# Assuming you have a vector metric_means with the same length as DIF levels
# For demonstration, let's simulate metric_means near 0
metric_means <- rep(0.02, length(unique(summary_long$DIF)))  # replace with actual values
metric_CI_low <- rep(-0.01, length(metric_means))
metric_CI_high <- rep(0.05, length(metric_means))

# Prepare data frame
metric_df <- data.frame(
  DIF = unique(summary_long$DIF),
  Method = "Metric invariance",
  Mean = metric_means,
  CI_Low = metric_CI_low,
  CI_High = metric_CI_high
)

# Combine with your existing summary_long
summary_long_full <- bind_rows(summary_long, metric_df)

# Make sure Method is a factor for consistent color order
summary_long_full$Method <- factor(summary_long_full$Method,
                                   levels = c("Multi-item scale", "Partial invariance", "Metric invariance"))

# Plot
ggplot(summary_long_full, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_Low, ymax = CI_High, fill = Method), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  scale_color_manual(values = c("steelblue", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("steelblue", "darkorange", "darkgreen")) +
  scale_x_continuous(breaks = seq(0.05, 0.9, by = 0.1))





######## In this code, I modified to make it closer to 0.2 
library(lavaan)

set.seed(2026)

############################################################
## 1. Parameters
############################################################
R <- 100          # Monte Carlo repetitions
N <- 1000         # Sample size per group
true_delta <- 2.0 # True latent mean difference
J <- 8            # Total items
dif_items <- paste0("auth_", 1:4)     # Items with DIF
anchor_items <- paste0("auth_", 5:8)  # Anchor items

# Ordinal cuts
cuts <- c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0.1, 0.3, 0.5, 0.7, 0.9) # DIF magnitudes
results_all <- list()

# Item parameters
lambda_dif <- 1.0
lambda_anchor <- 1.2
resid_sd <- 0.3

############################################################
## 2. Monte Carlo loop over DIF magnitude
############################################################
for (dif_shift in dif_values) {
  
  results <- data.frame(
    raw     = rep(NA, R),
    partial = rep(NA, R)
  )
  
  for (r in 1:R) {
    # 2 groups
    group <- rep(0:1, each = N/2)
    Author <- rnorm(N, mean = true_delta * group, sd = 1)
    
    sim_data <- data.frame(group = group)
    
    # Generate items
    for (j in 1:J) {
      item <- paste0("auth_", j)
      lambda <- ifelse(item %in% anchor_items, lambda_anchor, lambda_dif)
      y <- lambda * Author + rnorm(N, sd = resid_sd)
      if (item %in% dif_items) y <- y + dif_shift * group
      sim_data[[item]] <- ordered(cut(y, cuts))
    }
    
    # ---------------------
    # Raw mean difference
    # ---------------------
    sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
    results$raw[r] <- mean(sim_data$raw[group == 1]) -
      mean(sim_data$raw[group == 0])
    
    # ---------------------
    # Partial invariance CFA
    # ---------------------
    model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
    free_thresholds <- as.vector(
      sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh))
    )
    
    fit_partial <- try(
      cfa(
        model_full,
        data = sim_data,
        group = "group",
        ordered = paste0("auth_", 1:J),
        meanstructure = TRUE,
        estimator = "WLSMV",
        group.equal = c("loadings", "thresholds"),
        group.partial = free_thresholds
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_partial, "try-error")) {
      pe <- parameterEstimates(fit_partial)
      # get latent mean for each group
      lv <- subset(pe, lhs == "Author" & op == "~1")
      lv <- lv[order(lv$group), ]  # group 0 first, group 1 second
      if (nrow(lv) == 2) {
        results$partial[r] <- lv$est[2] - lv$est[1]
      }
    }
  }
  
  results_all[[paste0("DIF_", dif_shift)]] <- results
}

############################################################
## 3. Summarize
############################################################
summarize <- function(x, true_delta) {
  x <- x[!is.na(x)]
  c(
    Mean   = mean(x),
    Bias   = mean(x) - true_delta,
    RMSE   = sqrt(mean((x - true_delta)^2)),
    CI_Low = quantile(x, 0.025),
    CI_High= quantile(x, 0.975)
  )
}

# Build summary table
summary_table <- do.call(rbind, lapply(names(results_all), function(nm) {
  res <- results_all[[nm]]
  tmp <- rbind(
    Raw     = summarize(res$raw, true_delta),
    Partial = summarize(res$partial, true_delta)
  )
  rownames(tmp) <- paste0(rownames(tmp), "_", nm)  # unique rownames
  tmp
}))

summary_table <- round(summary_table, 3)
print(summary_table)

############################################################
## 4. Overall mean of raw and partial across DIF levels
############################################################
raw_means     <- summary_table[grep("Raw", rownames(summary_table)), "Mean"]
partial_means <- summary_table[grep("Partial", rownames(summary_table)), "Mean"]

# Remove NA before taking mean
partial_means_clean <- partial_means[!is.na(partial_means)]

cat("Overall mean of Raw:     ", round(mean(raw_means), 3), "\n")
cat("Overall mean of Partial: ", round(mean(partial_means_clean), 3), "\n")




##########. Mean of Raw=2.335, and mean of partial is 1.948, which is very close to 2.0. 


library(lavaan)
library(ggplot2)




set.seed(2026)

############################################################
# 1. Simulation Parameters
############################################################
R <- 100          # Monte Carlo repetitions
N <- 1000         # Sample size per group
true_delta <- 2.0 # True latent mean difference
J <- 8            # Total items
dif_items <- paste0("auth_", 1:4)     # Items with DIF
anchor_items <- paste0("auth_", 5:8)  # Anchor items

# Ordinal cuts
cuts <- c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7) # DIF magnitudes
results_all <- list()

# Item parameters
lambda_dif <- 1.0
lambda_anchor <- 1.0
resid_sd <- 0.2

############################################################
# 2. Monte Carlo Simulation
############################################################
for (dif_shift in dif_values) {
  
  results <- data.frame(
    raw     = rep(NA, R),
    partial = rep(NA, R)
  )
  
  for (r in 1:R) {
    # 2 groups
    group <- rep(0:1, each = N/2)
    Author <- rnorm(N, mean = true_delta * group) # latent factor
    
    sim_data <- data.frame(group = group)
    
    # Generate items
    for (j in 1:J) {
      item <- paste0("auth_", j)
      lambda <- ifelse(item %in% anchor_items, lambda_anchor, lambda_dif)
      y <- lambda * Author + rnorm(N, sd = resid_sd)
      
      # Add DIF for first 4 items
      if (item %in% dif_items) y <- y + dif_shift * group
      
      sim_data[[item]] <- ordered(cut(y, cuts))
    }
    
    # ---------------------
    # Raw mean difference
    # ---------------------
    sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
    results$raw[r] <- mean(sim_data$raw[group == 1]) -
      mean(sim_data$raw[group == 0])
    
    # ---------------------
    # Partial invariance CFA
    # ---------------------
    model_full <- paste0(
      "Author =~ ", paste(paste0("auth_", 1:J), collapse = " + ")
    )
    
    free_thresholds <- as.vector(
      sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh))
    )
    
    fit_partial <- try(
      cfa(
        model_full,
        data = sim_data,
        group = "group",
        ordered = paste0("auth_", 1:J),
        meanstructure = TRUE,
        estimator = "WLSMV",
        group.equal = c("loadings", "thresholds"),
        group.partial = free_thresholds
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_partial, "try-error") && inspect(fit_partial, "converged")) {
      pe <- parameterEstimates(fit_partial)
      lv <- subset(pe, lhs == "Author" & op == "~1")
      lv <- lv[order(lv$group), ]
      if (nrow(lv) == 2) results$partial[r] <- lv$est[2] - lv$est[1]
    }
  }
  
  results_all[[paste0("DIF_", dif_shift)]] <- results
}

############################################################
# 3. Prepare data for plotting
############################################################
plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
  dif_level <- as.numeric(sub("DIF_", "", nm))
  res <- results_all[[nm]]
  data.frame(
    DIF = dif_level,
    Method = rep(c("Multi-Item", "Partial"), each = nrow(res)),
    Estimate = c(res$raw, res$partial)
  )
}))

# Remove NA estimates from partial
plot_data <- plot_data[!is.na(plot_data$Estimate), ]

# Compute mean and SD per DIF level
plot_summary <- aggregate(Estimate ~ DIF + Method, data = plot_data,
                          FUN = function(x) c(Mean = mean(x), SD = sd(x)))
plot_summary <- do.call(data.frame, plot_summary)
names(plot_summary)[3:4] <- c("Mean", "SD")

############################################################
# 4. Plot
############################################################
ggplot(plot_summary, aes(x = DIF, y = Mean, color = Method, group = Method, fill = Method)) +
  geom_line(size = 1.2) +
  
  # Points: triangle for blue, circle for second
  geom_point(aes(shape = Method), size = 3, stroke = 1.2, color = "black") +
  
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = "Multi-Item vs Partial Invariance CFA Across DIF Levels"
  ) +
  
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  scale_fill_manual(values = c("steelblue", "#9A1543")) +
  scale_shape_manual(values = c(17, 21)) +
  scale_x_continuous(breaks = dif_values) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Outline for the plot area
  )








############# combined plot 



library(lavaan)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(2026)

############################################################
# 1. Simulation parameters
############################################################
R <- 100
N <- 2000
true_delta <- 2.0
J <- 8
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)
cuts <- c(-Inf, 1, 0, 0.5, Inf)
n_thresh <- length(cuts) - 1
dif_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

lambda_dif_values <- c(0.7, 0.8, 1.0)
resid_sd_values <- c(0.2, 0.3, 0.4)
param_grid <- expand.grid(lambda_dif = lambda_dif_values,
                          resid_sd = resid_sd_values)

############################################################
# 2. Run simulations
############################################################
all_results <- list()

for (p in 1:nrow(param_grid)) {
  lambda_dif <- param_grid$lambda_dif[p]
  resid_sd <- param_grid$resid_sd[p]
  
  results_all <- list()
  
  for (dif_shift in dif_values) {
    results <- data.frame(raw = rep(NA, R), partial = rep(NA, R))
    
    for (r in 1:R) {
      group <- rep(0:1, each = N/2)
      Author <- rnorm(N, mean = true_delta * group)
      sim_data <- data.frame(group = group)
      
      for (j in 1:J) {
        item <- paste0("auth_", j)
        lambda <- ifelse(item %in% anchor_items, 1.0, lambda_dif)
        y <- lambda * Author + rnorm(N, sd = resid_sd)
        if (item %in% dif_items) y <- y + dif_shift * group
        sim_data[[item]] <- ordered(cut(y, cuts))
      }
      
      # Raw difference
      sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
      results$raw[r] <- mean(sim_data$raw[group == 1]) - mean(sim_data$raw[group == 0])
      
      # Partial invariance CFA
      model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
      free_thresholds <- as.vector(sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh)))
      
      fit_partial <- try(
        cfa(model_full, data = sim_data, group = "group",
            ordered = paste0("auth_", 1:J), meanstructure = TRUE,
            estimator = "WLSMV", group.equal = c("loadings","thresholds"),
            group.partial = free_thresholds),
        silent = TRUE
      )
      
      if (!inherits(fit_partial, "try-error") && inspect(fit_partial, "converged")) {
        pe <- parameterEstimates(fit_partial)
        lv <- subset(pe, lhs == "Author" & op == "~1")
        lv <- lv[order(lv$group), ]
        if (nrow(lv) == 2) results$partial[r] <- lv$est[2] - lv$est[1]
      }
    }
    
    results_all[[paste0("DIF_", dif_shift)]] <- results
  }
  
  # Combine results for plotting
  plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
    dif_level <- as.numeric(sub("DIF_", "", nm))
    res <- results_all[[nm]]
    data.frame(
      DIF = dif_level,
      Method = rep(c("Raw", "Partial"), each = nrow(res)),
      Estimate = c(res$raw, res$partial)
    )
  }))
  
  # Add parameter info
  plot_data$lambda_dif <- lambda_dif
  plot_data$resid_sd <- resid_sd
  
  all_results[[p]] <- plot_data
}

all_results_df <- bind_rows(all_results)

############################################################
# 3. Average estimates per DIF and parameter combination
############################################################
summary_df <- all_results_df %>%
  group_by(DIF, Method, lambda_dif, resid_sd) %>%
  summarise(Mean = mean(Estimate, na.rm = TRUE), .groups = "drop")

############################################################
# 4. Plot 9 panels
############################################################
ggplot(summary_df, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(aes(shape = Method), size = 3, stroke = 1.2, color = "black") +
  scale_shape_manual(values = c(2, 16)) + # triangle for Raw, circle for Partial
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  facet_grid(resid_sd ~ lambda_dif, labeller = label_both) +
  labs(x = "DIF Magnitude", y = "Estimated Group Difference",
       title = "Raw vs Partial Invariance CFA Across DIF Levels") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))



###### This is only for binary 





############# Only for Binary



library(lavaan)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(2026)

############################################################
# 1. Simulation parameters
############################################################
R <- 100
N <- 1000
true_delta <- 2.0
J <- 8
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)
cuts <- c(-Inf, 0, Inf)
n_thresh <- length(cuts) - 1
dif_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

lambda_dif_values <- c(1.0)
resid_sd_values <- c(0.2)
param_grid <- expand.grid(lambda_dif = lambda_dif_values,
                          resid_sd = resid_sd_values)

############################################################
# 2. Run simulations
############################################################
all_results <- list()

for (p in 1:nrow(param_grid)) {
  lambda_dif <- param_grid$lambda_dif[p]
  resid_sd <- param_grid$resid_sd[p]
  
  results_all <- list()
  
  for (dif_shift in dif_values) {
    results <- data.frame(raw = rep(NA, R), partial = rep(NA, R))
    
    for (r in 1:R) {
      group <- rep(0:1, each = N/2)
      Author <- rnorm(N, mean = true_delta * group)
      sim_data <- data.frame(group = group)
      
      for (j in 1:J) {
        item <- paste0("auth_", j)
        lambda <- ifelse(item %in% anchor_items, 1.0, lambda_dif)
        y <- lambda * Author + rnorm(N, sd = resid_sd)
        if (item %in% dif_items) y <- y + dif_shift * group
        sim_data[[item]] <- ordered(cut(y, cuts))
      }
      
      # Raw difference
      sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
      results$raw[r] <- mean(sim_data$raw[group == 1]) - mean(sim_data$raw[group == 0])
      
      # Partial invariance CFA
      model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
      free_thresholds <- as.vector(sapply(dif_items, function(x) paste0(x, "|t", 1:n_thresh)))
      
      fit_partial <- try(
        cfa(model_full, data = sim_data, group = "group",
            ordered = paste0("auth_", 1:J), meanstructure = TRUE,
            estimator = "WLSMV", group.equal = c("loadings","thresholds"),
            group.partial = free_thresholds),
        silent = TRUE
      )
      
      if (!inherits(fit_partial, "try-error") && inspect(fit_partial, "converged")) {
        pe <- parameterEstimates(fit_partial)
        lv <- subset(pe, lhs == "Author" & op == "~1")
        lv <- lv[order(lv$group), ]
        if (nrow(lv) == 2) results$partial[r] <- lv$est[2] - lv$est[1]
      }
    }
    
    results_all[[paste0("DIF_", dif_shift)]] <- results
  }
  
  # Combine results for plotting
  plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
    dif_level <- as.numeric(sub("DIF_", "", nm))
    res <- results_all[[nm]]
    data.frame(
      DIF = dif_level,
      Method = rep(c("Raw", "Partial"), each = nrow(res)),
      Estimate = c(res$raw, res$partial)
    )
  }))
  
  # Add parameter info
  plot_data$lambda_dif <- lambda_dif
  plot_data$resid_sd <- resid_sd
  
  all_results[[p]] <- plot_data
}

all_results_df <- bind_rows(all_results)

############################################################
# 3. Average estimates per DIF and parameter combination
############################################################
summary_df <- all_results_df %>%
  group_by(DIF, Method, lambda_dif, resid_sd) %>%
  summarise(Mean = mean(Estimate, na.rm = TRUE), .groups = "drop")

############################################################
# 4. Plot 9 panels
############################################################
ggplot(summary_df, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(aes(shape = Method), size = 3, stroke = 1.2, color = "black") +
  scale_shape_manual(values = c(2, 16)) + # triangle for Raw, circle for Partial
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  facet_grid(resid_sd ~ lambda_dif, labeller = label_both) +
  labs(x = "DIF Magnitude", y = "Estimated Group Difference",
       title = "Raw vs Partial Invariance CFA Across DIF Levels") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))


