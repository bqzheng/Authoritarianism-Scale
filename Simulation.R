

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
free_thresholds <- paste0(dif_items, "|t1")


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





########### I try to create the partial invariance with using anchor items 



set.seed(2026)

############################################################
# 1. Simulation Parameters
############################################################
R <- 100
N <- 1000
true_delta <- 2.0
J <- 8

# All items
items <- paste0("auth_", 1:J)

# Ordinal cuts
cuts <- c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
n_thresh <- length(cuts) - 1

# DIF magnitudes
dif_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
results_all <- list()

# Item parameters
lambda <- 1.0
error_variances <- c(0.01, 0.05, 0.10, 0.15, 0.25)

############################################################
# 2. Monte Carlo Simulation
############################################################
for (dif_shift in dif_values) {
  
  results <- data.frame(
    raw = rep(NA, R),
    cfa = rep(NA, R)
  )
  
  for (r in 1:R) {
    
    # Two groups
    group <- rep(0:1, each = N/2)
    Author <- rnorm(N, mean = true_delta * group)
    
    sim_data <- data.frame(group = group)
    
    # Generate ALL items with DIF
    for (j in 1:J) {
      item <- paste0("auth_", j)
      
      y <- lambda * Author + rnorm(N, mean = 0, sd = sqrt(error_variance))
      
      # DIF applied to ALL items
      y <- y + dif_shift * group
      
      sim_data[[item]] <- ordered(cut(y, cuts))
    }
    
    # ---------------------
    # Raw mean difference
    # ---------------------
    sim_data$raw <- rowMeans(sapply(sim_data[items], as.numeric))
    results$raw[r] <- mean(sim_data$raw[group == 1]) -
      mean(sim_data$raw[group == 0])
    
    # ---------------------
    # CFA (NO anchor items)
    # ---------------------
    model_full <- paste0(
      "Author =~ ", paste(items, collapse = " + ")
    )
    
    fit_cfa <- try(
      cfa(
        model_full,
        data = sim_data,
        group = "group",
        ordered = items,
        meanstructure = TRUE,
        estimator = "WLSMV",
        
        # Only constrain loadings
        group.equal = c("loadings")
        
        # No threshold constraints, no partial invariance
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_cfa, "try-error") && inspect(fit_cfa, "converged")) {
      pe <- parameterEstimates(fit_cfa)
      lv <- subset(pe, lhs == "Author" & op == "~1")
      lv <- lv[order(lv$group), ]
      
      if (nrow(lv) == 2) {
        results$cfa[r] <- lv$est[2] - lv$est[1]
      }
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
    Method = rep(c("Raw", "CFA (No Anchors)"), each = nrow(res)),
    Estimate = c(res$raw, res$cfa)
  )
}))

# Remove NA
plot_data <- plot_data[!is.na(plot_data$Estimate), ]

# Summary stats
plot_summary <- aggregate(
  Estimate ~ DIF + Method,
  data = plot_data,
  FUN = function(x) c(Mean = mean(x), SD = sd(x))
)

plot_summary <- do.call(data.frame, plot_summary)
names(plot_summary)[3:4] <- c("Mean", "SD")

############################################################
# 4. Plot
############################################################
library(ggplot2)

ggplot(plot_summary,
       aes(x = DIF, y = Mean, color = Method, group = Method, fill = Method)) +
  
  geom_line(size = 1.2) +
  
  geom_point(aes(shape = Method),
             size = 3, stroke = 1.2, color = "black") +
  
  geom_hline(yintercept = true_delta,
             linetype = "dashed", color = "black") +
  
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = "No Anchor Items: Raw vs CFA"
  ) +
  
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  scale_fill_manual(values = c("steelblue", "#9A1543")) +
  scale_shape_manual(values = c(17, 21)) +
  scale_x_continuous(breaks = dif_values) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )



####### compare anchor and without anchor items 




############################################################
# 1. Simulation parameters
############################################################
set.seed(2026)

library(lavaan)
library(ggplot2)

############################################################
# 1. Simulation Parameters
############################################################
R <- 100
N <- 1000
true_delta <- 2.0
J <- 8

items <- paste0("auth_", 1:J)

anchor_items <- paste0("auth_", 5:8)
dif_items <- paste0("auth_", 1:4)

cuts <- c(-Inf, -1, 0, 1, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)

lambda <- 1
error_variance <- c(0.01, 0.10, 0.15, 0.25)

results_all <- list()

############################################################
# 2. Monte Carlo Simulation
############################################################
for (dif_shift in dif_values) {
  
  results <- data.frame(
    raw = rep(NA, R),
    anchor_cfa = rep(NA, R),
    naive_cfa = rep(NA, R)
  )
  
  for (r in 1:R) {
    
    group <- rep(0:1, each = N/2)
    eta <- rnorm(N, mean = true_delta * group)
    
    sim_data <- data.frame(group = group)
    
    # Generate items
    for (j in 1:J) {
      
      item <- paste0("auth_", j)
      
      y <- lambda * eta +
        rnorm(N, mean = 0, sd = sqrt(error_variance))
      
      # DIF for first 4 items
      if (item %in% dif_items) {
        y <- y + dif_shift * group
      }
      
      sim_data[[item]] <- ordered(cut(y, cuts))
    }
    
    ########################################################
    # 1. Multi-item raw score
    ########################################################
    sim_data$raw <- rowMeans(sapply(sim_data[items], as.numeric))
    results$raw[r] <- mean(sim_data$raw[group == 1]) -
      mean(sim_data$raw[group == 0])
    
    ########################################################
    # Common model
    ########################################################
    model <- paste0("F =~ ", paste(items, collapse = " + "))
    
    ########################################################
    # 2. Anchor-based MG-CFA (correct model)
    ########################################################
    fit_anchor <- try(
      cfa(
        model,
        data = sim_data,
        group = "group",
        ordered = items,
        estimator = "WLSMV",
        meanstructure = TRUE,
        
        group.equal = c("loadings", "thresholds"),
        group.partial = paste0(
          dif_items, "|t", rep(1:n_thresh, each = length(dif_items))
        )
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_anchor, "try-error") &&
        inspect(fit_anchor, "converged")) {
      
      pe <- parameterEstimates(fit_anchor)
      lv <- subset(pe, lhs == "F" & op == "~1")
      lv <- lv[order(lv$group), ]
      
      if (nrow(lv) == 2) {
        results$anchor_cfa[r] <- lv$est[2] - lv$est[1]
      }
    }
    
    ########################################################
    # 3. No-anchor MG-CFA (misspecified)
    ########################################################
    fit_naive <- try(
      cfa(
        model,
        data = sim_data,
        group = "group",
        ordered = items,
        estimator = "WLSMV",
        meanstructure = TRUE,
        
        group.equal = c("loadings", "thresholds")
      ),
      silent = TRUE
    )
    
    if (!inherits(fit_naive, "try-error") &&
        inspect(fit_naive, "converged")) {
      
      pe <- parameterEstimates(fit_naive)
      lv <- subset(pe, lhs == "F" & op == "~1")
      lv <- lv[order(lv$group), ]
      
      if (nrow(lv) == 2) {
        results$naive_cfa[r] <- lv$est[2] - lv$est[1]
      }
    }
  }
  
  results_all[[paste0("DIF_", dif_shift)]] <- results
}

############################################################
# 3. Prepare plotting data
############################################################
plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
  
  dif_level <- as.numeric(sub("DIF_", "", nm))
  res <- results_all[[nm]]
  
  data.frame(
    DIF = dif_level,
    Method = rep(c("Multi-item", "Anchor-Based", "No Anchors"),
                 each = nrow(res)),
    Estimate = c(res$raw, res$anchor_cfa, res$naive_cfa)
  )
}))

# Remove missing
plot_data <- plot_data[!is.na(plot_data$Estimate), ]

# Fix ordering (IMPORTANT for correct mapping)
plot_data$Method <- factor(
  plot_data$Method,
  levels = c("Multi-item", "Anchor-Based", "No Anchors")
)

plot_summary <- aggregate(
  Estimate ~ DIF + Method,
  data = plot_data,
  FUN = mean
)

############################################################
# 4. Plot (your requested styling)
############################################################
ggplot(plot_summary,
       aes(x = DIF, y = Estimate,
           color = Method, group = Method)) +
  
  geom_line(size = 1.2) +
  
  geom_point(aes(shape = Method),
             size = 3,
             stroke = 1.2,
             color = "black") +
  
  scale_shape_manual(values = c(
    "Multi-item" = 2,     # blue triangle
    "Anchor-Based" = 16,   # red circle
    "No Anchors" = 17      # orange triangle
  )) +
  
  geom_hline(yintercept = true_delta,
             linetype = "dashed",
             color = "black") +
  
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = ""
  ) +
  
  scale_color_manual(values = c(
    "Multi-item" = "steelblue",
    "Anchor-Based" = "red",
    "No Anchors" = "orange"
  )) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 1)
  )




##########  Anchor vs No Anchor combined plot 





set.seed(2026)

library(lavaan)
library(ggplot2)

############################################################
# 1. Simulation parameters
############################################################
R <- 100
N <- 1000
true_delta <- 2.0
J <- 8

items <- paste0("auth_", 1:J)
dif_items <- paste0("auth_", 1:4)

############################################################
# Category structures
############################################################
cuts_list <- list(
  "3cat" = c(-Inf, -1, 1, Inf),
  "5cat" = c(-Inf, -1, 0, 1, Inf),
  "7cat" = c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
)

############################################################
# Experimental factors
############################################################
dif_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
error_variances <- c(0.01, 0.05, 0.10, 0.25)

lambda <- 0.8

results_all <- list()

############################################################
# 2. Monte Carlo simulation (full factorial)
############################################################
for (cat_name in names(cuts_list)) {
  
  cuts <- cuts_list[[cat_name]]
  
  for (error_variance in error_variances) {
    
    for (dif_shift in dif_values) {
      
      results <- data.frame(
        raw = rep(NA, R),
        anchor_cfa = rep(NA, R),
        naive_cfa = rep(NA, R)
      )
      
      for (r in 1:R) {
        
        group <- rep(0:1, each = N/2)
        eta <- rnorm(N, mean = true_delta * group)
        
        sim_data <- data.frame(group = group)
        
        ########################################################
        # Generate ordinal items
        ########################################################
        for (j in 1:J) {
          
          item <- paste0("auth_", j)
          
          y <- lambda * eta +
            rnorm(N, mean = 0, sd = sqrt(error_variance))
          
          # DIF for first 4 items
          if (item %in% dif_items) {
            y <- y + dif_shift * group
          }
          
          sim_data[[item]] <- ordered(cut(y, cuts))
        }
        
        ########################################################
        # Multi-item raw score
        ########################################################
        sim_data$raw <- rowMeans(sapply(sim_data[items], as.numeric))
        results$raw[r] <- mean(sim_data$raw[group == 1]) -
          mean(sim_data$raw[group == 0])
        
        ########################################################
        # CFA model
        ########################################################
        model <- paste0("F =~ ", paste(items, collapse = " + "))
        
        ########################################################
        # Anchor-based MG-CFA (correct)
        ########################################################
        fit_anchor <- try(
          cfa(
            model,
            data = sim_data,
            group = "group",
            ordered = items,
            estimator = "WLSMV",
            meanstructure = TRUE,
            
            group.equal = c("loadings", "thresholds"),
            group.partial = paste0(
              dif_items, "|t", rep(1:(length(cuts) - 1),
                                   each = length(dif_items))
            )
          ),
          silent = TRUE
        )
        
        if (!inherits(fit_anchor, "try-error") &&
            inspect(fit_anchor, "converged")) {
          
          pe <- parameterEstimates(fit_anchor)
          lv <- subset(pe, lhs == "F" & op == "~1")
          lv <- lv[order(lv$group), ]
          
          if (nrow(lv) == 2) {
            results$anchor_cfa[r] <- lv$est[2] - lv$est[1]
          }
        }
        
        ########################################################
        # No-anchor MG-CFA
        ########################################################
        fit_naive <- try(
          cfa(
            model,
            data = sim_data,
            group = "group",
            ordered = items,
            estimator = "WLSMV",
            meanstructure = TRUE,
            
            group.equal = c("loadings", "thresholds")
          ),
          silent = TRUE
        )
        
        if (!inherits(fit_naive, "try-error") &&
            inspect(fit_naive, "converged")) {
          
          pe <- parameterEstimates(fit_naive)
          lv <- subset(pe, lhs == "F" & op == "~1")
          lv <- lv[order(lv$group), ]
          
          if (nrow(lv) == 2) {
            results$naive_cfa[r] <- lv$est[2] - lv$est[1]
          }
        }
      }
      
      results_all[[paste0(cat_name,
                          "_EV_", error_variance,
                          "_DIF_", dif_shift)]] <- results
    }
  }
}

############################################################
# 3. Build plotting dataset
############################################################
plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
  
  parts <- strsplit(nm, "_")[[1]]
  
  cat_type <- parts[1]
  ev <- as.numeric(parts[3])
  dif <- as.numeric(parts[5])
  
  res <- results_all[[nm]]
  
  data.frame(
    Category = cat_type,
    ErrorVariance = ev,
    DIF = dif,
    Method = rep(c("Multi-item", "Anchor-Based", "No Anchors"),
                 each = nrow(res)),
    Estimate = c(res$raw, res$anchor_cfa, res$naive_cfa)
  )
}))

plot_data <- plot_data[!is.na(plot_data$Estimate), ]

############################################################
# Factor labels (YOUR REQUEST)
############################################################
plot_data$Category <- factor(
  plot_data$Category,
  levels = c("3cat", "5cat", "7cat"),
  labels = c("3 Cat", "5 Cat", "7 Cat")
)

plot_data$Method <- factor(
  plot_data$Method,
  levels = c("Multi-item", "Anchor-Based", "No Anchors")
)

plot_summary <- aggregate(
  Estimate ~ Category + ErrorVariance + DIF + Method,
  data = plot_data,
  FUN = mean
)

############################################################
# 4. Plot
############################################################
############################################################
# Relabel Error Variance (compact labels)
############################################################
plot_summary$ErrorVariance <- factor(
  plot_summary$ErrorVariance,
  levels = c(0.01, 0.05, 0.10, 0.25),
  labels = c(
    "error var = 0.01",
    "error var = 0.05",
    "error var = 0.10",
    "error var = 0.25"
  )
)

############################################################
# Plot
############################################################
ggplot(plot_summary,
       aes(x = DIF, y = Estimate,
           color = Method, group = Method)) +
  
  geom_line(linewidth = 1.1) +
  
  geom_point(aes(shape = Method),
             size = 3,
             stroke = 1.1,
             color = "black") +
  
  scale_shape_manual(values = c(
    "Multi-item" = 2,    # open triangle
    "Anchor-Based" = 16, # solid circle
    "No Anchors" = 17    # solid triangle
  )) +
  
  geom_hline(yintercept = true_delta,
             linetype = "dashed",
             color = "black") +
  
  facet_grid(ErrorVariance ~ Category) +
  
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = ""
  ) +
  
  scale_color_manual(values = c(
    "Multi-item" = "steelblue",
    "Anchor-Based" = "red",
    "No Anchors" = "orange"
  )) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 1)
  )



######## Partial Invariance, Full Invariance, and Multi-Item 


set.seed(2026)

library(lavaan)
library(ggplot2)

############################################################
# 1. Simulation parameters
############################################################
R <- 100
N <- 1000
true_delta <- 2.0
J <- 8

items <- paste0("auth_", 1:J)
dif_items <- paste0("auth_", 1:4)

############################################################
# Category structures
############################################################
cuts_list <- list(
  "3cat" = c(-Inf, -1, 1, Inf),
  "5cat" = c(-Inf, -1, 0, 1, Inf),
  "7cat" = c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
)

############################################################
# Experimental factors
############################################################
dif_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
error_variances <- c(0.01, 0.05, 0.10, 0.25)

lambda <- 1.0

results_all <- list()

############################################################
# 2. Monte Carlo simulation
############################################################
for (cat_name in names(cuts_list)) {
  
  cuts <- cuts_list[[cat_name]]
  
  for (error_variance in error_variances) {
    
    for (dif_shift in dif_values) {
      
      results <- data.frame(
        raw = rep(NA, R),
        anchor_cfa = rep(NA, R),
        full_cfa = rep(NA, R)
      )
      
      for (r in 1:R) {
        
        group <- rep(0:1, each = N/2)
        eta <- rnorm(N, mean = true_delta * group)
        
        sim_data <- data.frame(group = group)
        
        ########################################################
        # Generate ordinal items
        ########################################################
        for (j in 1:J) {
          
          item <- paste0("auth_", j)
          
          y <- lambda * eta +
            rnorm(N, mean = 0, sd = sqrt(error_variance))
          
          # DIF only for first 4 items
          if (item %in% dif_items) {
            y <- y + dif_shift * group
          }
          
          sim_data[[item]] <- ordered(cut(y, cuts))
        }
        
        ########################################################
        # Multi-item scale
        ########################################################
        sim_data$raw <- rowMeans(sapply(sim_data[items], as.numeric))
        results$raw[r] <- mean(sim_data$raw[group == 1]) -
          mean(sim_data$raw[group == 0])
        
        ########################################################
        # CFA model
        ########################################################
        model <- paste0("F =~ ", paste(items, collapse = " + "))
        
        ########################################################
        # 1. Anchor-based partial invariance CFA (correct)
        ########################################################
        fit_anchor <- try(
          cfa(
            model,
            data = sim_data,
            group = "group",
            ordered = items,
            estimator = "WLSMV",
            meanstructure = TRUE,
            
            group.equal = c("loadings", "thresholds"),
            group.partial = paste0(
              dif_items, "|t",
              rep(1:(length(cuts) - 1),
                  each = length(dif_items))
            )
          ),
          silent = TRUE
        )
        
        if (!inherits(fit_anchor, "try-error") &&
            inspect(fit_anchor, "converged")) {
          
          pe <- parameterEstimates(fit_anchor)
          lv <- subset(pe, lhs == "F" & op == "~1")
          lv <- lv[order(lv$group), ]
          
          if (nrow(lv) == 2) {
            results$anchor_cfa[r] <- lv$est[2] - lv$est[1]
          }
        }
        
        ########################################################
        # 2. Full scalar invariance CFA (no-anchor, misspecified)
        ########################################################
        fit_full <- try(
          cfa(
            model,
            data = sim_data,
            group = "group",
            ordered = items,
            estimator = "WLSMV",
            meanstructure = TRUE,
            
            group.equal = c("loadings", "thresholds")
          ),
          silent = TRUE
        )
        
        if (!inherits(fit_full, "try-error") &&
            inspect(fit_full, "converged")) {
          
          pe <- parameterEstimates(fit_full)
          lv <- subset(pe, lhs == "F" & op == "~1")
          lv <- lv[order(lv$group), ]
          
          if (nrow(lv) == 2) {
            results$full_cfa[r] <- lv$est[2] - lv$est[1]
          }
        }
      }
      
      results_all[[paste0(cat_name,
                          "_EV_", error_variance,
                          "_DIF_", dif_shift)]] <- results
    }
  }
}

############################################################
# 3. Build plotting dataset
############################################################
plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
  
  parts <- strsplit(nm, "_")[[1]]
  
  cat_type <- parts[1]
  ev <- as.numeric(parts[3])
  dif <- as.numeric(parts[5])
  
  res <- results_all[[nm]]
  
  data.frame(
    Category = cat_type,
    ErrorVariance = ev,
    DIF = dif,
    Method = rep(c("Multi-item",
                   "Anchor-Based",
                   "Full Invariance"),
                 each = nrow(res)),
    Estimate = c(res$raw,
                 res$anchor_cfa,
                 res$full_cfa)
  )
}))

plot_data <- plot_data[!is.na(plot_data$Estimate), ]

############################################################
# Labels
############################################################
plot_data$Category <- factor(
  plot_data$Category,
  levels = c("3cat", "5cat", "7cat"),
  labels = c("3 Cat", "5 Cat", "7 Cat")
)

plot_data$ErrorVariance <- factor(
  plot_data$ErrorVariance,
  levels = c(0.01, 0.05, 0.10, 0.25),
  labels = c(
    "error var = 0.01",
    "error var = 0.05",
    "error var = 0.10",
    "error var = 0.25"
  )
)

plot_data$Method <- factor(
  plot_data$Method,
  levels = c("Multi-item",
             "Partial Invariance",
             "Full Invariance")
)

############################################################
# 4. Aggregate
############################################################
plot_summary <- aggregate(
  Estimate ~ Category + ErrorVariance + DIF + Method,
  data = plot_data,
  FUN = mean
)

############################################################
# 5. Plot
############################################################
ggplot(plot_summary,
       aes(x = DIF, y = Estimate,
           color = Method, group = Method)) +
  
  geom_line(linewidth = 1.1) +
  
  geom_point(aes(shape = Method),
             size = 3,
             stroke = 1.1,
             color = "black") +
  
  scale_shape_manual(values = c(
    "Multi-item" = 2,
    "Partial Invariance" = 16,
    "Full Invariance" = 17
  )) +
  
  scale_linetype_manual(values = c(
    "Multi-item" = "dotted",
    "Partial Invariance" = "solid",
    "Full Invariance" = "dashed"
  )) +
  geom_hline(yintercept = true_delta,
             linetype = "dashed",
             color = "black") +
  
  facet_grid(ErrorVariance ~ Category) +
  
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = ""
  ) +
  
  scale_color_manual(values = c(
    "Multi-item" = "steelblue",
    "Anchor-Based" = "red",
    "Full Invariance" = "orange"
  )) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 1)
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
cuts <- c(-Inf, -1, -0.5, 0, 0.5, 1,  Inf)
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





####### new plot. 7 categories

sset.seed(2026)

############################################################
# 1. Simulation Parameters
############################################################
library(lavaan)
library(dplyr)
library(ggplot2)


set.seed(2026)

R <- 100                  # Monte Carlo repetitions
N <- 1000                 # Increased sample size to improve convergence
true_delta <- 2.0         # True latent mean difference
J <- 8                     # Total items
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)
cuts <- c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
n_thresh <- length(cuts) - 1
dif_values <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Safer combinations to improve convergence
lambda_dif_values <- c(1.0, 1.0, 1.0, 1.0)
resid_sd_values <- c(0.1, 0.2, 0.3, 0.4)  

param_grid <- expand.grid(lambda_dif = lambda_dif_values,
                          resid_sd = resid_sd_values)

############################################################
# 2. Run Simulations
############################################################
all_results <- list()

for (p in 1:nrow(param_grid)) {
  
  lambda_dif <- param_grid$lambda_dif[p]
  resid_sd <- param_grid$resid_sd[p]
  
  results_all <- list()
  
  for (dif_shift in dif_values) {
    results <- data.frame(raw = rep(NA, R), partial = rep(NA, R))
    
    for (r in 1:R) {
      # Create groups
      group <- rep(0:1, each = N/2)
      Author <- rnorm(N, mean = true_delta * group)
      sim_data <- data.frame(group = group)
      
      # Generate items
      for (j in 1:J) {
        item <- paste0("auth_", j)
        lambda <- ifelse(item %in% anchor_items, 1.0, lambda_dif)
        y <- lambda * Author + rnorm(N, sd = resid_sd)
        
        # Add DIF for first 4 items
        if (item %in% dif_items) y <- y + dif_shift * group
        
        sim_data[[item]] <- ordered(cut(y, cuts))
      }
      
      # ---------------------
      # Raw mean difference
      # ---------------------
      sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
      results$raw[r] <- mean(sim_data$raw[group == 1]) - mean(sim_data$raw[group == 0])
      
      # ---------------------
      # Partial invariance CFA
      # ---------------------
      model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
      
      # Free only first threshold of each DIF item
      free_thresholds <- as.vector(sapply(dif_items, function(x) paste0(x, "|t1")))
      
      fit_partial <- try(
        cfa(model_full,
            data = sim_data,
            group = "group",
            ordered = paste0("auth_", 1:J),
            meanstructure = TRUE,
            estimator = "WLSMV",
            group.equal = c("loadings","thresholds"),
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

# Combine all results
all_results_df <- bind_rows(all_results)

# ---------------------
# Remove missing values
# ---------------------
all_results_df <- all_results_df %>%
  filter(!is.na(Estimate))

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







####### new plot. 5 categories

sset.seed(2026)

############################################################
# 1. Simulation Parameters
############################################################
library(lavaan)
library(dplyr)
library(ggplot2)


set.seed(2026)

R <- 100                  # Monte Carlo repetitions
N <- 1000                 # Increased sample size to improve convergence
true_delta <- 2.0         # True latent mean difference
J <- 8                     # Total items
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)
cuts <- c(-Inf, -1, 0, 1, Inf)
n_thresh <- length(cuts) - 1
dif_values <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Safer combinations to improve convergence
lambda_dif_values <- c(1.0, 1.0, 1.0, 1.0)
resid_sd_values <- c(0.1, 0.2, 0.3, 0.4)  

param_grid <- expand.grid(lambda_dif = lambda_dif_values,
                          resid_sd = resid_sd_values)

############################################################
# 2. Run Simulations
############################################################
all_results <- list()

for (p in 1:nrow(param_grid)) {
  
  lambda_dif <- param_grid$lambda_dif[p]
  resid_sd <- param_grid$resid_sd[p]
  
  results_all <- list()
  
  for (dif_shift in dif_values) {
    results <- data.frame(raw = rep(NA, R), partial = rep(NA, R))
    
    for (r in 1:R) {
      # Create groups
      group <- rep(0:1, each = N/2)
      Author <- rnorm(N, mean = true_delta * group)
      sim_data <- data.frame(group = group)
      
      # Generate items
      for (j in 1:J) {
        item <- paste0("auth_", j)
        lambda <- ifelse(item %in% anchor_items, 1.0, lambda_dif)
        y <- lambda * Author + rnorm(N, sd = resid_sd)
        
        # Add DIF for first 4 items
        if (item %in% dif_items) y <- y + dif_shift * group
        
        sim_data[[item]] <- ordered(cut(y, cuts))
      }
      
      # ---------------------
      # Raw mean difference
      # ---------------------
      sim_data$raw <- rowMeans(sapply(sim_data[dif_items], as.numeric))
      results$raw[r] <- mean(sim_data$raw[group == 1]) - mean(sim_data$raw[group == 0])
      
      # ---------------------
      # Partial invariance CFA
      # ---------------------
      model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
      
      # Free only first threshold of each DIF item
      free_thresholds <- as.vector(sapply(dif_items, function(x) paste0(x, "|t1")))
      
      fit_partial <- try(
        cfa(model_full,
            data = sim_data,
            group = "group",
            ordered = paste0("auth_", 1:J),
            meanstructure = TRUE,
            estimator = "WLSMV",
            group.equal = c("loadings","thresholds"),
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

# Combine all results
all_results_df <- bind_rows(all_results)

# ---------------------
# Remove missing values
# ---------------------
all_results_df <- all_results_df %>%
  filter(!is.na(Estimate))

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








####### new plot. 3 categories

############################################################
# 1. Simulation Parameters
############################################################
library(lavaan)
library(dplyr)
library(ggplot2)

set.seed(2026)

R <- 100
N <- 1000
true_delta <- 2.0
J <- 8
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)

# Ordinal cuts (3 categories)
cuts <- c(-Inf, -1, 1, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Parameter values
factor_loading_values <- c(1.0, 1.0, 1.0, 1.0)
error_variance_values <- c(0.01, 0.04, 0.09, 0.16)  # sd = 0.1, 0.2, 0.3, 0.4

param_grid <- expand.grid(factor_loading = factor_loading_values,
                          error_variance = error_variance_values)

############################################################
# 2. Run Simulations
############################################################
all_results <- list()

for (p in 1:nrow(param_grid)) {
  
  factor_loading <- param_grid$factor_loading[p]
  error_variance <- param_grid$error_variance[p]
  
  results_all <- list()
  
  for (dif_shift in dif_values) {
    
    results <- data.frame(raw = rep(NA, R), partial = rep(NA, R))
    
    for (r in 1:R) {
      
      # Create groups
      group <- rep(0:1, each = N/2)
      Author <- rnorm(N, mean = true_delta * group)
      sim_data <- data.frame(group = group)
      
      # Generate items
      for (j in 1:J) {
        item <- paste0("auth_", j)
        lambda <- ifelse(item %in% anchor_items, 1.0, factor_loading)
        
        y <- lambda * Author + rnorm(N, sd = sqrt(error_variance))
        
        # Add DIF
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
      
      free_thresholds <- paste0(dif_items, "|t1")
      
      fit_partial <- try(
        cfa(model_full,
            data = sim_data,
            group = "group",
            ordered = paste0("auth_", 1:J),
            meanstructure = TRUE,
            estimator = "WLSMV",
            group.equal = c("loadings", "thresholds"),
            group.partial = free_thresholds),
        silent = TRUE
      )
      
      if (!inherits(fit_partial, "try-error") && inspect(fit_partial, "converged")) {
        pe <- parameterEstimates(fit_partial)
        lv <- subset(pe, lhs == "Author" & op == "~1")
        lv <- lv[order(lv$group), ]
        if (nrow(lv) == 2) {
          results$partial[r] <- lv$est[2] - lv$est[1]
        }
      }
    }
    
    results_all[[paste0("DIF_", dif_shift)]] <- results
  }
  
  # Combine results
  plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
    dif_level <- as.numeric(sub("DIF_", "", nm))
    res <- results_all[[nm]]
    
    data.frame(
      DIF = dif_level,
      Method = rep(c("Raw", "Partial"), each = nrow(res)),
      Estimate = c(res$raw, res$partial)
    )
  }))
  
  # Add parameter labels
  plot_data$factor_loading <- factor_loading
  plot_data$error_variance <- error_variance
  
  all_results[[p]] <- plot_data
}

# Combine all
all_results_df <- bind_rows(all_results)

# Remove NA
all_results_df <- all_results_df %>%
  filter(!is.na(Estimate))

############################################################
# 3. Summary
############################################################
summary_df <- all_results_df %>%
  group_by(DIF, Method, factor_loading, error_variance) %>%
  summarise(Mean = mean(Estimate, na.rm = TRUE), .groups = "drop")

############################################################
# 4. Plot
############################################################
ggplot(summary_df, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(aes(shape = Method), size = 3, stroke = 1.2, color = "black") +
  scale_shape_manual(values = c(2, 16)) +
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  facet_grid(error_variance ~ factor_loading, labeller = label_both) +
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference",
    title = "Raw vs Partial Invariance CFA Across DIF Levels"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

















####### Testing plot for binary




set.seed(2026)

############################################################
# 1. Simulation Parameters
############################################################
R <- 100          # Monte Carlo repetitions
N <- 500         # Sample size per group
true_delta <- 0.5 # True latent mean difference
J <- 8            # Total items
dif_items <- paste0("auth_", 1:4)     # Items with DIF
anchor_items <- paste0("auth_", 5:8)  # Anchor items

# Ordinal cuts
cuts <- c(-Inf, -0.5, 0.5, Inf)
n_thresh <- length(cuts) - 1

dif_values <- c(0.0, 0.1, 0.2, 0.3) 
results_all <- list()
free_thresholds <- NULL 
# Item parameters
lambda_dif <- 1.0
lambda_anchor <- 1.0
resid_sd <- 0.5

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




#############.  3/5/7 Categories 



############################################################
# 1. Simulation Parameters
############################################################
library(lavaan)
library(dplyr)
library(ggplot2)

set.seed(2026)

R <- 100
N <- 1000
true_delta <- 2.0
J <- 8
dif_items <- paste0("auth_", 1:4)
anchor_items <- paste0("auth_", 5:8)

# Different category structures
cuts_list <- list(
  "3 categories" = c(-Inf, -1, 1, Inf),
  "5 categories" = c(-Inf, -1, 0, 1, Inf),
  "7 categories" = c(-Inf, -1, -0.5, 0, 0.5, 1, Inf)
)

dif_values <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Parameters
factor_loading_values <- c(1.0, 1.0, 1.0, 1.0)
error_variance_values <- c(0.01, 0.05, 0.10, 0.25)

param_grid <- expand.grid(factor_loading = factor_loading_values,
                          error_variance = error_variance_values)

############################################################
# 2. Run Simulations
############################################################
all_results <- list()

for (cat_name in names(cuts_list)) {
  
  cuts <- cuts_list[[cat_name]]
  n_thresh <- length(cuts) - 1
  
  for (p in 1:nrow(param_grid)) {
    
    factor_loading <- param_grid$factor_loading[p]
    error_variance <- param_grid$error_variance[p]
    
    results_all <- list()
    
    for (dif_shift in dif_values) {
      
      results <- data.frame(Multi_item = rep(NA, R),
                            partial = rep(NA, R))
      
      for (r in 1:R) {
        
        # Groups
        group <- rep(0:1, each = N/2)
        Author <- rnorm(N, mean = true_delta * group)
        sim_data <- data.frame(group = group)
        
        # Generate items
        for (j in 1:J) {
          item <- paste0("auth_", j)
          lambda <- ifelse(item %in% anchor_items, 1.0, factor_loading)
          
          y <- lambda * Author + rnorm(N, sd = sqrt(error_variance))
          
          if (item %in% dif_items) y <- y + dif_shift * group
          
          sim_data[[item]] <- ordered(cut(y, cuts))
        }
        
        # ---------------------
        # Multi-item difference
        # ---------------------
        sim_data$multi <- rowMeans(sapply(sim_data[dif_items], as.numeric))
        results$Multi_item[r] <- mean(sim_data$multi[group == 1]) -
          mean(sim_data$multi[group == 0])
        
        # ---------------------
        # Partial invariance CFA
        # ---------------------
        model_full <- paste0("Author =~ ", paste0("auth_", 1:J, collapse = " + "))
        
        free_thresholds <- paste0(dif_items, "|t1")
        
        fit_partial <- try(
          cfa(model_full,
              data = sim_data,
              group = "group",
              ordered = paste0("auth_", 1:J),
              meanstructure = TRUE,
              estimator = "WLSMV",
              group.equal = c("loadings","thresholds"),
              group.partial = free_thresholds),
          silent = TRUE
        )
        
        if (!inherits(fit_partial, "try-error") && inspect(fit_partial, "converged")) {
          pe <- parameterEstimates(fit_partial)
          lv <- subset(pe, lhs == "Author" & op == "~1")
          lv <- lv[order(lv$group), ]
          if (nrow(lv) == 2) {
            results$partial[r] <- lv$est[2] - lv$est[1]
          }
        }
      }
      
      results_all[[paste0("DIF_", dif_shift)]] <- results
    }
    
    # Combine results
    plot_data <- do.call(rbind, lapply(names(results_all), function(nm) {
      dif_level <- as.numeric(sub("DIF_", "", nm))
      res <- results_all[[nm]]
      
      data.frame(
        DIF = dif_level,
        Method = rep(c("Multi-item", "Partial"), each = nrow(res)),
        Estimate = c(res$Multi_item, res$partial)
      )
    }))
    
    plot_data$factor_loading <- factor_loading
    plot_data$error_variance <- error_variance
    plot_data$Categories <- cat_name
    
    all_results[[length(all_results) + 1]] <- plot_data
  }
}

# Combine all
all_results_df <- bind_rows(all_results)

# Remove NA
all_results_df <- all_results_df %>%
  filter(!is.na(Estimate))

############################################################
# 3. Summary
############################################################
summary_df <- all_results_df %>%
  group_by(DIF, Method, factor_loading, error_variance, Categories) %>%
  summarise(Mean = mean(Estimate, na.rm = TRUE), .groups = "drop")

############################################################
# 4. Plot (3 columns = categories)
############################################################
ggplot(summary_df, aes(x = DIF, y = Mean, color = Method, group = Method)) +
  geom_line(size = 1.2) +
  geom_point(aes(shape = Method), size = 3, stroke = 1.2, color = "black") +
  scale_shape_manual(values = c(2, 16)) +
  geom_hline(yintercept = true_delta, linetype = "dashed", color = "black") +
  facet_grid(error_variance ~ Categories,
             labeller = labeller(
               error_variance = function(x) paste0("error_var: ", x),  # simple "error_var" label
               Categories = function(x) x                               # just show 3, 5, 7
             )) +
  labs(
    x = "DIF Magnitude",
    y = "Estimated Group Difference"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "#9A1543")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))


