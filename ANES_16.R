

ANES16<-read_dta("/Users/bangzheng/Dropbox/ANES-16-20-24/ANES/anes_2016.dta")
ANES16<-read_dta("/Users/bv533/Library/CloudStorage/Dropbox/ANES-16-20-24/ANES/anes_2016.dta")

dim(ANES16)


# Keep only White (1) and Black (2)
ANES16_clean <- ANES16 |>
  dplyr::filter(race2 %in% c(1, 2)) |>
  dplyr::mutate(
    race2 = factor(race2,
                   levels = c(1, 2),
                   labels = c("White", "Black"))
  )



ANES16_clean$race2 <- as.factor(ANES16_clean$race2)


cor_mat <- round(
  cor(ANES16_clean[, c("anchor1m", "anchor2m", "anchor3m", "anchor4m")], use = "complete.obs"),
  3
)
cor_mat


#### This is working!!!!!




lavTestLRT(fit1, fit2, fit3)


anchor_model <- 'anchor =~ anchor1m + anchor2m + anchor3m + anchor4m'


fit1 <- cfa(anchor_model, data = ANES16_clean,
            ordered = c("anchor1m", "anchor2", "anchor2m", "anchor3m",  "anchor4m"),
            group = "race2")

fit2<-cfa(anchor_model, data=ANES16_clean, ordered=c("anchor1m","anchor2m", "anchor3m", "anchor4m"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=ANES16_clean, ordered=c("anchor1m","anchor2m", "anchor3m", "anchor4m"), group="race2", group.equal = c("thresholds", "loadings"))


lavTestLRT(fit1, fit2, fit3)


## So far, the result shows measurement invariance among these items.






model_anes16<-'auth=~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1m + anchor2m + anchor3m + anchor4m' 




fit1 <- cfa(model_anes16,
            data =ANES16_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1m", "anchor2m", "anchor3m", "anchor4m"),
            group = "race2")


fit2 <- cfa(
  model_anes16,
  data = ANES16_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor1m", "anchor2m", "anchor3m", "anchor4m"),
  group = "race2",
  group.equal = "loadings",
  group.partial = c("anchor1m", "anchor2m", "anchor3m", "anchor4m")
)



fit3<- cfa(model_anes16,
           data = ANES16_clean,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor1m", "anchor2m", "anchor3m", "anchor4m"),
           group="race2", group.equal = c("thresholds", "loadings"),
           group.partial = c("anchor1m", "anchor2m", "anchor3m", "anchor4m"))

fit4 <- cfa(
  model_anes16,
  data = ANES16_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor1m", "anchor2m", "anchor3m", "anchor4m"),
  group = "race2",
  group.equal = c("means", "loadings", "thresholds"),
  group.partial = c(
    "anchor1m",
    "anchor2m",
    "anchor3m",
    "anchor4m"
  )
)        


lavTestLRT(fit1, fit2, fit3, fit4)



### table for fit3 

summary(fit3, standardized = TRUE)



###### Threshold 


child_items<-c("auth_1", "auth_2", "auth_3", "auth_4",
               "anchor1m", "anchor2m", "anchor3m", "anchor4m")

# Fit model allowing item thresholds to vary
fit_partial <- cfa(model_anes16, data =ANES16_clean, group = "race2", ordered = child_items, group.equal = c("means") )

# Extract thresholds
thresholds <- parameterEstimates(fit_partial, standardized = TRUE)
child_thresholds <- thresholds[thresholds$lhs %in% child_items & thresholds$op == "|", ]
child_thresholds 



############### making the threshold mean plot

# =========================
# 1. MODEL SPECIFICATION
# =========================
model_anes16 <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1m + anchor2m + anchor3m + anchor4m
'

ordered_items <- c(
  "auth_1", "auth_2", "auth_3", "auth_4",
  "anchor1m", "anchor2m", "anchor3m", "anchor4m"
)

# =========================
# 2. FIT CFA (THRESHOLDS FREE ACROSS RACE)
# =========================
fit_partial <- cfa(
  model_anes16,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("means")  # only latent means constrained
)

# =========================
# 3. EXTRACT THRESHOLDS
# =========================
library(dplyr)

thresholds <- parameterEstimates(fit_partial)

item_thresholds <- thresholds %>%
  filter(op == "|", lhs %in% ordered_items)

# =========================
# 4. RESHAPE TO WIDE (WHITE vs BLACK)
# =========================
threshold_diff <- reshape(
  item_thresholds,
  idvar = c("lhs", "op", "rhs"),
  timevar = "group",
  direction = "wide"
)

# Automatically identify group columns
est_cols <- grep("^est\\.", names(threshold_diff), value = TRUE)
if (length(est_cols) != 2) stop("Expected exactly two race groups.")

names(threshold_diff)[names(threshold_diff) == est_cols[1]] <- "est_white"
names(threshold_diff)[names(threshold_diff) == est_cols[2]] <- "est_black"

# =========================
# 5. COMPUTE THRESHOLD DIFFERENCE
# =========================
threshold_diff <- threshold_diff %>%
  mutate(
    diff = est_black - est_white,
    type = ifelse(grepl("anchor", lhs, ignore.case = TRUE),
                  "Anchor", "Child-Rearing")
  )

# =========================
# 6. RENAME ITEMS FOR PLOTTING  (FIXED BUG HERE)
# =========================
rename_items <- c(
  "auth_1"    = "Item 1",
  "auth_2"    = "Item 2",
  "auth_3"    = "Item 3",
  "auth_4"    = "Item 4",
  "anchor1m"  = "Anchor 1",
  "anchor2m"  = "Anchor 2",
  "anchor3m"  = "Anchor 3",
  "anchor4m"  = "Anchor 4"
)

threshold_diff$label <- rename_items[threshold_diff$lhs]

# Drop rows that failed to map (should now be none)
threshold_diff <- threshold_diff %>%
  filter(!is.na(label))

# =========================
# 7. ORDER ITEMS (ANCHORS LEFT, CHILD ITEMS RIGHT)
# =========================
plot_anchor_items <- c("Anchor 1", "Anchor 2", "Anchor 3", "Anchor 4")
plot_child_items  <- c("Item 1", "Item 2", "Item 3", "Item 4")

threshold_diff$label <- factor(
  threshold_diff$label,
  levels = c(plot_anchor_items, plot_child_items)
)

# =========================
# 8. PLOT
# =========================
library(ggplot2)

ggplot(threshold_diff, aes(x = label, y = diff, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "",
    y = "Threshold Difference (Black − White)",
    fill = "Type",
    title = "ANES 2016"
  ) +
  coord_cartesian(ylim = c(-1, 0.5)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
##############





# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)


child_thresholds_filtered <- child_thresholds %>%
  dplyr::filter(op == "|", lhs %in% child_items)

child_thresholds_filtered <- as.data.frame(child_thresholds_filtered)

child_thresholds_filtered <- child_thresholds_filtered %>%
  filter(op == "|", lhs %in% child_items)

colnames(child_thresholds_filtered)

child_thresholds_subset <- child_thresholds_filtered[, c("lhs", "op", "rhs", "group", "est")]

# Reshape without 'block' since it differs by group
threshold_diff <- reshape(
  child_thresholds_subset,
  idvar = c("lhs", "op", "rhs"),  # unique threshold identifier
  timevar = "group",
  direction = "wide"
)

# Compute Black - White difference
threshold_diff$diff <- threshold_diff$est.2 - threshold_diff$est.1

# View results
threshold_diff


#### making a plot

library(ggplot2)
library(dplyr)
library(tidyr)
# Your threshold_diff data
threshold_diff <- data.frame(
  lhs = c("Item 1","Item 2","Item 3","Item 4","anchor 1","anchor 1",
          "anchor 2","anchor 2","anchor 3","anchor 3","anchor 4","anchor 4"),
  op = rep("|", 12),
  rhs = c("t1","t1","t1","t1","t1","t2","t1","t2","t1","t2","t1","t2"),
  est.1 = c(-0.5237,-0.215,-0.164,0.607,-0.018,0.348,-1.496,-1.031,0.276,0.953,-1.924,-1.208),
  est.2 = c(-0.741,-0.803,0.439,0.058,-0.268,0.292,-1.451,-1.179,0.173,0.937,-1.919,-1.179)
)

threshold_diff <- threshold_diff %>% 
  mutate(
    diff = est.2 - est.1,
    type = ifelse(grepl("anchor", lhs, ignore.case = TRUE), "Anchor", "Child-Rearing")
  )

# Plot with gray85 background
ggplot(threshold_diff, aes(x = lhs, y = diff, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "",
    y = "Threshold Difference (Black - White)",
    fill = "Type",
    title = "Threshold Differences (2016 ANES)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


##### Latent Means

fit <- cfa(
  model_anes16,
  data = ANES16_clean,
  ordered = child_items,
  group = "race2",
  meanstructure=TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)



pt <- parameterEstimates(fit)


# Filter latent means (nu parameters)
latent_means <- subset(pt, op == "~1" & grepl("latent", lhs) == FALSE)

# Or if your factor is named "auth" or "authoritarianism", use:
latent_means <- subset(pt, op == "~1" & lhs == "auth")

latent_means


white_mean <- latent_means$est[latent_means$group == 1]
black_mean <- latent_means$est[latent_means$group == 2]

latent_diff <- black_mean - white_mean
latent_diff



latent_table <- data.frame(
  Group = c("White", "Black", "Black - White"),
  Mean  = c(white_mean, black_mean, latent_diff)
)

latent_table





#######. Empirical Illustration 


## Step A.
library(lavaan)

##############################################
## 1. Specify CFA model
##############################################

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)


##############################################
## 2. Fit Configural, Metric, Scalar Models
##############################################

# Configural
fit_cfg <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV"
)

# Metric (equal loadings)
fit_metric <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  group.equal = "loadings"
)

# Scalar (equal loadings + thresholds)
fit_scalar <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  group.equal = c("loadings", "thresholds")
)


##############################################
## 3. Partial Scalar (if needed)
##############################################

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)


##############################################
## 4. Extract Latent Means
##############################################

pe <- parameterEstimates(fit_partial)

lv_means <- subset(pe, op == "~1" & lhs == "Author", select = c(group, est))
lv_means


mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta



##############################################
## 5. Model Comparisons
##############################################

lavTestLRT(fit_metric, fit_scalar)
lavTestLRT(fit_metric, fit_partial)

summary(fit_cfg, fit.measures = TRUE)

anova(fit_cfg, fit_metric, fit_scalar, fit_partial)


##############################################
## 6. Extract Factor Scores (Final Model)
##############################################

# Predict factor scores for each group (returns a list)
fs_list <- lavPredict(fit_partial, type = "lv")

# Combine list into one long vector in original row order
case_idx <- lavInspect(fit_partial, "case.idx")  # row indices per group
eta_hat <- rep(NA, nrow(ANES16_clean))

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

ANES16_clean$eta_hat <- eta_hat


### This step is now successful

####### Step C. 



################# Inequality 


model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)


pe <- parameterEstimates(fit_partial)

ANES16_clean$inequality <- ordered(ANES16_clean$inequality, levels = c(1, 2, 3))

# Fit the ordinal regression model
m1 <- polr(
  inequality ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean,
  Hess = TRUE
)

# Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_lower <- beta_hat - 1.96 * se_beta
ci_upper <- beta_hat + 1.96 * se_beta

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# -----------------------
# Summary
# -----------------------
round(
  c(
    Estimate  = beta_hat,
    SE        = se_beta,
    CI_lower  = ci_lower,
    CI_upper  = ci_upper
  ),
  3
)


##############. Inequality (Raw) 


library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)

fit_metric <- cfa(
  model_raw,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
ANES16_clean$auth_raw <- rowMeans(
  ANES16_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(ANES16_clean$auth_raw, ANES16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

ANES16_clean$eta_hat_metric <- NA
ANES16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ANES16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- ANES16_clean$race2 == 1
black_idx <- ANES16_clean$race2 == 2

mean_white <- mean(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
ANES16_clean$inequality <- ordered(ANES16_clean$inequality, levels = c(1,2,3))

m_metric <- polr(
  inequality ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean, Hess = TRUE
)

beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# --------------------------------------------------
# 6. Policy Effect of Latent Difference
# --------------------------------------------------
delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)



##############.   Immigration 

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pe <- parameterEstimates(fit_partial)

# Make sure immigration is an ordered factor
ANES16_clean$immig <- ordered(ANES16_clean$immig, levels = c(1, 2, 3, 4))

# Fit the ordinal regression model
m1 <- polr(
  immig ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean,
  Hess = TRUE
)

# Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_lower <- beta_hat - 1.96 * se_beta
ci_upper <- beta_hat + 1.96 * se_beta

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# -----------------------
# Summary
# -----------------------
round(
  c(
    Estimate  = beta_hat,
    SE        = se_beta,
    CI_lower  = ci_lower,
    CI_upper  = ci_upper
  ),
  3
)



##### Immigration (RAW)

library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)

fit_metric <- cfa(
  model_raw,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
ANES16_clean$auth_raw <- rowMeans(
  ANES16_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(ANES16_clean$auth_raw, ANES16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

ANES16_clean$eta_hat_metric <- NA
ANES16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ANES16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- ANES16_clean$race2 == 1
black_idx <- ANES16_clean$race2 == 2

mean_white <- mean(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
ANES16_clean$immig <- ordered(ANES16_clean$immig, levels = c(1,2,3,4))

m_metric <- polr(
  immig ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean, Hess = TRUE
)

beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# --------------------------------------------------
# 6. Policy Effect of Latent Difference
# --------------------------------------------------
delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






############## Wall 


model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pe <- parameterEstimates(fit_partial)

# Make sure wall is an ordered factor
ANES16_clean$wall <- ordered(ANES16_clean$wall, levels = c(1, 2, 3))

# Fit the ordinal regression model
m1 <- polr(
  wall ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean,
  Hess = TRUE
)

# Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_lower <- beta_hat - 1.96 * se_beta
ci_upper <- beta_hat + 1.96 * se_beta

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# -----------------------
# Summary
# -----------------------
round(
  c(
    Estimate  = beta_hat,
    SE        = se_beta,
    CI_lower  = ci_lower,
    CI_upper  = ci_upper
  ),
  3
)






#####. Wall (RAW)



library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)

fit_metric <- cfa(
  model_raw,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
ANES16_clean$auth_raw <- rowMeans(
  ANES16_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(ANES16_clean$auth_raw, ANES16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

ANES16_clean$eta_hat_metric <- NA
ANES16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ANES16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- ANES16_clean$race2 == 1
black_idx <- ANES16_clean$race2 == 2

mean_white <- mean(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
ANES16_clean$wall <- ordered(ANES16_clean$wall, levels = c(1,2,3))

m_metric <- polr(
  wall ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean, Hess = TRUE
)

beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# --------------------------------------------------
# 6. Policy Effect of Latent Difference
# --------------------------------------------------
delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






############## Birthcitizen



model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pe <- parameterEstimates(fit_partial)

# Make sure birthcitizen is an ordered factor
ANES16_clean$birthcitizen <- ordered(ANES16_clean$birthcitizen, levels = c(1, 2, 3))

# Fit the ordinal regression model
m1 <- polr(
  birthcitizen ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean,
  Hess = TRUE
)

# Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_lower <- beta_hat - 1.96 * se_beta
ci_upper <- beta_hat + 1.96 * se_beta

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# -----------------------
# Summary
# -----------------------
round(
  c(
    Estimate  = beta_hat,
    SE        = se_beta,
    CI_lower  = ci_lower,
    CI_upper  = ci_upper
  ),
  3
)





######. Birth Citizen (RAW)



library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)

fit_metric <- cfa(
  model_raw,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
ANES16_clean$auth_raw <- rowMeans(
  ANES16_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(ANES16_clean$auth_raw, ANES16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

ANES16_clean$eta_hat_metric <- NA
ANES16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ANES16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- ANES16_clean$race2 == 1
black_idx <- ANES16_clean$race2 == 2

mean_white <- mean(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
ANES16_clean$birthcitizen <- ordered(ANES16_clean$birthcitizen, levels = c(1,2,3))

m_metric <- polr(
  birthcitizen ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean, Hess = TRUE
)

beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# --------------------------------------------------
# 6. Policy Effect of Latent Difference
# --------------------------------------------------
delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






##############.  Welfare




model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1m + anchor2m + anchor3m + anchor4m
'

fit_partial <- cfa(
  model_cfg, data = ANES16_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pe <- parameterEstimates(fit_partial)

# Make sure welfare is an ordered factor
ANES16_clean$welfare <- ordered(ANES16_clean$welfare, levels = c(1, 2, 3))

# Fit the ordinal regression model
m1 <- polr(
  welfare ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean,
  Hess = TRUE
)

# Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_lower <- beta_hat - 1.96 * se_beta
ci_upper <- beta_hat + 1.96 * se_beta

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# -----------------------
# Summary
# -----------------------
round(
  c(
    Estimate  = beta_hat,
    SE        = se_beta,
    CI_lower  = ci_lower,
    CI_upper  = ci_upper
  ),
  3
)








######. Welfare (RAW)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1m","anchor2m","anchor3m","anchor4m"
)

fit_metric <- cfa(
  model_raw,
  data = ANES16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
ANES16_clean$auth_raw <- rowMeans(
  ANES16_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(ANES16_clean$auth_raw, ANES16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

ANES16_clean$eta_hat_metric <- NA
ANES16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ANES16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- ANES16_clean$race2 == 1
black_idx <- ANES16_clean$race2 == 2

mean_white <- mean(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(ANES16_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(ANES16_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
ANES16_clean$welfare <- ordered(ANES16_clean$welfare, levels = c(1,2,3))

m_metric <- polr(
  welfare ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ANES16_clean, Hess = TRUE
)

beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# --------------------------------------------------
# 6. Policy Effect of Latent Difference
# --------------------------------------------------
delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






#### Coefficient plot

library(tidyr)
library(ggplot2)

# --------------------------------------------------
# 1. Create the new data frame with 2016 ANES estimates
# --------------------------------------------------
df <- data.frame(
  item = c(
    "Immigration",
    "Wall",
    "Birth Citizen",
    "Welfare",
    "Inequality"
  ),
  estimate_new = c(-0.953, 1.056, 0.522, 0.782, 0.422),
  se_new       = c(0.135, 0.141, 0.124, 0.133, 0.130),
  estimate_raw = c(0.021, -0.024, -0.011, -0.018, -0.006),
  se_raw       = c(0.023, 0.026, 0.012, 0.020, 0.007)
)

# --------------------------------------------------
# 2. Pivot longer for ggplot
# --------------------------------------------------
df_long <- pivot_longer(
  df, 
  cols = c(estimate_new, estimate_raw), 
  names_to = "method", 
  values_to = "estimate"
)

# --------------------------------------------------
# 3. Add correct SEs
# --------------------------------------------------
df_long$se <- ifelse(df_long$method == "estimate_new", df$se_new, df$se_raw)

# Update method labels
df_long$method <- ifelse(df_long$method == "estimate_new", "New Model", "Raw Model")

# --------------------------------------------------
# 4. Plot
# --------------------------------------------------
ggplot(df_long, aes(x = estimate, y = item, color = method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(
    aes(xmin = estimate - 1.96*se, xmax = estimate + 1.96*se),
    position = position_dodge(width = 0.5), height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient / Policy Difference",
    y = "",
    color = "Method",
    title = "Policy Attitude Differences (2016 ANES)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )




############ measurement error plot


library(lavaan)
library(ggplot2)

# Fit your CFA model
model <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'
fit <- cfa(model, data = ANES16_clean)

# Get standardized solution
std_sol <- standardizedSolution(fit)

# Extract residual variances
resid_var <- std_sol[std_sol$op == "~~" & std_sol$lhs %in% c("auth_1","auth_2","auth_3","auth_4"), ]
resid_var$Item <- resid_var$lhs
resid_var$ResidualVariance <- resid_var$est.std

# Plot residual variances
ggplot(resid_var, aes(x = Item, y = ResidualVariance)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Item-level Measurement Error (Residual Variance)",
    y = "Residual Variance",
    x = "Item"
  ) +
  theme_minimal()




###### Anchor-corrected MG-CFA measurement error

library(lavaan)

# Example: anchor-corrected model
model_cfg <- '
Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1m + anchor2m + anchor3m + anchor4m
'

fit_anchor <- cfa(
  model_cfg,
  data = ANES16_clean,
  group = "race2",
  ordered = c("auth_1","auth_2","auth_3","auth_4",
              "anchor1m","anchor2m","anchor3m","anchor4m"),
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1", "auth_3|t1", "auth_4|t1") # anchors held constant
)


# Standardized solution
std_anchor <- standardizedSolution(fit_anchor)

# Extract residuals (~~) for the substantive items, not the latent factor
resid_anchor <- std_anchor[std_anchor$op == "~~" & std_anchor$lhs %in% c("auth_1","auth_2","auth_3","auth_4"), ]
resid_anchor$Item <- resid_anchor$lhs
resid_anchor$ResidualVariance <- resid_anchor$est.std

resid_anchor


fit_raw <- cfa(model_raw, data = ANES16_clean, group = "race2", ordered = ordered_items, estimator = "WLSMV", group.equal = "loadings")

std_raw <- standardizedSolution(fit_raw)
resid_raw <- std_raw[std_raw$op == "~~" & std_raw$lhs %in% c("auth_1","auth_2","auth_3","auth_4"), ]
resid_raw$Model <- "Raw"
resid_anchor$Model <- "Anchor-corrected"

df_resid <- rbind(
  resid_raw[, c("lhs","est.std","Model")],
  resid_anchor[, c("lhs","est.std","Model")]
)
colnames(df_resid) <- c("Item","ResidualVariance","Model")




library(ggplot2)

ggplot(df_resid, aes(x = Item, y = ResidualVariance, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    title = "Item-level Measurement Error: Raw vs Anchor-Corrected MG-CFA",
    y = "Residual Variance (Measurement Error)",
    x = "Item"
  ) +
  theme_minimal()



library(ggplot2)

df <- data.frame(
  Group = rep(c("White", "Black"), 2),
  Model = c(rep("Raw", 2), rep("Anchor-corrected", 2)),
  LatentMean = c(0, 0.45, 0, 0.40)
)

ggplot(df, aes(x = Group, y = LatentMean, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    y = "Estimated Latent Authoritarianism",
    x = "Racial Group",
    title = "Latent Mean Differences: Raw vs. Anchor-Corrected MG-CFA"
  ) +
  theme_minimal()




################. Histogram of Latent Scores 



# Create data frame with scores and group membership
df_eta <- data.frame(
  eta = eta_hat,
  group = ANES16_clean$race2   # or whatever grouping variable you used
)

# If needed, label the groups (adjust labels to your coding)
df_eta$group <- factor(df_eta$group,
                       levels = c(1, 2),
                       labels = c("White", "Black"))

library(ggplot2)

ggplot(df_eta, aes(x = eta, color = group, fill = group)) +
  geom_density(alpha = 0.25, size = 1.1) +
  labs(
    title = "Distribution of Authoritarianism Factor Scores",
    x = "Factor Score (η̂)",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14)


ggplot(df_eta, aes(x = eta, fill = group)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ group, ncol = 1) +
  labs(
    title = "Density of Factor Scores by Group",
    x = "Factor Score (η̂)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

library(ggplot2)
# Compute group means
mean_white <- mean(df_eta$eta[df_eta$group == "White"], na.rm = TRUE)
mean_black <- mean(df_eta$eta[df_eta$group == "Black"], na.rm = TRUE)

ggplot(df_eta, aes(x = eta, color = group, fill = group)) +
  geom_density(alpha = 0.25, size = 1.1) +
  
  # Vertical lines mapped through the same color scale
  geom_vline(aes(xintercept = mean_white, color = "White"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_black, color = "Black"),
             linetype = "dashed", size = 1) +
  
  # X-axis limits and breaks
  scale_x_continuous(
    limits = c(-1.5, 1.5),
    breaks = seq(-1.5, 1.5, by = 0.5)
  ) +
  
  labs(
    title = "2016 ANES",
    x = "Latent Factor Score",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

