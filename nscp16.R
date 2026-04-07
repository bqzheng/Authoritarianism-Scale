


####. 2016 Nationscape data

library(readstata13)


nscp16<-read.dta13("/Users/bangzheng/Dropbox/Racial Measurement Invariance/Authoritarianism/Nationscape/Nationscape.dta")
dim(nscp16)


nscp16_clean <- nscp16[!is.na(nscp16$race2), ]


nscp16_clean$anchor1 <- (nscp16_clean$anchor1-1) / (6 - 1)
nscp16_clean$anchor2 <- (nscp16_clean$anchor2 - 1) / (4 - 1)
nscp16_clean$anchor3 <- (nscp16_clean$anchor3 - 1) / (4 - 1)
nscp16_clean$anchor4 <- (nscp16_clean$anchor4 - 1) / (4 - 1)



anchor_model <- 'anchor =~ nonrelig + ft_asian  + ft_jew + ft_altright '


fit1 <- cfa(anchor_model, data = nscp16_clean,
            ordered = c( "nonrelig", "ft_hisp", "ft_asian",  "ft_jew", "ft_altright"),
            group = "race2")

fit2<-cfa(anchor_model, data=nscp16_clean, ordered=c("nonrelig", "ft_hisp", "ft_asian",  "ft_jew", "ft_altright"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=nscp16_clean, ordered=c( "nonrelig", "ft_hisp", "ft_asian",  "ft_jew", "ft_altright"), group="race2", group.equal = c("thresholds", "loadings"))

lavTestLRT(fit1, fit2, fit3)



#############

anchor_model <- 'anchor =~ anchor1 + anchor2  + anchor3 + anchor4'


fit1 <- cfa(anchor_model, data = nscp16_clean,
            ordered = c("anchor1", "anchor2",  "anchor3", "anchor4"),
            group = "race2")

fit2<-cfa(anchor_model, data=nscp16_clean, ordered=c("anchor1", "anchor2",  "anchor3", "anchor4"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=nscp16_clean, ordered=c("anchor1", "anchor2",  "anchor3", "anchor4"), group="race2", group.equal = c("thresholds", "loadings"))

lavTestLRT(fit1, fit2, fit3)

####. Anchor items work successfully


round(cor(
  nscp16_clean[, c("anchor1", "anchor2", "anchor3", "anchor4")],
  use = "pairwise.complete.obs"
), 3)




anchor_model <- 'anchor =~ anchor1 + anchor2  + anchor3 + anchor4'


fit1 <- cfa(anchor_model, data = nscp16_clean,
            ordered = c("anchor1", "anchor2",  "anchor3", "anchor4"),
            group = "race2")

fit2<-cfa(anchor_model, data=nscp16_clean, ordered=c("anchor1", "anchor2",  "anchor3", "anchor4"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=nscp16_clean, ordered=c("anchor1", "anchor2",  "anchor3", "anchor4"), group="race2", group.equal = c("thresholds", "loadings"))

lavTestLRT(fit1, fit2, fit3)

###############.  MG-CFA model


model_nscp16<-'auth=~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1 + anchor2 + anchor3 + anchor4' 

fit1 <- cfa(model_nscp16,
            data = nscp16_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2")

fit2 <- cfa(model_nscp16,
            data = nscp16_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2",
            group.equal = "loadings",
            group.partial = c("anchor1", "anchor2", "anchor3", "anchor4"))

fit3 <- cfa(model_nscp16,
            data = nscp16_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2",
            group.equal = c("thresholds", "loadings"),
            group.partial = c("anchor1", "anchor2", "anchor3", "anchor4"))

fit4 <- cfa(model_nscp16,
            data = nscp16_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2",
            group.equal = c("means", "loadings", "thresholds"),
            group.partial = c("anchor1", "anchor2", "anchor3", "anchor4"))

lavTestLRT(fit1, fit2, fit3, fit4)




####### plot 


model_nscp16 <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1 + anchor2 + anchor3 + anchor4
'

child_items <- c("auth_1", "auth_2", "auth_3", "auth_4",
                 "anchor1", "anchor2", "anchor3", "anchor4")

# -------------------------
# FIT MODEL ALLOWING THRESHOLDS TO VARY ACROSS RACE GROUPS
# -------------------------
fit_partial <- cfa(
  model_nscp16,
  data = nscp16_clean,
  group = "race2",
  ordered = child_items,
  group.equal = c( "loadings", "lv.variances", "means")
)

# -------------------------
# EXTRACT THRESHOLDS
# -------------------------
thresholds <- parameterEstimates(fit_partial)

child_thresholds <- thresholds %>%
  filter(op == "|", lhs %in% child_items) # just plain column names


# -------------------------
# RESHAPE TO WIDE FORMAT
# -------------------------
threshold_diff <- reshape(
  child_thresholds,
  idvar = c("lhs", "op", "rhs"),
  timevar = "group",
  direction = "wide"
)

# Identify and rename columns automatically
est_cols <- grep("^est\\.", names(threshold_diff), value = TRUE)
if (length(est_cols) != 2) stop("Expected exactly two groups in 'race2'.")

names(threshold_diff)[names(threshold_diff) == est_cols[1]] <- "est_white"
names(threshold_diff)[names(threshold_diff) == est_cols[2]] <- "est_black"

# -------------------------
# COMPUTE BLACK - WHITE DIFFERENCE
# -------------------------
threshold_diff <- threshold_diff %>%
  mutate(
    diff = est_black - est_white,
    type = ifelse(grepl("anchor", lhs, ignore.case = TRUE),
                  "Anchor", "Child-Rearing")
  )

# -------------------------
# RENAME ITEMS FOR PLOT
# -------------------------
rename_items <- c(
  "auth_1"  = "Item 1",
  "auth_2"  = "Item 2",
  "auth_3"  = "Item 3",
  "auth_4"  = "Item 4",
  "anchor1" = "Anchor 1",
  "anchor2" = "Anchor 2",
  "anchor3" = "Anchor 3",
  "anchor4" = "Anchor 4"
)

threshold_diff$label <- rename_items[threshold_diff$lhs]

# Remove any rows with NA labels
threshold_diff <- threshold_diff %>% filter(!is.na(label))

# Reorder items: anchors left, child-rearing right
anchor_items <- c("Anchor 1","Anchor 2","Anchor 3","Anchor 4")
child_items  <- c("Item 1","Item 2","Item 3","Item 4")

threshold_diff$label <- factor(
  threshold_diff$label,
  levels = c(anchor_items, child_items)
)

# -------------------------
# PLOT
# -------------------------
ggplot(threshold_diff, aes(x = label, y = diff, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "",
    y = "Threshold Difference (Black - White)",
    fill = "Type",
    title = "Threshold Differences (2016 NSCP)"
  ) +
  coord_cartesian(ylim = c(-1.0, 0.45)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





#################. Empirical Illustration



#######. Empirical Illustration 

## Step A.
library(lavaan)
##############################################
## 1. Specify CFA model
##############################################

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)


##############################################
## 2. Fit Configural, Metric, Scalar Models
##############################################

# Configural
fit_cfg <- cfa(
  model_cfg, 
  data = nscp16_clean,     # <-- updated
  group = "race2",         # <-- updated
  ordered = ordered_items, 
  estimator = "WLSMV"
)

# Metric
fit_metric <- cfa(
  model_cfg, 
  data = nscp16_clean,     # <-- updated
  group = "race2",         # <-- updated
  ordered = ordered_items, 
  estimator = "WLSMV",
  group.equal = "loadings"
)

# Scalar
fit_scalar <- cfa(
  model_cfg, 
  data = nscp16_clean,     # <-- updated
  group = "race2",         # <-- updated
  ordered = ordered_items, 
  estimator = "WLSMV",
  group.equal = c("loadings", "thresholds")
)


##############################################
## 3. Partial Scalar
##############################################

fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,     # <-- updated
  group = "race2",         # <-- updated
  ordered = ordered_items, 
  estimator = "WLSMV",
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

lv_means <- subset(pe, op == "~1" & lhs == "Author")

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

## include SEs
lv_means <- subset(pe, op == "~1" & lhs == "Author", 
                   select = c(group, est, se))

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
## 6. Factor Scores
##############################################

fs_list <- lavPredict(fit_partial, type = "lv")

case_idx <- lavInspect(fit_partial, "case.idx")

eta_hat <- rep(NA, nrow(nscp16_clean))     # <-- updated

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

nscp16_clean$eta_hat <- eta_hat            # <-- updated

##############################################
## Step C. Ordered Logit Policy Model
##############################################



######.  Immigrant contribution

library(MASS)

### 1. Ensure outcome is ordered


# 2. Convert to ordered factor with correct levels
nscp16_clean$immi_ctrb <- ordered(
  nscp16_clean$immi_ctrb,
  levels = c(1, 2, 3)
)

# 3. Fit ordered logit model (correct formula syntax!)
m1 <- polr(
  immi_ctrb ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)

### 3. Extract coefficient and SE for eta_hat
fs_list <- lavPredict(fit_partial, type = "lv")

case_idx <- lavInspect(fit_partial, "case.idx")

eta_hat <- rep(NA, nrow(nscp16_clean))     # <-- updated

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

nscp16_clean$eta_hat <- eta_hat   






### 4. Compute latent mean difference effect


fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,        # <-- updated
  group = "race2",
  ordered = ordered_items, 
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

lv_means <- subset(pe, op == "~1" & lhs == "Author")

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta


delta_policy_pref <- beta_hat * delta_eta
delta_policy_pref
### 5. Delta-method SE for policy effect
se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




######  Immigrant Contribution (RAW)


nscp16_clean$auth_1 <- as.numeric(nscp16_clean$auth_1)
nscp16_clean$auth_2 <- as.numeric(nscp16_clean$auth_2)
nscp16_clean$auth_3 <- as.numeric(nscp16_clean$auth_3)
nscp16_clean$auth_4 <- as.numeric(nscp16_clean$auth_4)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Compute raw Authoritarianism scores and difference by race
nscp16_clean$model_raw <- rowMeans(
  nscp16_clean[, c("auth_1","auth_2","auth_3","auth_4")], 
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$model_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4")

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean differences
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# SE for delta_eta
se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
nscp16_clean$immi_ctrb <- ordered(nscp16_clean$immi_ctrb, levels = c(1,2,3))

m_metric <- polr(
  immi_ctrb ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)





######.  Immigrant Make more difficult

library(MASS)

### 1. Ensure outcome is ordered


# 2. Convert to ordered factor with correct levels
nscp16_clean$immi_harder <- ordered(
  nscp16_clean$immi_harder,
  levels = c(1, 2, 3, 4, 5)
)

# 3. Fit ordered logit model (correct formula syntax!)
m1 <- polr(
  immi_harder ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Compute latent mean difference effect


fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,        # <-- updated
  group = "race2",
  ordered = ordered_items, 
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

lv_means <- subset(pe, op == "~1" & lhs == "Author")

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta


delta_policy_pref <- beta_hat * delta_eta

### 5. Delta-method SE for policy effect
se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






######  Immigration more difficult (RAW)


nscp16_clean$auth_1 <- as.numeric(nscp16_clean$auth_1)
nscp16_clean$auth_2 <- as.numeric(nscp16_clean$auth_2)
nscp16_clean$auth_3 <- as.numeric(nscp16_clean$auth_3)
nscp16_clean$auth_4 <- as.numeric(nscp16_clean$auth_4)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Compute raw Authoritarianism scores and difference by race
nscp16_clean$model_raw <- rowMeans(
  nscp16_clean[, c("auth_1","auth_2","auth_3","auth_4")], 
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$model_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4")

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean differences
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# SE for delta_eta
se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
nscp16_clean$immi_harder <- ordered(nscp16_clean$immi_harder, levels = c(1,2,3, 4, 5))

m_metric <- polr(
  immi_harder ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)



######.  Abortion Legalization

library(MASS)

### 1. Ensure outcome is ordered


# 2. Convert to ordered factor with correct levels
nscp16_clean$abortion <- ordered(
  nscp16_clean$abortion,
  levels = c(1, 2, 3)
)

# 3. Fit ordered logit model (correct formula syntax!)
m1 <- polr(
  abortion ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Compute latent mean difference effect


fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,        # <-- updated
  group = "race2",
  ordered = ordered_items, 
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

lv_means <- subset(pe, op == "~1" & lhs == "Author")

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta


delta_policy_pref <- beta_hat * delta_eta

### 5. Delta-method SE for policy effect
se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

### 6. Wald CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

### 7. Confidence intervals for delta policy effect
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

### 8. Final summary output
c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
)





##############.  Abortion (Raw)
library(MASS)
library(lavaan)

# 1. Ensure auth items are numeric
nscp16_clean$auth_1 <- as.numeric(nscp16_clean$auth_1)
nscp16_clean$auth_2 <- as.numeric(nscp16_clean$auth_2)
nscp16_clean$auth_3 <- as.numeric(nscp16_clean$auth_3)
nscp16_clean$auth_4 <- as.numeric(nscp16_clean$auth_4)

# 2. CFA model for Authoritarianism
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# 3. Compute raw Authoritarianism scores
nscp16_clean$model_raw <- rowMeans(
  nscp16_clean[, c("auth_1","auth_2","auth_3","auth_4")], 
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$model_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# 4. Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4") # correct: no anchor5

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# 5. Extract factor scores by group
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# 6. Latent mean differences
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) / sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) / sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# 7. Ensure outcome is ordered and remove NAs
nscp16_clean$abortion <- ordered(nscp16_clean$abortion, levels = c(1,2,3))
nscp16_clean2 <- subset(nscp16_clean, !is.na(abortion) & !is.na(eta_hat_metric))

# 8. Fit ordered logit model
m_metric <- polr(
  abortion ~ eta_hat_metric + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean2,
  Hess = TRUE
)

# 9. Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

delta_policy_pref <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# 10. Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




######. Death Penalty
library(MASS)
library(lavaan)

# 1. Ensure outcome is ordered
nscp16_clean$deathpt <- ordered(nscp16_clean$deathpt, levels = c(1,2,3))

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,        # <-- updated
  group = "race2",
  ordered = ordered_items, 
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

fs_list <- lavPredict(fit_partial, type = "lv")

case_idx <- lavInspect(fit_partial, "case.idx")

eta_hat <- rep(NA, nrow(nscp16_clean))     # <-- updated

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

nscp16_clean$eta_hat <- eta_hat  


# 2. Fit ordered logit model using the latent score
# Make sure latent score is eta_hat_metric
m1 <- polr(
  deathpt ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)

# 3. Extract coefficient and SE for latent Authoritarianism score
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]


# 5. Extract latent means by group
pe <- parameterEstimates(fit_partial)
lv_means <- subset(pe, op == "~1" & lhs == "Author")

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white

# 6. Compute policy effect using delta method
delta_policy_pref <- beta_hat * delta_eta

# 6. Delta-method SE (including both sources of uncertainty)
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))

se_delta_policy <- abs(delta_eta) * se_beta

# 7. Wald confidence interval for delta policy effect
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# 8. Final summary output
c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
)


##############.  Death Penalty (Raw)
library(MASS)
library(lavaan)

### 1. Convert authoritarianism items to numeric
auth_items <- c("auth_1","auth_2","auth_3","auth_4")
nscp16_clean[auth_items] <- lapply(nscp16_clean[auth_items], as.numeric)

### 2. CFA model (no anchor5)
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

### 3. Raw authoritarianism score
nscp16_clean$model_raw <- rowMeans(
  nscp16_clean[, auth_items],
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$model_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

### 4. Metric Invariance CFA
ordered_items <- c(auth_items, "anchor1","anchor2","anchor3","anchor4")

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

### 5. Factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

### 6. Latent mean difference
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

### 7. SE of latent mean difference
se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

### Print results
cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

### 8. Ordered logit on the latent score
nscp16_clean$deathpt <- ordered(nscp16_clean$deathpt, levels = c(1,2,3))

m_metric <- polr(
  deathpt ~ eta_hat_metric + pid3 + as.numeric(income) +
    age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)


beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

### 10. Policy preference difference
delta_policy_pref <- beta_hat * delta_eta

### SE via delta method
se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)


# 7. Wald confidence interval for delta policy effect
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

# 8. Final summary
c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
)






#####.  Health Reform

library(MASS)
library(lavaan)

### 1. Fix outcome variable first
# Check original categories

table(nscp16_clean2$hreform, useNA = "ifany")


# If needed, convert properly:
nscp16_clean$hreform <- ordered(nscp16_clean$hreform)

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'


ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4")

fit_partial <- cfa(
  model_cfg,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1",
                    "auth_3|t1",
                    "auth_4|t1")
)

fs_list <- lavPredict(fit_partial, type = "lv")

case_idx <- lavInspect(fit_partial, "case.idx")

eta_hat <- rep(NA, nrow(nscp16_clean))     # <-- updated

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

nscp16_clean$eta_hat <- eta_hat 


m1 <- polr(
  hreform ~ eta_hat + pid3 + as.numeric(income) +
    age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)


### 3. Extract coefficient and SE
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]


pe <- parameterEstimates(fit_partial)

lv_means <- subset(pe, op == "~1" & lhs == "Author")

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white


### 5. Delta policy effect
delta_policy_pref <- beta_hat * delta_eta


### 6. SE of delta: correct delta method
# (Assumes delta_eta treated as fixed; simplest version)
se_delta_policy <- abs(delta_eta) * se_beta


### 7. CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




##############.  Healthcare Reform (Raw)



nscp16_clean$auth_1 <- as.numeric(nscp16_clean$auth_1)
nscp16_clean$auth_2 <- as.numeric(nscp16_clean$auth_2)
nscp16_clean$auth_3 <- as.numeric(nscp16_clean$auth_3)
nscp16_clean$auth_4 <- as.numeric(nscp16_clean$auth_4)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Raw authoritarianism (simple average)
nscp16_clean$auth_raw <- rowMeans(
  nscp16_clean[, c("auth_1","auth_2","auth_3","auth_4")], 
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$auth_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4")

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# Factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean difference
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# SE(delta_eta)
se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logit model using *metric* factor scores
nscp16_clean$hreform <- ordered(nscp16_clean$hreform, levels = c(1,2,3))

m_metric <- polr(
  hreform ~ eta_hat_metric + pid3 + as.numeric(income) +
    age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)

# Extract beta
beta_hat <- coef(m_metric)["eta_hat_metric"]

# Extract SE
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Delta policy effect
delta_policy_pref <- beta_hat * delta_eta

# SE via delta method

se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))


# 7. Wald confidence interval for delta policy effect
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# 8. Final summary output
c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
)









################### Global Warming
library(MASS)
library(lavaan)

# 1. Ensure outcome is ordered
nscp16_clean$gwarm <- ordered(nscp16_clean$gwarm, levels = c(1,2,3, 4))


model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'
# 2. Fit ordered logit model using the latent score
# Make sure latent score is eta_hat_metric
m1 <- polr(
  gwarm ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)
summary(m1)

# 3. Extract coefficient and SE for latent Authoritarianism score
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]


fit_partial <- cfa(
  model_cfg, 
  data = nscp16_clean,
  group = "race2",
  ordered = c("auth_1","auth_2","auth_3","auth_4",
              "anchor1","anchor2","anchor3","anchor4", "gwarm"),
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

# 5. Extract latent means by group
pe <- parameterEstimates(fit_partial)
lv_means <- subset(pe, op == "~1" & lhs == "Author")

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white

# 6. Compute policy effect using delta method
delta_policy_pref <- beta_hat * delta_eta
se_delta_policy <- abs(delta_eta) * se_beta

# 7. Wald confidence interval for delta policy effect
se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

# Final summary rounded to 3 decimals
round(
  c(
    Estimate = delta_policy_pref,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






##############.  Global Warming (Raw)



nscp16_clean$auth_1 <- as.numeric(nscp16_clean$auth_1)
nscp16_clean$auth_2 <- as.numeric(nscp16_clean$auth_2)
nscp16_clean$auth_3 <- as.numeric(nscp16_clean$auth_3)
nscp16_clean$auth_4 <- as.numeric(nscp16_clean$auth_4)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Raw authoritarianism (simple average)
nscp16_clean$auth_raw <- rowMeans(
  nscp16_clean[, c("auth_1","auth_2","auth_3","auth_4")], 
  na.rm = TRUE
)

mean_raw <- tapply(nscp16_clean$auth_raw, nscp16_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4")

fit_metric <- cfa(
  model_raw,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# Factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

nscp16_clean$eta_hat_metric <- NA
nscp16_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
nscp16_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean difference
mean_white <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# SE(delta_eta)
se_delta_eta <- sqrt(
  var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 1], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 1) +
    var(nscp16_clean$eta_hat_metric[nscp16_clean$race2 == 2], na.rm = TRUE) /
    sum(nscp16_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1,1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logit model using *metric* factor scores
nscp16_clean$gwarm <- ordered(nscp16_clean$gwarm, levels = c(1,2,3))

m_metric <- polr(
  gwarm ~ eta_hat_metric + pid3 + as.numeric(income) +
    age + as.numeric(edu) + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)

# Extract beta
beta_hat <- coef(m_metric)["eta_hat_metric"]

# Extract SE
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Delta policy effect
delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt(
  (delta_eta^2)*(se_beta^2) +
    (beta_hat^2)*(se_delta_eta^2)
)

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

round(
  c(
    Estimate = delta_policy_pref,
    SE = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




########. Plot 

library(tidyr)
library(ggplot2)

# --------------------------------------------------
# 1. Create the updated data frame
# --------------------------------------------------
df <- data.frame(
  item = c(
    "Immigrant contribution",
    "Immigration more difficult",
    "Abortion",
    "Death Penalty",
    "Healthcare Reform",
    "Global Warming"
  ),
  estimate_new = c(0.600, 0.369, 0.303, 0.393, 0.438, 0.306),
  se_new       = c(0.046, 0.035, 0.038, 0.033, 0.039, 0.034),
  estimate_raw = c(-0.069, -0.043, -0.034, -0.045, -0.051, -0.035),
  se_raw       = c(0.043, 0.027, 0.021, 0.028, 0.032, 0.022)
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
    title = "Policy Attitude Differences (2016 NSCP)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )




###########################################

    Mean Difference Plot

###########################################

library(ggplot2)

# Create data
df <- data.frame(
  Study = c("2016 ANES", "2008 CCAP", "2008 ANES", "2016 NSCP"),
  Estimate = c(0.242, 0.404, 0.220, 0.014),
  SE = c(0.182, 0.109, 0.093, 0.056)
)

# Compute 95% CI
df$CI_lower <- df$Estimate - 1.96 * df$SE
df$CI_upper <- df$Estimate + 1.96 * df$SE

# Factor for ordering
df$Study <- factor(df$Study, levels = rev(df$Study))  # reverse to have top-down

# Plot
ggplot(df, aes(x = Estimate, y = Study)) +
  geom_point(size = 3, color = "#F8766D") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#F8766D") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "Latent Mean Difference (Black - White)",
    y = "",
    title = "Estimated Latent Mean Differences"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )




### New plot

library(tidyverse)

# --------------------------------------------------
# 1. Updated data frame with NEW 2016 NSCP results
# --------------------------------------------------
df <- data.frame(
  item = c(
    "Immigrant contribution",
    "Immigration more difficult",
    "Abortion",
    "Death Penalty",
    "Healthcare Reform",
    "Global Warming"
  ),
  estimate_new = c(0.026, 0.016, 0.013, 0.021, 0.021, 0.017),
  se_new       = c(0.002, 0.002, 0.002, 0.002, 0.002, 0.002),
  estimate_raw = c(-0.069, -0.043, -0.034, -0.045, -0.051, -0.035),
  se_raw       = c(0.043, 0.027, 0.021, 0.028, 0.032, 0.022)
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

# Relabel methods
df_long$method <- ifelse(df_long$method == "estimate_new", "New Model", "Raw Model")

# --------------------------------------------------
# 4. Plot
# --------------------------------------------------
ggplot(df_long, aes(x = estimate, y = item, color = method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(
    aes(xmin = estimate - 1.96 * se,
        xmax = estimate + 1.96 * se),
    position = position_dodge(width = 0.5),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient / Policy Difference",
    y = "",
    color = "Method",
    title = "Policy Attitude Differences (2016 NSCP)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )






#####. Histogram



library(ggplot2)

ggplot(df_eta, aes(x = eta, color = group, fill = group)) +
  geom_density(alpha = 0.25, size = 1.1) +
  
  # Vertical lines matched through same color scale
  geom_vline(aes(xintercept = mean_white, color = "White"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_black +0.08, color = "Black"),
             linetype = "dashed", size = 1) +
  scale_x_continuous(
    limits = c(-1.5, 1.5),
    breaks = seq(-1.5, 1.5, by = 0.5)
  ) +
  
  
  labs(
    title = "2016 Nationscape",
    x = "Latent Factor Score",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )





###########. Try this new model 



#### Immigrant Contribution


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  immi_ctrb ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "immi_ctrb"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "immi_ctrb" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


## se_delta_policy <- abs(delta_policy) * se_beta
## se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)





###########. Immigration Make More Difficult


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  immi_harder ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "immi_ctrb"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "immi_harder" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


## se_delta_policy <- abs(delta_policy) * se_beta
## se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)



###########. Abortion


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  abortion ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "abortion"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "abortion" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


## se_delta_policy <- abs(delta_policy) * se_beta
## se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)





###########. Death Penalty


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  deathpt~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "deathpt"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "deathpt" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


##se_delta_policy <- abs(delta_policy) * se_beta
## se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)






###########. Heathcare Reform


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  hreform~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "hreform"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "hreform" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


##se_delta_policy <- abs(delta_policy) * se_beta
##se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




###########. Global Warming


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  gwarm~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "gwarm"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "gwarm" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


##se_delta_policy <- abs(delta_policy) * se_beta
##se_delta_policy





##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




#############   Testing the Raw. 




##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 

  ############################
  ## Structural (policy model)
  ############################
  immi_ctrb ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "immi_ctrb"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c(
    "loadings"
  ),
  
  # Partial scalar (if needed)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 4. Model Summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 5. Extract Latent Means
##############################################

pe <- parameterEstimates(fit)

lv_means <- subset(
  pe,
  op == "~1" & lhs == "Author",
  select = c(group, est, se)
)

lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

##############################################
## 6. SE of Latent Mean Difference
##############################################

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

##############################################
## 7. Extract Structural Coefficient (β)
##############################################

beta_row <- subset(
  pe,
  lhs == "immi_ctrb" & rhs == "Author" & op == "~"
)

beta_hat <- beta_row$est
se_beta  <- beta_row$se

beta_hat
se_beta

##############################################
## 8. Implied Black–White Policy Difference
##############################################

delta_policy <- beta_hat * delta_eta
delta_policy

##############################################
## 9. Delta-Method SE
##############################################

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)


se_delta_policy <- abs(delta_policy) * se_beta
se_delta_policy

##############################################
## 10. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 11. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)



##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Prepare data
##############################################

# Treat covariates as numeric (modeling choice)
nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)

##############################################
## 2. SEM model specification
##############################################

model_sem <- '
  ############################
  ## Measurement model
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4

  ############################
  ## Structural model
  ############################
  immi_ctrb ~ b*Author + pid3 + income + age + edu + female + ownhome
'

##############################################
## 3. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "immi_ctrb"
)

##############################################
## 4. Fit multi-group SEM (partial scalar invariance)
##############################################

fit <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Scalar invariance for latent mean comparison
  group.equal = c(
    "loadings",
    "thresholds",
    "lv.variances"
  ),
  
  # Partial scalar invariance (free selected thresholds)
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

##############################################
## 5. Model summary
##############################################

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 6. Extract parameter estimates
##############################################

pe <- parameterEstimates(fit)

##############################################
## 7. Latent mean difference (Black – White)
##############################################
# In lavaan, group 1 mean is fixed to 0

lv_mean_black <- pe$est[pe$lhs == "Author" &
                          pe$op  == "~1" &
                          pe$group == 2]

delta_eta <- lv_mean_black

##############################################
## 8. Structural coefficient (β)
##############################################

beta_hat <- pe$est[pe$lhs == "immi_ctrb" &
                     pe$rhs == "Author" &
                     pe$op  == "~"]

##############################################
## 9. Implied policy difference
##############################################

delta_policy <- beta_hat * delta_eta

##############################################
## 10. Delta-method SE using full vcov
##############################################

V <- vcov(fit)

idx_mu <- which(pe$lhs == "Author" &
                  pe$op  == "~1" &
                  pe$group == 2)

idx_b  <- which(pe$lhs == "immi_ctrb" &
                  pe$rhs == "Author" &
                  pe$op  == "~")

var_delta_policy <-
  (delta_eta^2) * V[idx_b, idx_b] +
  (beta_hat^2) * V[idx_mu, idx_mu] +
  2 * beta_hat * delta_eta * V[idx_b, idx_mu]

se_delta_policy <- sqrt(var_delta_policy)

##############################################
## 11. 95% Confidence interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 12. Final results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)




###### testing new SE. 

library(lavaan)

##############################################
## 1. Fit partial scalar model
##############################################

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 + 
  anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1", "anchor2", "anchor3", "anchor4",   "immi_ctrb")

fit_partial <- cfa(
  model_cfg,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1","auth_3|t1","auth_4|t1")
)

##############################################
## 2. Fit structural regression with latent variable
##############################################

model_sem <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4
  immi_ctrb ~ b*Author + pid3 + income + age + edu + female + ownhome
'

fit_struct <- sem(
  model_sem,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1","auth_3|t1","auth_4|t1")
)

##############################################
## 3. Extract latent mean difference and SEs
##############################################

pe <- parameterEstimates(fit_struct)

# Latent means: White (reference) is zero
mu_black <- pe$est[pe$lhs=="Author" & pe$op=="~1" & pe$group==2]
se_mu_black <- pe$se[pe$lhs=="Author" & pe$op=="~1" & pe$group==2]

delta_eta <- mu_black       # since White mean = 0
se_delta_eta <- se_mu_black

##############################################
## 4. Extract regression coefficient and SE
##############################################

beta_hat <- pe$est[pe$lhs=="immi_ctrb" & pe$rhs=="Author" & pe$op=="~"]
se_beta  <- pe$se[pe$lhs=="immi_ctrb" & pe$rhs=="Author" & pe$op=="~"]

##############################################
## 5. Delta-method SE
##############################################

delta_policy <- beta_hat * delta_eta

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
)

##############################################
## 6. 95% Confidence Interval
##############################################

ci_lower <- delta_policy - 1.96 * se_delta_policy
ci_upper <- delta_policy + 1.96 * se_delta_policy

##############################################
## 7. Final Results
##############################################

round(
  c(
    Estimate = delta_policy,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)





############.  Try boothtrap SE 


library(lavaan)

##############################################
## 1. Specify the measurement + structural model
##############################################

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 + 
  anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1", "anchor2", "anchor3", "anchor4",   "immi_ctrb")


model_sem <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1 + anchor2 + anchor3 + anchor4
  immi_ctrb ~ b*Author + pid3 + income + age + edu + female + ownhome
'


##############################################
## 2. Bootstrap function
##############################################
compute_delta_policy <- function(data, indices) {
  d <- data[indices, ]  # bootstrap sample
  
  fit <- sem(
    model_sem,
    data = d,
    group = "race2",
    ordered = ordered_items,
    estimator = "WLSMV",
    meanstructure = TRUE,
    group.equal = c("loadings", "thresholds", "lv.variances"),
    group.partial = c("auth_2|t1","auth_3|t1","auth_4|t1")
  )
  
  pe <- parameterEstimates(fit)
  
  # Latent means: White mean = 0
  mu_black <- pe$est[pe$lhs=="Author" & pe$op=="~1" & pe$group==2]
  delta_eta <- mu_black
  
  # Regression coefficient
  beta_hat <- pe$est[pe$lhs=="immi_ctrb" & pe$rhs=="Author" & pe$op=="~"]
  
  # Implied policy difference
  delta_policy <- beta_hat * delta_eta
  return(delta_policy)
}

##############################################
## 3. Run bootstrap
##############################################
library(boot)

set.seed(123)
boot_results <- boot(
  data = nscp16_clean,
  statistic = compute_delta_policy,
  R = 500   # number of bootstrap samples
)

##############################################
## 4. Compute SE and 95% CI
##############################################
delta_policy_hat <- mean(boot_results$t)
delta_policy_hat
se_delta_policy <- sd(boot_results$t)
se_delta_policy

ci_lower <- quantile(boot_results$t, 0.025)
ci_upper <- quantile(boot_results$t, 0.975)

##############################################
## 5. Results
##############################################
round(
  c(
    Estimate = delta_policy_hat,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)


############. (Healthcare Reform)  boothtrap SE 


library(lavaan)

##############################################
## 1. Specify the measurement + structural model
##############################################
model_sem <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1 + anchor2 + anchor3 + anchor4
  hreform ~ b*Author + pid3 + income + age + edu + female + ownhome
'

ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1", "anchor2", "anchor3", "anchoe4", "hreform")

##############################################
## 2. Bootstrap function
##############################################
compute_delta_policy <- function(data, indices) {
  d <- data[indices, ]  # bootstrap sample
  
  fit <- sem(
    model_sem,
    data = d,
    group = "race2",
    ordered = ordered_items,
    estimator = "WLSMV",
    meanstructure = TRUE,
    group.equal = c("loadings", "thresholds", "lv.variances"),
    group.partial = c("auth_2|t1","auth_3|t1","auth_4|t1")
  )
  
  pe <- parameterEstimates(fit)
  
  # Latent means: White mean = 0
  mu_black <- pe$est[pe$lhs=="Author" & pe$op=="~1" & pe$group==2]
  delta_eta <- mu_black
  
  # Regression coefficient
  beta_hat <- pe$est[pe$lhs=="hreform" & pe$rhs=="Author" & pe$op=="~"]
  
  # Implied policy difference
  delta_policy <- beta_hat * delta_eta
  return(delta_policy)
}

##############################################
## 3. Run bootstrap
##############################################
library(boot)

set.seed(123)
boot_results <- boot(
  data = nscp16_clean,
  statistic = compute_delta_policy,
  R = 500   # number of bootstrap samples
)



##############################################
## 4. Compute SE and 95% CI
##############################################
delta_policy_hat <- mean(boot_results$t)
delta_policy_hat
se_delta_policy <- sd(boot_results$t)
se_delta_policy

ci_lower <- quantile(boot_results$t, 0.025)
ci_upper <- quantile(boot_results$t, 0.975)

##############################################
## 5. Results
##############################################
round(
  c(
    Estimate = delta_policy_hat,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)


########### Factor Score Model with Bootstrap SE

##############################################
## 0. Packages
##############################################
library(lavaan)
library(MASS)   # polr
library(boot)

##############################################
## 1. Data preparation
##############################################

nscp16_clean$income <- as.numeric(nscp16_clean$income)
nscp16_clean$edu    <- as.numeric(nscp16_clean$edu)

##############################################
## 2. Measurement model (CFA only)
##############################################

model_cfa <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4"
)

##############################################
## 3. Fit multi-group CFA
##############################################

fit_cfa <- cfa(
  model_cfa,
  data = nscp16_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  
  # Measurement invariance
  group.equal = c("loadings", "thresholds", "lv.variances"),
  
  # Partial scalar invariance
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

summary(
  fit_cfa,
  standardized = TRUE,
  fit.measures = TRUE
)

##############################################
## 4. Extract factor scores
##############################################

fs <- lavPredict(
  fit_cfa,
  method = "regression"
)

nscp16_clean$Author_fs <- fs[, "Author"]

##############################################
## 5. Factor-score policy model
##############################################

fs_model <- polr(
  hreform ~ Author_fs + pid3 + income + age + edu + female + ownhome,
  data = nscp16_clean,
  Hess = TRUE
)

summary(fs_model)

##############################################
## 6. Implied Black–White policy difference
##############################################

# Group means of factor scores
mu_fs <- tapply(
  nscp16_clean$Author_fs,
  nscp16_clean$race2,
  mean,
  na.rm = TRUE
)

mu_white <- mu_fs["White"]
mu_black <- mu_fs["Black"]

delta_eta_fs <- mu_black - mu_white

# Structural coefficient
beta_fs <- coef(fs_model)["Author_fs"]

# Implied policy difference
delta_policy_fs <- beta_fs * delta_eta_fs
delta_policy_fs

##############################################
## 7. Bootstrap function
##############################################

compute_delta_policy_fs <- function(data, indices) {
  
  d <- data[indices, ]
  
  # CFA
  fit_cfa <- cfa(
    model_cfa,
    data = d,
    group = "race2",
    ordered = ordered_items,
    estimator = "WLSMV",
    meanstructure = TRUE,
    group.equal = c("loadings", "thresholds", "lv.variances"),
    group.partial = c("auth_2|t1","auth_3|t1","auth_4|t1")
  )
  
  # Factor scores
  fs <- lavPredict(fit_cfa, method = "regression")
  d$Author_fs <- fs[, "Author"]
  
  # Policy model
  fs_model <- polr(
    hreform ~ Author_fs + pid3 + income + age + edu + female + ownhome,
    data = d,
    Hess = FALSE
  )
  
  beta_fs <- coef(fs_model)["Author_fs"]
  
  # Group means of factor scores
  mu_fs <- tapply(d$Author_fs, d$race2, mean, na.rm = TRUE)
  
  delta_eta_fs <- mu_fs["Black"] - mu_fs["White"]
  
  beta_fs * delta_eta_fs
}

##############################################
## 8. Run bootstrap
##############################################

set.seed(123)

boot_fs <- boot(
  data = nscp16_clean,
  statistic = compute_delta_policy_fs,
  R = 500
)

##############################################
## 9. Bootstrap SE and 95% CI
##############################################

delta_policy_hat <- mean(boot_fs$t)
se_delta_policy  <- sd(boot_fs$t)

ci_lower <- quantile(boot_fs$t, 0.025)
ci_upper <- quantile(boot_fs$t, 0.975)

##############################################
## 10. Final results
##############################################

round(
  c(
    Estimate = delta_policy_hat,
    SE       = se_delta_policy,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  ),
  3
)





#####. The end 
