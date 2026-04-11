
## 2008 ANES
library(readstata13)
anes<-read.dta13('/Users/bangzheng/Dropbox/Racial Measurement Invariance/Authoritarianism/2008 ANES data/anes_2008.dta')
dim(anes)


#####################################

      Testing Anchor Items

####################################

anes08_clean <- anes[!is.na(anes$race2), ]

anchor_model <- 'anchor =~ anchor1 + anchor2 + anchor3 + anchor4'




fit1 <- cfa(anchor_model, data = anes08_clean,
            ordered = c("anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2")



fit2<-cfa(anchor_model, data=anes08_clean, ordered=c("anchor1", "anchor2", "anchor3", "anchor4"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=anes08_clean, ordered=c("anchor1", "anchor2", "anchor3", "anchor4"), group="race2", group.equal = c("thresholds", "loadings"))



lavTestLRT(fit1, fit2, fit3)




## So far, the result shows measurement invariance among these items.




#### new way to do it

model <- '
auth =~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1 + anchor2 + anchor3 + anchor4
'

ordvars <- c("auth_1","auth_2","auth_3","auth_4",
             "anchor1","anchor2", "anchor3", "anchor4")

fit1 <- cfa(model,
            data = anes08_clean,
            ordered = ordvars,
            group = "race2",
            group.partial = c("anchor1","anchor2", "anchor3", "anchor4")) 


fit2<- cfa(model,
           data = anes08_clean,
           ordered = ordvars,
           group="race2", group.equal = c("loadings"),
           group.partial = c("anchor1","anchor2", "anchor3", "anchor4")) 

fit3<- cfa(model,
           data = anes08_clean,
           ordered = ordvars,
           group="race2", group.equal = c("thresholds", "loadings"),
           group.partial = c("anchor2", "anchor3", "anchor3", "anchor4")) 

fit4<- cfa(model,
           data = anes08_clean,
           ordered = ordvars,
           group="race2", group.equal = c("means", "thresholds", "loadings"),
           group.partial = c("anchor1","anchor2", "anchor3", "anchor4"))


lavTestLRT(fit1, fit2, fit3, fit4)

## I used 3 anchor items, the result is really good. 


### This shows the perfect result that I wanted!!!!



###### Report fit3 for factor loadings. 


summary(fit3, fit.measures = TRUE, standardized = TRUE)



library(lavaan)

set.seed(123)

# simulate continuous latent variable
n <- 200
eta <- rnorm(n)

# generate ordinal indicators (3-point Likert)
y1 <- cut(eta + rnorm(n), breaks = c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
y2 <- cut(eta + rnorm(n), breaks = c(-Inf, -0.3, 0.7, Inf), labels = FALSE)
y3 <- cut(eta + rnorm(n), breaks = c(-Inf, -0.6, 0.4, Inf), labels = FALSE)

# combine into data frame
dat <- data.frame(y1, y2, y3)

# add some missing values
dat$y1[sample(1:n, 10)] <- NA
dat$y3[sample(1:n, 8)] <- NA

head(dat)


model <- '
  f1 =~ y1 + y2 + y3
'
fit_cat_fiml <- cfa(model,
                    data = dat,
                    ordered = c("y1", "y2", "y3"),
                    missing="fiml")

summary(fit_cat_fiml, fit.measures = TRUE, standardized = TRUE)





##### Threshold Difference Plot

library(lavaan)
library(dplyr)
library(ggplot2)

model_anes08 <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1 + anchor2 + anchor3 + anchor4
'

child_items <- c("auth_1", "auth_2", "auth_3", "auth_4",
                 "anchor1", "anchor2", "anchor3", "anchor4")

# -------------------------
# FIT MODEL ALLOWING THRESHOLDS TO VARY ACROSS RACE GROUPS
# -------------------------
fit_partial <- cfa(
  model_anes08,
  data = anes08_clean,
  group = "race2",
  ordered = child_items,
  group.equal = c("means")
)

# -------------------------
# EXTRACT THRESHOLDS
# -------------------------
thresholds <- parameterEstimates(fit_partial)

child_thresholds <- thresholds %>%
  dplyr::filter(op == "|", lhs %in% item_vars)

# -------------------------
# RESHAPE TO WIDE FORMAT
# -------------------------
threshold_diff <- reshape(
  child_thresholds,
  idvar = c("lhs", "op", "rhs"),
  timevar = "group",
  direction = "wide"
)

# Rename columns for groups
est_cols <- grep("^est\\.", names(threshold_diff), value = TRUE)
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

# -------------------------
# REORDER ITEMS: Anchors left, Child-Rearing right
# -------------------------
anchor_items <- c("Anchor 1","Anchor 2","Anchor 3","Anchor 4")
child_items  <- c("Item 1","Item 2","Item 3","Item 4")

threshold_diff$label <- factor(
  threshold_diff$label,
  levels = c(anchor_items, child_items)
)

# -------------------------
# PLOT: consistent with previous plots
# -------------------------
ggplot(threshold_diff, aes(x = label, y = diff, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "",
    y = "Threshold Difference (Black - White)",
    fill = "Type",
    title = "2008 ANES"
  ) +
  coord_cartesian(ylim = c(-1.0, 0.45)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


############# Latent Mean 



##### Latent Means for ANES 2008

model_anes08 <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1 + anchor2 + anchor3 + anchor4 + anchor5
'

child_items<-c("auth_1", "auth_2", "auth_3", "auth_4",
               "anchor1", "anchor2", "anchor3", "anchor4", "anchor5")

fit <- cfa(
  model_anes08,
  data = anes08_clean,        # <-- updated dataset
  ordered = child_items,       # keep as is unless items differ in 2008 data
  group = "race2",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c(
    "auth_2|t1",
    "auth_3|t1",
    "auth_4|t1"
  )
)

pt <- parameterEstimates(fit)

# Extract latent means (factor intercepts)
# Assuming your factor is named "auth"
latent_means <- subset(pt, op == "~1" & lhs == "auth")

latent_means






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
  data = anes08_clean,
  group = "race2",
  ordered = ordered_items, 
  estimator = "WLSMV"
)

# Metric
fit_metric <- cfa(
  model_cfg, 
  data = anes08_clean,        
  group = "race2",
  ordered = ordered_items, 
  estimator = "WLSMV",
  group.equal = "loadings"
)

# Scalar
fit_scalar <- cfa(
  model_cfg, 
  data = anes08_clean,      
  group = "race2",
  ordered = ordered_items, 
  estimator = "WLSMV",
  group.equal = c("loadings", "thresholds")
)


##############################################
## 3. Partial Scalar
##############################################

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,      
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

## FIX: must include "se" in the subset
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

eta_hat <- rep(NA, nrow(anes08_clean))

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

anes08_clean$eta_hat <- eta_hat


##############################################
## Step C. Ordered Logit Policy Model
##############################################



######. Illegal Immigrants

library(MASS)

### 1. Ensure outcome is ordered
anes08_clean$illegal_imm <- ordered(
  anes08_clean$illegal_imm, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  illegal_imm ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Compute latent mean difference effect


fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,      
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




######  Raw score. (Illegal Immigration)

library(lavaan)
library(MASS)


model_raw<-'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Compute raw Authoritarianism scores and difference by race
anes08_clean$auth_raw <- rowMeans(anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4","anchor5")

fit_metric <- cfa(model_raw, data = anes08_clean, group = "race2",
                  ordered = ordered_items, group.equal = "loadings", estimator="WLSMV")

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent means and SE by race
mean_white <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)/sum(anes08_clean$race2 == 1) +
    var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)/sum(anes08_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
anes08_clean$illegal_imm <- ordered(anes08_clean$illegal_imm, levels = c(1,2,3))

m_metric <- polr(
  illegal_imm ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96*se_delta_policy
ci_upper <- delta_policy_pref + 1.96*se_delta_policy

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



################ World Market


library(MASS)

### 1. Ensure outcome is ordered
anes08_clean$world_market <- ordered(
  anes08_clean$world_market, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  world_market ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,        # <-- updated
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





############### World Market (Raw)

library(lavaan)
library(MASS)


model_raw<-'Author =~ auth_1 + auth_2 + auth_3 + auth_4'


# Compute raw Authoritarianism scores and difference by race
anes08_clean$auth_raw <- rowMeans(anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4","anchor5")

fit_metric <- cfa(model_raw, data = anes08_clean, group = "race2",
                  ordered = ordered_items, group.equal = "loadings", estimator="WLSMV")

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent means and SE by race
mean_white <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)/sum(anes08_clean$race2 == 1) +
    var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)/sum(anes08_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
anes08_clean$world_market <- ordered(anes08_clean$world_market, levels = c(1,2,3))

m_metric <- polr(
  world_market ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96*se_delta_policy
ci_upper <- delta_policy_pref + 1.96*se_delta_policy

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



########### World Democracy

library(MASS)

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)


### 1. Ensure outcome is ordered
anes08_clean$world_democracy <- ordered(
  anes08_clean$world_democracy, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  world_democracy ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,
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

### 6. Wald CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

### 7. Confidence intervals for delta policy effect
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

### 8. Final summary output
round(c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
),3)



############### World Democracy (Raw) 

library(lavaan)
library(MASS)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Compute raw Authoritarianism scores and difference by race
anes08_clean$auth_raw <- rowMeans(anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c("auth_1","auth_2","auth_3","auth_4",
                   "anchor1","anchor2","anchor3","anchor4","anchor5")

fit_metric <- cfa(
  model_raw, 
  data = anes08_clean, 
  group = "race2",
  ordered = ordered_items, 
  group.equal = "loadings", 
  estimator = "WLSMV"
)

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent means and SE by race
mean_white <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)/sum(anes08_clean$race2 == 1) +
    var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)/sum(anes08_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
anes08_clean$world_democracy <- ordered(
  anes08_clean$world_democracy, 
  levels = c(1,2,3)
)

m_metric <- polr(
  world_democracy ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96*se_delta_policy
ci_upper <- delta_policy_pref + 1.96*se_delta_policy

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



############ Terrorism

library(MASS)

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)

### 1. Ensure outcome is ordered
anes08_clean$terrorism <- ordered(
  anes08_clean$terrorism, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  terrorism ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,
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

### 6. Wald CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

### 7. Confidence intervals for delta policy effect
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

### 8. Final summary output
round(c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
),3)



#############. Terrorism (Raw)

library(lavaan)
library(MASS)

model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

# Compute raw Authoritarianism scores and difference by race
anes08_clean$auth_raw <- rowMeans(anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Fit metric invariance CFA
ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4","anchor5"
)

fit_metric <- cfa(
  model_raw, 
  data = anes08_clean, 
  group = "race2",
  ordered = ordered_items, 
  group.equal = "loadings", 
  estimator = "WLSMV"
)

# Extract factor scores
fs_metric <- lavPredict(fit_metric, type = "lv")
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent means and SE by race
mean_white <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)
delta_eta <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 1], na.rm = TRUE)/sum(anes08_clean$race2 == 1) +
    var(anes08_clean$eta_hat_metric[anes08_clean$race2 == 2], na.rm = TRUE)/sum(anes08_clean$race2 == 2)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

# Ordered logistic regression using metric-invariant scores
anes08_clean$terrorism <- ordered(
  anes08_clean$terrorism, 
  levels = c(1,2,3)
)

m_metric <- polr(
  terrorism ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

# Compute delta policy effect and SE
beta_hat <- coef(m_metric)["eta_hat_metric"]
se_beta  <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

delta_policy_pref <- beta_hat * delta_eta

# SE via delta method
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta_hat^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96*se_delta_policy
ci_upper <- delta_policy_pref + 1.96*se_delta_policy

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






################. Immigrants Take Jobs 


library(MASS)

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)

### 1. Ensure outcome is ordered
anes08_clean$imm_takejob <- ordered(
  anes08_clean$imm_takejob, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  imm_takejob ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,
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

### 6. Wald CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

### 7. Confidence intervals for delta policy effect
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

### 8. Final summary output
round(c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
),3)




###############. Immigrant Take Jobs (Raw)

library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)

fit_metric <- cfa(
  model_raw,
  data        = anes08_clean,
  group       = "race2",
  ordered     = ordered_items,
  group.equal = "loadings",
  estimator   = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
anes08_clean$auth_raw <- rowMeans(
  anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- anes08_clean$race2 == 1
black_idx <- anes08_clean$race2 == 2

mean_white <- mean(anes08_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white

se_delta_eta <- sqrt(
  var(anes08_clean$eta_hat_metric[white_idx], na.rm = TRUE) / sum(white_idx) +
    var(anes08_clean$eta_hat_metric[black_idx], na.rm = TRUE) / sum(black_idx)
)

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
anes08_clean$imm_takejob <- ordered(anes08_clean$imm_takejob, levels = c(1,2,3))

m_metric <- polr(
  imm_takejob ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean, Hess = TRUE
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



##################. Immigrant Level 


library(MASS)

model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)

### 1. Ensure outcome is ordered
anes08_clean$imm_number <- ordered(
  anes08_clean$imm_number, levels = c(1, 2, 3, 4, 5)
)

### 2. Fit ordered logit model
m1 <- polr(
  imm_number ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference
fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,
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

### 6. Wald CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

### 7. Confidence intervals for delta policy effect
ci_lower <- ci_beta[1] * delta_eta
ci_upper <- ci_beta[2] * delta_eta

### 8. Final summary output
round(c(
  Estimate = delta_policy_pref,
  SE       = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper
),3)



######### Immigrant Level (Raw)



library(lavaan)
library(MASS)

# --------------------------------------------------
# 1. CFA Model
# --------------------------------------------------
model_raw <- 'Author =~ auth_1 + auth_2 + auth_3 + auth_4'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)

fit_metric <- cfa(
  model_raw,
  data = anes08_clean,
  group = "race2",
  ordered = ordered_items,
  group.equal = "loadings",
  estimator = "WLSMV"
)

# --------------------------------------------------
# 2. Raw Scale Difference
# --------------------------------------------------
anes08_clean$auth_raw <- rowMeans(
  anes08_clean[, c("auth_1","auth_2","auth_3","auth_4")],
  na.rm = TRUE
)

mean_raw  <- tapply(anes08_clean$auth_raw, anes08_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]
delta_raw
# --------------------------------------------------
# 3. Factor Scores Under Metric Invariance
# --------------------------------------------------
fs_metric <- lavPredict(fit_metric, type = "lv")
ci        <- lavInspect(fit_metric, "case.idx")

anes08_clean$eta_hat_metric <- NA
anes08_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
anes08_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# --------------------------------------------------
# 4. Latent Mean Difference
# --------------------------------------------------
white_idx <- anes08_clean$race2 == 0
black_idx <- anes08_clean$race2 == 1

mean_white <- mean(anes08_clean$eta_hat_metric[white_idx], na.rm = TRUE)
mean_black <- mean(anes08_clean$eta_hat_metric[black_idx], na.rm = TRUE)
delta_eta  <- mean_black - mean_white
delta_eta



ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta,
    "95% CI:", ci_delta_eta, "\n")

# --------------------------------------------------
# 5. Ordered Logistic Regression
# --------------------------------------------------
anes08_clean$imm_number <- ordered(anes08_clean$imm_number, levels = c(1,2,3,4,5))

m_metric <- polr(
  imm_number ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean, Hess = TRUE
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

delta_policy_pref <- beta_hat * delta_eta
delta_policy_pref

se_delta_policy <- abs(delta_eta) * se_beta

se_delta_policy

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




####################.   Histogram  Plot 



library(lavaan)
library(dplyr)
library(ggplot2)

# -------------------------
# 1. Compute factor scores
# -------------------------



model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4 + anchor5
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4","anchor5"
)

### 1. Ensure outcome is ordered
anes08_clean$imm_takejob <- ordered(
  anes08_clean$imm_takejob, levels = c(1, 2, 3)
)

### 2. Fit ordered logit model
m1 <- polr(
  imm_takejob ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = anes08_clean,
  Hess = TRUE
)

summary(m1)

### 3. Extract coefficient and SE for eta_hat
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

### 4. Effect of latent mean difference

fit_partial <- cfa(
  model_cfg, 
  data = anes08_clean,
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



# fs_list is a list of factor score matrices (one per group)
fs_list <- lavPredict(fit_partial, type = "lv")
case_idx <- lavInspect(fit_partial, "case.idx")

# initialize vector for all rows
eta_hat <- rep(NA, nrow(anes08_clean))

# assign factor scores to the original row order
for (i in seq_along(case_idx)) {
  eta_hat[case_idx[[i]]] <- fs_list[[i]][, "Author"]
}

# add to dataset
anes08_clean$eta_hat <- eta_hat

# -------------------------
# 2. Create df_eta for plotting
# -------------------------
df_eta <- ccap_clean %>%
  filter(!is.na(eta_hat) & !is.na(race2)) %>%   # remove missing values
  mutate(
    group = factor(race2, levels = c(0, 1), labels = c("White", "Black")),  # adjust labels as needed
    eta = eta_hat   # create eta column explicitly
  ) %>%
  # keep only eta and group columns
  dplyr::select(eta, group)

# check groups
print(table(df_eta$group))

# -------------------------
# 3. Compute group means for vertical lines
# -------------------------
mean_white <- mean(df_eta$eta[df_eta$group == "White"], na.rm = TRUE)
mean_black <- mean(df_eta$eta[df_eta$group == "Black"], na.rm = TRUE)

# -------------------------
# 4. Plot density
# -------------------------
ggplot(df_eta, aes(x = eta, color = group, fill = group)) +
  geom_density(alpha = 0.25, size = 1.1) +
  
  # Vertical lines mapped to SAME color scale
  geom_vline(aes(xintercept = mean_white, color = "White"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_black, color = "Black"),
             linetype = "dashed", size = 1) +
  
  # X-axis limits and breaks
  scale_x_continuous(limits = c(-1.5, 1.5),
                     breaks = seq(-1.5, 1.5, 0.5)) +
  
  labs(
    title = "2008 CCAP",
    x = "Latent Factor Score",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


####

library(lavaan)
library(MASS)
library(dplyr)
library(ggplot2)

# -------------------------------------------
# 1. MODEL CONFIGURATION
# -------------------------------------------
model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4 + anchor5
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)



model_cfg <- '
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4
'

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4"
)


fit_partial <- cfa(
  model_cfg,
  data = anes08_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1", "auth_3|t1", "auth_4|t1")
)

# -------------------------------------------
# 5. Extract latent means
# -------------------------------------------
pe <- parameterEstimates(fit_partial)
lv_means <- subset(pe, op == "~1" & lhs == "Author")
lv_means

mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

# -------------------------------------------
# 6. Compute factor scores
# -------------------------------------------
fs_list  <- lavPredict(fit_partial, type = "lv")     # list: one element per group
case_idx <- lavInspect(fit_partial, "case.idx")      # indices of rows per group

eta_hat <- rep(NA, nrow(anes08_clean))

# Assign scores in original row order
for (i in seq_along(case_idx)) {
  eta_hat[case_idx[[i]]] <- fs_list[[i]][, "Author"]
}

anes08_clean$eta_hat <- eta_hat

# -------------------------------------------
# 7. Build df_eta for plotting
# -------------------------------------------
df_eta <- anes08_clean %>%
  filter(!is.na(eta_hat) & !is.na(race2)) %>%
  mutate(
    group = factor(race2, levels = c(1, 2), labels = c("White", "Black")),
    eta   = eta_hat
  ) %>%
  dplyr::select(eta, group)

table(df_eta$group)

# -------------------------------------------
# 8. Compute group means
# -------------------------------------------
mean_white <- mean(df_eta$eta[df_eta$group == "White"], na.rm = TRUE)
mean_black <- mean(df_eta$eta[df_eta$group == "Black"], na.rm = TRUE)

# -------------------------------------------
# 9. Plot
# -------------------------------------------
ggplot(df_eta, aes(x = eta, color = group, fill = group)) +
  geom_density(alpha = 0.25, size = 1.1) +
  
  # Vertical lines mapped through same color scale
  geom_vline(aes(xintercept = mean_white, color = "White"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_black, color = "Black"),
             linetype = "dashed", size = 1) +
  
  scale_x_continuous(
    limits = c(-1.5, 1.5),
    breaks = seq(-1.5, 1.5, 0.5)
  ) +
  
  labs(
    title = "2008 ANES",
    x = "Latent Factor Score",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )





#############################

using one step method 

############################

anes08_clean$illegal_imm <- ordered(
  anes08_clean$illegal_imm, levels = c(1, 2, 3)
)

model_anes08 <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
  anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  illegal_imm ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

model_mgcfa <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
  anchor1 + anchor2 + anchor3 + anchor4
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "illegal_imm"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit_partial <- cfa(
  model_mgcfa, 
  data = anes08_clean,      
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

pe <- parameterEstimates(fit_partial)

pe
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

beta_row <- subset(pe, lhs == "illegal_imm" & op == "~")


beta_hat <- pe$est[pe$lhs == "illegal_imm" &
                     pe$rhs == "Author" &
                     pe$op  == "~"]

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



#######. Plot 

library(tidyr)
library(ggplot2)

# --------------------------------------------------
# 1. Create the updated 2008 ANES data frame
# --------------------------------------------------
df <- data.frame(
  item = c(
    "Illegal Immigration",
    "World Market",
    "World Democracy",
    "Terrorism",
    "Immigrant Take Jobs",
    "Immigrant Level"
  ),
  estimate_new = c(0.073, 0.006, 0.030, 0.026, 0.038, 0.088),
  se_new       = c(0.016, 0.015, 0.015, 0.018, 0.017, 0.015),
  estimate_raw = c(-0.008, -0.001, -0.007, -0.006, -0.005, -0.001),
  se_raw       = c(0.013, 0.002, 0.011, 0.010, 0.008, 0.003)
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
    title = "Policy Attitude Differences (2008 ANES)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )



