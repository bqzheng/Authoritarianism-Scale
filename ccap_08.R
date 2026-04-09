2008 CCAP Data


library(haven)
library(readstata13)

ccap<-read_dta('/Users/bangzheng/Dropbox/Racial Measurement Invariance/Authoritarianism/CCAP Data/ccap.dta')
dim(ccap)

ccap_clean <- ccap[!is.na(ccap$race2), ]

model <- '
# Latent factor
Author =~ c(l1, l1)*auth_1 + c(l2, l2)*auth_2 + c(l3, l3)*auth_3 + c(l4, l4)*auth_4 + anchor1b + anchor1c + anchor1g + anchor1i

# Intercepts (only constrain for selected items)
auth_1 ~ c(i1, i1)*1
auth_2 ~ c(i2, i2)*1
auth_3 ~ c(i3, i3)*1
auth_4 ~ c(i4, i4)*1
anchor1b ~ 1
anchor1c ~ 1
anchor1g ~ 1
anchor1i ~ 1

'

cor(ccap[, c("anchor1a", "anchor1b", "anchor1d", "anchor1e", "anchor1f", "anchor1g", "anchor1h", "anchor1i", "anchor1j")], use = "pairwise.complete.obs")

cor(ccap[, c("anchor1b", "anchor1c", "anchor1g", "anchor1i", "anchor1d")], use = "pairwise.complete.obs")

ccap_clean <- ccap[!is.na(ccap$race2), ]
ccap_clean <- ccap[!is.na(ccap$race3), ]


anchor_model <- 'anchor =~ anchor_4n + anchor_5n + anchor_7n'


fit1 <- cfa(anchor_model, data = ccap_clean,
            ordered = c( "anchor_1n", "anchor_2n", "anchor_3n", "anchor_4n", "anchor_5n", "anchor_6n", "anchor_7n", "anchor_8n", "anchor_9n", "anchor_10n"),
            group = "race2")

fit2<-cfa(anchor_model, data=ccap_clean, ordered=c( "anchor_1n", "anchor_2n", "anchor_3n", "anchor_4n", "anchor_5n", "anchor_6n", "anchor_7n", "anchor_8n", "anchor_9n", "anchor_10n"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=ccap_clean, ordered=c( "anchor_1n", "anchor_2n", "anchor_3n", "anchor_4n", "anchor_5n", "anchor_6n", "anchor_7n", "anchor_8n", "anchor_9n", "anchor_10n"), group="race2", group.equal = c("thresholds", "loadings"))


lavTestLRT(fit1, fit2, fit3)


## This result shows measurement invariance across these anchor items.


## These 4 anchor items shown measurement invariance

ccap_clean <- ccap[!is.na(ccap$race2), ]


anchor_model <- 'anchor =~ anchor_2n + anchor_6n + anchor_5n + anchor_8n'


fit1 <- cfa(anchor_model, data = ccap_clean,
            ordered = c( "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
            group = "race2")

fit2<-cfa(anchor_model, data=ccap_clean, ordered=c( "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"), group="race2", group.equal = "loadings") 
fit3<-cfa(anchor_model, data=ccap_clean, ordered=c( "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"), group="race2", group.equal = c("thresholds", "loadings"))


lavTestLRT(fit1, fit2, fit3)



### making correlation matrix.

install.packages("xtable")
library(xtable)

# Compute correlation matrix
cor_mat <- round(
  cor(ccap[, c("anchor_2n", "anchor_5n", "anchor_6n", "anchor_8n")], use = "complete.obs"),
  3
)

# Rename rows and columns
rownames(cor_mat) <- colnames(cor_mat) <- c("Anchor 1", "Anchor 2", "Anchor 3", "Anchor 4")

# Print LaTeX table
print(xtable(cor_mat), type = "latex")



round(
  cor(ccap[, c("anchor_2n", "anchor_5n", "anchor_6n", "anchor_8n")], use = "complete.obs"),
  3
)



lavTestLRT(fit1, fit2, fit3)


#####. The model


model_ccap<-'auth=~ auth_1 + auth_2 + auth_3 + auth_4 + anchor_2n + anchor_6n + anchor_5n + anchor_8n ' 




fit1 <- cfa(model_ccap,
            data =ccap_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
            group = "race2")


fit2 <- cfa(
  model_ccap,
  data = ccap_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
  group = "race2",
  group.equal = "loadings",
  group.partial = c("anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n")
)


fit2 <- cfa(
  model_ccap,
  data = ccap_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
  group = "race2",
  group.equal = "loadings",
  group.partial = c(
    "anchor_2n =~ f1",
    "anchor_6n =~ f1",
    "anchor_5n =~ f1",
    "anchor_8n =~ f1"
  )
)

fit3<- cfa(model_ccap,
           data = ccap_clean,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
           group="race2", group.equal = c("thresholds", "loadings"),
           group.partial = c("anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"))

fit4 <- cfa(
  model_ccap,
  data = ccap_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor_2n", "anchor_6n", "anchor_5n", "anchor_8n"),
  group = "race2",
  group.equal = c("means", "loadings", "thresholds"),
  group.partial = c(
    "anchor_2n",
    "anchor_6n",
    "anchor_5n",
    "anchor_8n"
  )
)        


lavTestLRT(fit1, fit2, fit3, fit4)



##### report fit3 

summary(fit3, standardized = TRUE)



#######################################

Variable name:
  - [SCAP719] {single varlabel = " AUTHORITARIANISM - RESPECT/INDEPENDENCE"} Independence or
respect for elders?
  - [SCAP725] {single varlabel = " AUTHORITARIANISM - OBEDIENCE/SELF-RELIANCE"} Obedience or self-
  reliance?
  - [SCAP721] {single varlabel = " AUTHORITARIANISM - MANNERS/CURIOSITY"} Curiosity or good
manners?
  - [SCAP722] {single varlabel = " AUTHORITARIANISM - WELL BEHAVED/CONSIDERATE"} Being
considerate or well behaved?
  - [SCAP723] {single varlabel = " AUTHORITARIANISM - DISCIPLINED/CREATIVE"} Disciplined or creative?
  Question: Although there are a number of qualities that people feel that children should have, every
person thinks that some are more important than others. Listed below are pairs of desirable qualities.
For each pair please mark which one you think is more important for a child to have: Which is more
important for a child to have?
  Variable type: categorical
Order: randomized
Valid codes:
  <1> first choice (e.g. independence)
<5> second choice (e.g. respect for elders)
Missing values:
  <8> Skipped
<9> Not asked


# Recode SCAP719: 1 = Independence, 0 = Respect for elders
ccap$auth_1 <- ifelse(ccap$SCAP719 == 1, 1,
                           ifelse(ccap$SCAP719 == 5, 0, NA))

ccap$auth_2 <- ifelse(ccap$SCAP725 == 1, 1,
                      ifelse(ccap$SCAP725 == 5, 0, NA))

ccap$auth_3 <- ifelse(ccap$SCAP721 == 1, 1,
                      ifelse(ccap$SCAP721 == 5, 0, NA))

ccap$auth_4 <- ifelse(ccap$SCAP722 == 1, 1,
                      ifelse(ccap$SCAP722 == 5, 0, NA))

ccap$auth_5 <- ifelse(ccap$SCAP723 == 1, 1,
                      ifelse(ccap$SCAP723 == 5, 0, NA))


table(ccap$PROFILE55)


ccap$race2 <- NA  # Initialize with NA
ccap$race2[ccap$PROFILE55 == 1] <- "White"
ccap$race2[ccap$PROFILE55 == 2] <- "Black"


ccap$race3 <- NA  # Initialize with NA

ccap$race3[ccap$PROFILE55 == 1] <- "White"
ccap$race3[ccap$PROFILE55 == 2] <- "Black"
ccap$race3[ccap$PROFILE55 == 3] <- "Latino"


race2<-ccap$PROFILE55
white<-subset(ccap,race2==1)
black<-subset(ccap,race2==2)

author<-'auth=~auth_1 + auth_2 + auth_3 + auth_4'
fit1<-cfa(author, data=ccap, ordered=c("auth_1", "auth_2", "auth_3", "auth_4"), group="race2")
fit2 <- cfa(author,data = ccap, ordered = c("auth_1", "auth_2", "auth_3", "auth_4"), group = "race2", group.equal = c("loadings"))

fit3<-cfa(author, data=ccap, ordered=c("auth_1", "auth_2", "auth_3", "auth_4"), group="race2", group.equal = c("thresholds", "loadings"))
fit4<-cfa(author, data=ccap, ordered=c("auth_1", "auth_2", "auth_3", "auth_4"), group="race2", group.equal = c("means", "loadings", "thresholds"))
fit5<-cfa(author, data=ccap, ordered=c("auth_1", "auth_2", "auth_3", "auth_4"), group="race2", group.equal = c("means","thresholds", "loadings"))

fit4<-cfa(author, data=ccap, ordered=c("auth_1", "auth_2", "auth_3", "auth_4"), group="race2", group.equal = c("means", "loadings", "thresholds"))


lavTestLRT(fit1, fit2, fit3, fit4)

lavTestLRT(fit1, fit2, fit5)

lavTestLRT(fit3, fit2)

lavTestLRT(fit3, fit4)







fit1 <- cfa(model,
            data = ccap,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1b", "anchor1c", "anchor1g", "anchor1i", "anchor1d"),
            group = "race2")

fit2<- cfa(model,
           data = ccap,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor1b", "anchor1c", "anchor1g", "anchor1i", "anchor1d"),
           group="race2", group.equal = "loadings")

fit3<- cfa(model,
           data = ccap,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor1b", "anchor1c", "anchor1g", "anchor1i", "anchor1d"),
           group="race2", group.equal = c("thresholds", "loadings"))

fit4<- cfa(model,
           data = ccap,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor1b", "anchor1c", "anchor1g", "anchor1i", "anchor1d"),
           group="race2", group.equal = c("means", "loadings", "thresholds"))           

lavTestLRT(fit1, fit2)

lavTestLRT(fit1, fit2, fit3, fit4)
lavTestLRT(fit3, fit4)







##### Plot

library(lavaan)

# Example model: partial invariance with anchors free (adjust labels for number of groups)
model <- '
Author =~ c(l1,l1)*auth_1 + c(l2,l2)*auth_2 + c(l3,l3)*auth_3 + c(l4,l4)*auth_4
Author =~ anchor1b + anchor1c + anchor1g + anchor1i
# intercepts for constrained items
auth_1 ~ c(i1,i1)*1
auth_2 ~ c(i2,i2)*1
auth_3 ~ c(i3,i3)*1
auth_4 ~ c(i4,i4)*1
# anchors: intercepts free across groups (no c(...))
anchor1b ~ 1
anchor1c ~ 1
anchor1g ~ 1
anchor1i ~ 1
'

fit <- cfa(model,
           data = ccap_clean,
           ordered = c("auth_1","auth_2","auth_3","auth_4",
                       "anchor1b","anchor1c","anchor1g","anchor1i"),
           group = "race2",
           estimator = "WLSMV")   # for ordinal items
summary(fit, fit.measures = TRUE, standardized = TRUE)




library(ggplot2)
df_plot <- data.frame(dataset = c("2008 ANES","2008 CCAP","ANES 2024","2020 CCES", "2016 Nationscape","2020 CMPS"),
                      est = c(.30, .25, .28, .32, .29, 0.22),
                      lo = c(.12, .08, .10, .15, .09, 0.11),
                      hi = c(.48, .42, .46, .49, .49, 0.38))

ggplot(df_plot, aes(y = dataset, x = est)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs( title = "Latent Mean Differences Across Datasets",
    x = "Latent mean difference (Black - White)", y = "", 
       caption = "Estimates from WLSMV MG-CFA with anchor thresholds constrained; 95% CIs") +
  theme_minimal()




####. Threshold Difference


model_ccap<-'auth=~ auth_1 + auth_2 + auth_3 + auth_4 + anchor1 + anchor2 + anchor3 + anchor4' 

fit1 <- cfa(model_ccap,
            data =ccap_clean,
            ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                        "anchor1", "anchor2", "anchor3", "anchor4"),
            group = "race2")


fit2 <- cfa(model_ccap,
            data = ccap_clean,
            ordered = ordered_items,
            group = "race2",
            group.equal = "loadings",
            group.partial = c("anchor1", "anchor2", "anchor3", "anchor4"))

fit3<- cfa(model_ccap,
           data = ccap_clean,
           ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
                       "anchor1", "anchor2", "anchor3", "anchor4"),
           group="race2", group.equal = c("thresholds", "loadings"),
           group.partial = c("anchor1", "anchor2", "anchor3", "anchor4"))

fit4 <- cfa(
  model_ccap,
  data = ccap_clean,
  ordered = c("auth_1", "auth_2", "auth_3", "auth_4",
              "anchor1", "anchor2", "anchor3", "anchor4"),
  group = "race2",
  group.equal = c("means", "loadings", "thresholds"),
  group.partial = c(
    "anchor1",
    "anchor2",
    "anchor3",
    "anchor4"
  )
)        


lavTestLRT(fit1, fit2, fit3, fit4)


###########.  Making the Mean difference plot


library(tidyverse)
##############################################
## MEASUREMENT MODEL
##############################################
model_ccap <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1 + anchor2 + anchor3 + anchor4
'

item_vars <- c(
  "auth_1", "auth_2", "auth_3", "auth_4",
  "anchor1", "anchor2", "anchor3", "anchor4"
)

# -------------------------
# FIT MODEL ALLOWING THRESHOLDS TO VARY ACROSS RACE GROUPS
# -------------------------
fit_partial <- cfa(
  model_ccap,
  data = ccap_clean,
  group = "race2",
  ordered = item_vars,
  group.equal = c( "means")
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

# Identify and rename columns automatically
est_cols <- grep("^est\\.", names(threshold_diff), value = TRUE)
if (length(est_cols) != 2) stop("Expected exactly two groups in 'race2'.")

names(threshold_diff)[names(threshold_diff) == est_cols[1]] <- "est_white"
names(threshold_diff)[names(threshold_diff) == est_cols[2]] <- "est_black"

# -------------------------
# COMPUTE BLACK - WHITE DIFFERENCE
# -------------------------
threshold_diff <- threshold_diff %>%
  dplyr::mutate(
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
threshold_diff <- threshold_diff %>% dplyr::filter(!is.na(label))

# -------------------------
# REORDER ITEMS (anchors left, child-rearing right)
# -------------------------
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
    title = "Threshold Differences (2008 CCAP)"
  ) +
  coord_cartesian(ylim = c(-1.0, 0.5)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#########.  This is a very strict, hold loading, lv variance and means fixed. 


library(lavaan)
library(dplyr)
library(ggplot2)

model_ccap <- '
  auth =~ auth_1 + auth_2 + auth_3 + auth_4 +
          anchor1 + anchor2 + anchor3 + anchor4
'

item_vars <- c(
  "auth_1", "auth_2", "auth_3", "auth_4",
  "anchor1", "anchor2", "anchor3", "anchor4"
)

fit_thresh_dif <- cfa(
  model_ccap,
  data = ccap_clean,
  group = "race2",
  ordered = item_vars,
  estimator = "WLSMV",
  group.equal = c("loadings", "lv.variances", "means")
)



thresholds <- parameterEstimates(fit_thresh_dif)

thresholds_items <- thresholds %>%
  filter(op == "|", lhs %in% item_vars)

threshold_wide <- reshape(
  thresholds_items,
  idvar = c("lhs", "op", "rhs"),
  timevar = "group",
  direction = "wide"
)

# Auto-detect group columns
est_cols <- grep("^est\\.", names(threshold_wide), value = TRUE)
stopifnot(length(est_cols) == 2)

names(threshold_wide)[names(threshold_wide) == est_cols[1]] <- "est_white"
names(threshold_wide)[names(threshold_wide) == est_cols[2]] <- "est_black"

threshold_wide <- threshold_wide %>%
  mutate(
    diff = est_black - est_white,
    type = ifelse(grepl("anchor", lhs), "Anchor", "Child-Rearing")
  )



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

threshold_wide$label <- rename_items[threshold_wide$lhs]
threshold_wide <- threshold_wide %>% filter(!is.na(label))

threshold_wide$label <- factor(
  threshold_wide$label,
  levels = c("Anchor 1","Anchor 2","Anchor 3","Anchor 4",
             "Item 1","Item 2","Item 3","Item 4")
)


ggplot(threshold_diff, aes(x = label, y = diff, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "",
    y = "Threshold Difference (Black - White)",
    fill = "Type",
    title = "Threshold Differences (2008 CCAP)"
  ) +
  coord_cartesian(ylim = c(-1.0, 0.45)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )








#######. Empirical Illustration 


## Step A.
library(lavaan)
library(MASS)

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
  model_cfg, data = ccap_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV"
)

# Metric (equal loadings)
fit_metric <- cfa(
  model_cfg, data = ccap_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  group.equal = "loadings"
)

# Scalar (equal loadings + thresholds)
fit_scalar <- cfa(
  model_cfg, data = ccap_clean, group = "race2",
  ordered = ordered_items, estimator = "WLSMV",
  group.equal = c("loadings", "thresholds")
)


##############################################
## 3. Partial Scalar (if needed)
##############################################

fit_partial <- cfa(
  model_cfg, data = ccap_clean, group = "race2",
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

pe <- parameterEstimates(fit_partial, standardized = TRUE)

lv_means <- subset(pe, op == "~1" & lhs == "Author", select = c(group, est, se))
lv_means


mu_white <- lv_means$est[lv_means$group == 1]
mu_black <- lv_means$est[lv_means$group == 2]

delta_eta <- mu_black - mu_white
delta_eta

se_white <- lv_means$se[lv_means$group == 1]
se_black <- lv_means$se[lv_means$group == 2]

se_delta_eta <- sqrt(se_white^2 + se_black^2)
se_delta_eta

delta_policy <- lv_means$est[2] - lv_means$est[1]
delta_policy

lower_ci <- delta_policy - 1.96 * se_delta_eta
upper_ci <- delta_policy + 1.96 * se_delta_eta

c(lower_ci, upper_ci)

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
eta_hat <- rep(NA, nrow(ccap_clean))

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

ccap_clean$eta_hat <- eta_hat


### This step is now successful

####### Step C. 

library(MASS)


fit_partial <- cfa(
  model_cfg,
  data = ccap_clean,
  group = "race2",
  ordered = ordered_items,
  estimator = "WLSMV",
  meanstructure = TRUE,
  group.equal = c("loadings", "thresholds", "lv.variances"),
  group.partial = c("auth_2|t1",
                    "auth_3|t1",
                    "auth_4|t1")
)



ccap_clean$health <- ifelse(ccap_clean$health %in% c(1, 2, 3),
                            ccap_clean$health,
                            NA)

ccap_clean$health <- factor(ccap_clean$health,
                              levels = c(1, 2, 3),
                              ordered = TRUE)


m1 <- polr(
  health ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE   # ensures standard errors
)

summary(m1)


## 95% CI
beta <- coef(m1)["eta_hat"]
delta_policy_pref <- beta * delta_policy
delta_policy_pref


beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
wald_ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
wald_ci_beta


ci_all <- confint(m1)   # profile likelihood CI for all coefficients
ci_all
ci_delta_pref <- ci_beta*delta_policy
ci_delta_pref


ccap_clean$health<- ordered(ccap_clean$health, levels = c(1, 2, 3))

m1 <- polr(
  health ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

beta_hat <- coef(m1)["eta_hat"]
beta_hat
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]
se_beta 

# Wald 95% CI for beta
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_beta


####### testing code 

# 2. Convert to ordered factor with correct levels
ccap_clean$health <- ordered(
  ccap_clean$health,
  levels = c(1, 2, 3)
)

# 3. Fit ordered logit model
m1 <- polr(
  health ~ eta_hat + pid3 + as.numeric(income) + age + as.numeric(edu) + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)
summary(m1)

### 3. Extract coefficient and SE for eta_hat
fs_list <- lavPredict(fit_partial, type = "lv")

case_idx <- lavInspect(fit_partial, "case.idx")

eta_hat <- rep(NA, nrow(ccap_clean))

eta_hat[case_idx[[1]]] <- fs_list[[1]][, "Author"]
eta_hat[case_idx[[2]]] <- fs_list[[2]][, "Author"]

ccap_clean$eta_hat <- eta_hat


### 4. Compute latent mean difference effect

fit_partial <- cfa(
  model_cfg, 
  data = ccap_clean,
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

se_delta_policy <- sqrt(
  (delta_eta^2) * (se_beta^2) +
    (beta_hat^2) * (se_delta_eta^2)
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






###########testing code end 



################ Health (Raw)



ccap_clean$auth_raw <- rowMeans(ccap_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(ccap_clean$auth_raw, ccap_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]




fit_metric <- cfa(model_cfg, data = ccap_clean, group = "race2",
                  ordered = c("auth_1","auth_2","auth_3","auth_4",
                              "anchor1","anchor2","anchor3","anchor4"),
                  group.equal = "loadings", estimator="WLSMV")

fs <- lavPredict(fit_metric, type = "lv")
fs_mat <- fs[[1]]

# extract the Author factor scores
eta_hat_metric <- fs_mat[, "Author"]



fs_metric <- lavPredict(fit_metric, type = "lv")

# Inspect
str(fs_metric)
# It should be a list: fs_metric[[1]] = group 1, fs_metric[[2]] = group 2

# Get case indices per group
ci <- lavInspect(fit_metric, "case.idx")

# Assign scores group by group
ccap_clean$eta_hat_metric <- NA
ccap_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ccap_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]


ccap_clean$eta_hat_metric


# Assuming race2 = 1 (White), 2 (Black)

mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)
delta_eta <- mean_black - mean_white
delta_eta
# Standard error of latent mean difference
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 2], na.rm = TRUE)/sum(ccap_clean$race2 == 2)
)

# Print latent means and SE
cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "\n")
cat("SE of difference:", se_delta_eta, "\n")
ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta

# Print
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")
library(MASS)

# Make sure your outcome is ordered
ccap_clean$health <- ordered(ccap_clean$health, levels=c(1,2,3))

# Run the ordered logistic regression using metric-invariant scores
m_metric <- polr(
  health ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

# View the summary
summary(m_metric)

beta <- coef(m_metric)["eta_hat_metric"]

delta_policy_pref <- beta*delta_eta
delta_policy_pref



## Try to have 95% CI
# Regression coefficient and its SE
beta <- coef(m_metric)["eta_hat_metric"]
beta
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]
se_beta

# Latent mean difference and its SE
mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2==0], na.rm=TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2==1], na.rm=TRUE)
delta_eta <- mean_black - mean_white
se_delta_eta <- sqrt(var(ccap_clean$eta_hat_metric[ccap_clean$race2==0], na.rm=TRUE)/sum(ccap_clean$race2==0) +
                       var(ccap_clean$eta_hat_metric[ccap_clean$race2==1], na.rm=TRUE)/sum(ccap_clean$race2==1))

# SE of delta_policy
se_delta_policy <- sqrt( (delta_eta^2)*(se_beta^2) + (beta^2)*(se_delta_eta^2) )
se_delta_policy
# 95% CI
ci_lower <- delta_policy_pref - 1.96*se_delta_policy
ci_upper <- delta_policy_pref + 1.96*se_delta_policy

c(lower=ci_lower, upper=ci_upper)





############# Gay 
# make sure gay is ordered

ccap_clean$gay <- factor(
  ccap_clean$gay,
  ordered = TRUE
)

ccap_clean$gay <- ordered(ccap_clean$gay, 
                          levels = c(1, 2, 3))





# Ordered logistic regression
m1 <- polr(
  gay ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE   # ensures standard errors
)

summary(m1)

## 95% CI for beta (eta_hat)
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_beta

# Predicted difference in policy preference:
# uses your precomputed delta_policy
delta_policy_pref <- beta_hat * delta_policy
delta_policy_pref

se_delta_policy_pref <- abs(delta_policy) * se_beta
se_delta_policy_pref

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# CI for delta_policy_pref
ci_delta_pref <- ci_beta * delta_policy
ci_delta_pref




############. Gay (Raw)


ccap_clean$auth_raw <- rowMeans(ccap_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(ccap_clean$auth_raw, ccap_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]


# Metric invariance model
fit_metric <- cfa(model_cfg, data = ccap_clean, group = "race2",
                  ordered = c("auth_1","auth_2","auth_3","auth_4",
                              "anchor1","anchor2","anchor3","anchor4"),
                  group.equal = "loadings", estimator="WLSMV")

fs_metric <- lavPredict(fit_metric, type = "lv")

# Inspect (optional)
str(fs_metric)

# Case indices
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
ccap_clean$eta_hat_metric <- NA
ccap_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ccap_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]


# Latent mean difference
mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)
delta_eta <- mean_black - mean_white
delta_eta

# Correct SE of latent mean difference
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "\n")
cat("SE of difference:", se_delta_eta, "\n")

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")


library(MASS)

# Make sure outcome is ordered
ccap_clean$gay <- ordered(ccap_clean$gay, levels=c(1,2,3))

# Ordered logistic regression using metric-invariant scores
m_metric <- polr(
  gay ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

summary(m_metric)

beta <- coef(m_metric)["eta_hat_metric"]
delta_policy_pref <- beta * delta_eta
delta_policy_pref


## 95% CI for delta_policy_pref

# Regression coefficient SE
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Recompute SE of Δη (correct)
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

# SE of delta_policy_pref
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

c(Estimate = delta_policy_pref,
  SE = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper)



##########. Iraq


# Make sure iraq is ordered
ccap_clean$iraq <- ordered(ccap_clean$iraq, 
                           levels = c(1, 2, 3))

# Ordered logistic regression
m1 <- polr(
  iraq ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE   # ensures standard errors
)

summary(m1)

## 95% CI for beta (eta_hat)
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_beta

# Predicted difference in policy preference:
# uses your precomputed delta_policy
delta_policy_pref <- beta_hat * delta_policy
delta_policy_pref

se_delta_policy_pref <- abs(delta_policy) * se_beta
se_delta_policy_pref

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# CI for delta_policy_pref
ci_delta_pref <- ci_beta * delta_policy
ci_delta_pref

c(Estimate = delta_policy_pref,
  SE = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper)



########. Iraq (Raw)

ccap_clean$auth_raw <- rowMeans(ccap_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(ccap_clean$auth_raw, ccap_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Metric invariance model
fit_metric <- cfa(model_cfg, data = ccap_clean, group = "race2",
                  ordered = c("auth_1","auth_2","auth_3","auth_4",
                              "anchor1","anchor2","anchor3","anchor4"),
                  group.equal = "loadings", estimator="WLSMV")

fs_metric <- lavPredict(fit_metric, type = "lv")

# Inspect (optional)
str(fs_metric)

# Case indices
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
ccap_clean$eta_hat_metric <- NA
ccap_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ccap_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean difference
mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)
delta_eta <- mean_black - mean_white
delta_eta

# Correct SE of latent mean difference
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "\n")
cat("SE of difference:", se_delta_eta, "\n")

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

library(MASS)

# Make sure outcome is ordered
ccap_clean$iraq <- ordered(ccap_clean$iraq, levels=c(1,2,3))

# Ordered logistic regression using metric-invariant scores
m_metric <- polr(
  iraq ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

summary(m_metric)

beta <- coef(m_metric)["eta_hat_metric"]
delta_policy_pref <- beta * delta_eta
delta_policy_pref

## 95% CI for delta_policy_pref

# Regression coefficient SE
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Recompute SE of Δη (correct)
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

# SE of delta_policy_pref
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

c(Estimate = delta_policy_pref,
  SE = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper)



############### Tax 

# Make sure tax is ordered
ccap_clean$tax <- ordered(ccap_clean$tax, 
                          levels = c(1, 2, 3))

# Ordered logistic regression
m1 <- polr(
  tax ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE   # ensures standard errors
)

summary(m1)

## 95% CI for beta (eta_hat)
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_beta

# Predicted difference in policy preference:
# uses your precomputed delta_policy
delta_policy_pref <- beta_hat * delta_policy
delta_policy_pref

se_delta_policy_pref <- abs(delta_policy) * se_beta
se_delta_policy_pref

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# CI for delta_policy_pref
ci_delta_pref <- ci_beta * delta_policy
ci_delta_pref

c(Estimate = delta_policy_pref,
  SE = se_delta_policy_pref,
  CI_lower = ci_delta_pref[1],
  CI_upper = ci_delta_pref[2])


############. Tax (Raw)

ccap_clean$auth_raw <- rowMeans(ccap_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(ccap_clean$auth_raw, ccap_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Metric invariance model
fit_metric <- cfa(model_cfg, data = ccap_clean, group = "race2",
                  ordered = c("auth_1","auth_2","auth_3","auth_4",
                              "anchor1","anchor2","anchor3","anchor4"),
                  group.equal = "loadings", estimator="WLSMV")

fs_metric <- lavPredict(fit_metric, type = "lv")

# Inspect (optional)
str(fs_metric)

# Case indices
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
ccap_clean$eta_hat_metric <- NA
ccap_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ccap_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean difference
mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# Correct SE of latent mean difference
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "\n")
cat("SE of difference:", se_delta_eta, "\n")

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

library(MASS)

# Make sure outcome is ordered
ccap_clean$tax <- ordered(ccap_clean$tax, levels=c(1,2,3))

# Ordered logistic regression using metric-invariant scores
m_metric <- polr(
  tax ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

summary(m_metric)

beta <- coef(m_metric)["eta_hat_metric"]
delta_policy_pref <- beta * delta_eta

# 95% CI for delta_policy_pref
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Recompute SE of Δη
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

# SE of delta_policy_pref
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

c(Estimate = delta_policy_pref,
  SE = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper)



#################. Abortion 


# Make sure abortion is ordered
ccap_clean$abortion <- ordered(ccap_clean$abortion, 
                               levels = c(1, 2, 3))

# Ordered logistic regression
m1 <- polr(
  abortion ~ eta_hat + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE   # ensures standard errors
)

summary(m1)

## 95% CI for beta (eta_hat)
beta_hat <- coef(m1)["eta_hat"]
se_beta  <- sqrt(diag(vcov(m1)))["eta_hat"]

# Wald 95% CI
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta
ci_beta

# Predicted difference in policy preference:
# uses your precomputed delta_policy
delta_policy_pref <- beta_hat * delta_policy
delta_policy_pref

se_delta_policy_pref <- abs(delta_policy) * se_beta
se_delta_policy_pref

# Profile likelihood CI for all coefficients
ci_all <- confint(m1)

# CI for delta_policy_pref
ci_delta_pref <- ci_beta * delta_policy
ci_delta_pref

c(Estimate = delta_policy_pref,
  SE = se_delta_policy_pref,
  CI_lower = ci_delta_pref[1],
  CI_upper = ci_delta_pref[2])



##########. Abortion (Raw)


ccap_clean$auth_raw <- rowMeans(ccap_clean[, c("auth_1","auth_2","auth_3","auth_4")], na.rm = TRUE)
mean_raw <- tapply(ccap_clean$auth_raw, ccap_clean$race2, mean, na.rm = TRUE)
delta_raw <- mean_raw[2] - mean_raw[1]

# Metric invariance model
fit_metric <- cfa(model_cfg, data = ccap_clean, group = "race2",
                  ordered = c("auth_1","auth_2","auth_3","auth_4",
                              "anchor1","anchor2","anchor3","anchor4"),
                  group.equal = "loadings", estimator="WLSMV")

fs_metric <- lavPredict(fit_metric, type = "lv")

# Inspect (optional)
str(fs_metric)

# Case indices
ci <- lavInspect(fit_metric, "case.idx")

# Assign factor scores by group
ccap_clean$eta_hat_metric <- NA
ccap_clean$eta_hat_metric[ci[[1]]] <- fs_metric[[1]][, "Author"]
ccap_clean$eta_hat_metric[ci[[2]]] <- fs_metric[[2]][, "Author"]

# Latent mean difference
mean_white <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)
mean_black <- mean(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)
delta_eta <- mean_black - mean_white

# Correct SE of latent mean difference
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

cat("White mean:", mean_white, "\n")
cat("Black mean:", mean_black, "\n")
cat("Difference (Black - White):", delta_eta, "\n")
cat("SE of difference:", se_delta_eta, "\n")

ci_delta_eta <- delta_eta + c(-1, 1) * 1.96 * se_delta_eta
cat("Difference (Black - White):", delta_eta, "95% CI:", ci_delta_eta, "\n")

library(MASS)

# Make sure outcome is ordered
ccap_clean$abortion <- ordered(ccap_clean$abortion, levels=c(1,2,3))

# Ordered logistic regression using metric-invariant scores
m_metric <- polr(
  abortion ~ eta_hat_metric + pid3 + income + age + edu + female + ownhome,
  data = ccap_clean,
  Hess = TRUE
)

summary(m_metric)

beta <- coef(m_metric)["eta_hat_metric"]
delta_policy_pref <- beta * delta_eta

# 95% CI for delta_policy_pref
se_beta <- sqrt(diag(vcov(m_metric)))["eta_hat_metric"]

# Recompute SE of Δη
se_delta_eta <- sqrt(
  var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 0], na.rm = TRUE)/sum(ccap_clean$race2 == 0) +
    var(ccap_clean$eta_hat_metric[ccap_clean$race2 == 1], na.rm = TRUE)/sum(ccap_clean$race2 == 1)
)

# SE of delta_policy_pref
se_delta_policy <- sqrt((delta_eta^2)*(se_beta^2) + (beta^2)*(se_delta_eta^2))

# 95% CI
ci_lower <- delta_policy_pref - 1.96 * se_delta_policy
ci_upper <- delta_policy_pref + 1.96 * se_delta_policy

c(Estimate = delta_policy_pref,
  SE = se_delta_policy,
  CI_lower = ci_lower,
  CI_upper = ci_upper)


######. Plot 


library(tidyr)
library(ggplot2)

# Create the new data frame
df <- data.frame(
  item = c("Health","Gay","Iraq","Tax","Abortion"),
  estimate_new = c(-0.527, -0.426, -0.138, -0.088, -0.368),
  se_new = c(0.039, 0.021, 0.014, 0.022, 0.019),
  estimate_raw = c(0.013, 0.024, 0.008, 0.005, 0.022),
  se_raw = c(0.008, 0.014, 0.005, 0.003, 0.012)
)

# Pivot longer for ggplot
df_long <- pivot_longer(df, 
                        cols = c(estimate_new, estimate_raw), 
                        names_to = "method", 
                        values_to = "estimate")

# Add correct SEs
df_long$se <- ifelse(df_long$method == "estimate_new", df$se_new, df$se_raw)

# Update method labels
df_long$method <- ifelse(df_long$method == "estimate_new", "New Model", "Raw Model")

# Plot
ggplot(df_long, aes(x = estimate, y = item, color = method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*se, xmax = estimate + 1.96*se),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Coefficient / Policy Difference",
       y = "",
       color = "Method",
       title = "Policy Attitude Differences (2008 CCAP)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray85", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))




###############.  Plot 

library(tidyr)
library(ggplot2)

# --------------------------------------------------
# 1. Create the new data frame
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
  estimate_new = c(0.173, -0.049, 0.196, 0.049, 0.172, 0.128),
  se_new       = c(0.074, 0.072, 0.07, 0.091, 0.08, 0.1),
  estimate_raw = c(-0.008, -0.001, -0.007, -0.006, -0.005, -0.001),
  se_raw       = c(0.013, 0.002, 0.011, 0.01, 0.008, 0.003)
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



############. Histogram 

# ----------------------------------------------
# CCAP Density Plot: Latent Factor Scores by Race
# ----------------------------------------------

library(lavaan)
library(dplyr)
library(ggplot2)

# -------------------------
# 1. Compute factor scores
# -------------------------
# fs_list is a list of factor score matrices (one per group)
fs_list <- lavPredict(fit_partial, type = "lv")
case_idx <- lavInspect(fit_partial, "case.idx")

# initialize vector for all rows
eta_hat <- rep(NA, nrow(ccap_clean))

# assign factor scores to the original row order
for (i in seq_along(case_idx)) {
  eta_hat[case_idx[[i]]] <- fs_list[[i]][, "Author"]
}

# add to dataset
ccap_clean$eta_hat <- eta_hat

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






###########. Try this new model 



#### Health


##############################################
## 0. Packages
##############################################
library(lavaan)

##############################################
## 1. Measurement Model
##############################################

ccap_clean$income <- as.numeric(ccap_clean$income)
ccap_clean$edu    <- as.numeric(ccap_clean$edu)



model_sem <- '
  ############################
  ## Measurement
  ############################
  Author =~ auth_1 + auth_2 + auth_3 + auth_4 +
            anchor1 + anchor2 + anchor3 + anchor4

  ############################
  ## Structural (policy model)
  ############################
  health ~ b*Author + pid3 + income + age + edu + female + ownhome
  
'

##############################################
## 2. Ordered indicators
##############################################

ordered_items <- c(
  "auth_1","auth_2","auth_3","auth_4",
  "anchor1","anchor2","anchor3","anchor4",
  "health"
)

##############################################
## 3. Fit Multi-Group SEM
##############################################

fit <- sem(
  model_sem,
  data = ccap_clean,
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
  lhs == "health" & rhs == "Author" & op == "~"
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





