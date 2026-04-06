

anes <- read_dta("/Users/bangzheng/Dropbox/Negative Voting/ANES_24/ANES16-24.dta")


dim(anes)
library(ggplot2)

anes$neg_partisan24 <- as.numeric(anes$neg_partisan24)

hist(anes$neg_partisan24, main="Negative Partisanship, 2024", xlab = "" )

hist(anes$anti_trump24, main="Anti-Trump, 2024 (Harris)", xlab="")
hist(anes$anti_trump24b, main="Anti-Trump, 2024 (Biden)", xlab="")

hist(anes$retrospect)

library(ggplot2)

# Create a factor for your x variable with the custom labels
anes$retrospect_label <- factor(
  anes$retrospect,
  levels = 1:5,
  labels = c(
    "1. Much better off",
    "2. Somewhat better off",
    "3. About the same",
    "4. Somewhat worse off",
    "5. Much worse off"
  )
)

library(ggplot2)
library(dplyr)

# Summarize counts and percentages
anes_summary <- anes %>%
  filter(retrospect %in% 1:5) %>%
  group_by(retrospect) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    percent = round(100 * count / sum(count), 2),
    cum_percent = round(cumsum(percent), 2),
    label = case_when(
      retrospect == 1 ~ "Much better",
      retrospect == 2 ~ "Somewhat better",
      retrospect == 3 ~ "About the same",
      retrospect == 4 ~ "Somewhat worse",
      retrospect == 5 ~ "Much worse"
    ))

# Plot with counts and percentages
ggplot(anes_summary, aes(x = factor(retrospect, levels = 1:5), y = count)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_x_discrete(labels = anes_summary$label) +
  labs(
    title = "Perception of Economic Trend, 2024",
    x = "",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +  # increase base font size
  theme(
    panel.background = element_rect(fill = "gray85", color = NA), # only inside plot area
    plot.background = element_rect(fill = "white", color = NA),   # outside remains white
    plot.title = element_text(size = 20, face = "bold"),          # bigger title
    axis.text = element_text(size = 14),                          # bigger axis labels
    axis.title = element_text(size = 16)
  )




# Plot histogram
ggplot(anes, aes(x = neg_partisan24, fill = preference)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red", "Neutral" = "gray")) +
  labs(title = "Histogram of Democrat vs Republican Preference (ANES)",
       x = "Democrat minus Republican liking",
       y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")





ggplot(anes %>% filter(!is.na(pid3)), aes(x = neg_partisan24, fill = factor(pid3))) +
  geom_histogram(binwidth = 2, color = "white") +
  scale_fill_manual(
    values = c("1" = "blue", "2" = "black", "3" = "red"),
    labels = c("1" = "Democrat", "2" = "Independent", "3" = "Republican"),
    name = NULL
  ) +
  labs(
    title = "2024",
    x = "",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background  = element_rect(fill = "white",  color = NA),  # outside panel
    panel.background = element_rect(fill = "gray90", color = NA),  # inside panel only
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18)
  ) 







##### Preference Difference. Harrivs vs Trump
ggplot(anes %>% filter(!is.na(pid3)), aes(x = diff_cand, fill = factor(pid3))) +
  geom_histogram(binwidth = 2, color = "white") +
  scale_fill_manual(
    values = c("1" = "blue", "2" = "black", "3" = "red"),
    labels = c("1" = "Democrat", "2" = "Independent", "3" = "Republican"),
    name = NULL
  ) +
  labs(
    title = "Harris vs Trump (2024)",
    x = "",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background  = element_rect(fill = "white",  color = NA),  # outside panel
    panel.background = element_rect(fill = "gray90", color = NA),  # inside panel only
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18)
  ) 




##### Preference Difference. Biden vs Trump
ggplot(anes %>% filter(!is.na(pid3)), aes(x = diff_cand_b, fill = factor(pid3))) +
  geom_histogram(binwidth = 2, color = "white") +
  scale_fill_manual(
    values = c("1" = "#1f78b4", "2" = "black", "3" = "#e31a1c"),
    labels = c("1" = "Democrat", "2" = "Independent", "3" = "Republican"),
    name = NULL
  ) +
  labs(
    title = "Biden vs Trump (2024)",
    x = "",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background  = element_rect(fill = "white",  color = NA),  # outside panel
    panel.background = element_rect(fill = "gray90", color = NA),  # inside panel only
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18)
  ) 





#######. Models 

anes$edu3 <- relevel(factor(anes$edu3), ref = "1")      
anes$race <- relevel(factor(anes$race), ref = "2")  
anes$age_cat <- relevel(factor(anes$age_cat), ref = "4")
anes$income3<- relevel(factor(anes$income3), ref = "2") 

anes$race3 <- ifelse(anes$race %in% c(1, 2, 3), anes$race, NA)
anes$race3 <- relevel(factor(anes$race3), ref = "2")  


anes$white    <- ifelse(anes$race3 == 1, 1, 0)
anes$black    <- ifelse(anes$race3 == 2, 1, 0)
anes$hispanic <- ifelse(anes$race3 == 3, 1, 0)

# Harris model
model_harris <- glm(vote_harris ~ diff_cand + dem24 + rep24 + 
                      edu3 + race + age_cat + income3 + 
                      born_again + news + married + female + ownhome, family = binomial(link = "logit"), 
                    data = anes)



# Trump model
model_trump <- glm(vote_trump ~ diff_cand + dem24 + rep24 + 
                     edu3 + white + black + age_cat + income3 + 
                     born_again + news + married + female + ownhome, family = binomial(link = "logit"), 
                   data = anes)


# Model 1: Harris vs Trump vote
model_harris_neg1 <- glm(vote_harris ~ neg_partisan24 + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 1))


model_trump_neg1 <- glm(vote_trump ~ neg_partisan24 + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 1))

model_harris_neg3 <- glm(vote_harris ~ neg_partisan24 + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 3))


model_trump_neg3 <- glm(vote_trump ~ neg_partisan24 + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 3))


model_harris_neg2 <- glm(vote_harris ~ neg_partisan24 + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 2))


model_trump_neg2 <- glm(vote_trump ~ neg_partisan24 + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 2))


# Model 2: Trump vote
model_harris1 <- glm(vote_harris ~ diff_cand + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 1))


model_trump1 <- glm(vote_trump ~ diff_cand + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 1))


model_harris3 <- glm(vote_harris ~ diff_cand + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 3))


model_trump3 <- glm(vote_trump ~ diff_cand + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 3))


model_harris2 <- glm(vote_harris ~ diff_cand + retrospect +
                           edu3 + race3 + age_cat + income3 + 
                           born_again + news + married + female + ownhome, 
                         family = binomial(link = "logit"), 
                         data = subset(anes, pid3 == 2))


model_trump2 <- glm(vote_trump ~ diff_cand + retrospect +
                          edu3 + race3 + age_cat + income3 + 
                          born_again + news + married + female + ownhome, 
                        family = binomial(link = "logit"), 
                        data = subset(anes, pid3 == 2))


install.packages('stargazer')
library(stargazer)

stargazer(model_harris_neg2, model_trump_neg2,
          type = "latex",  # use "latex" for LaTeX output
          title = "Logistic Regression: Vote for Trump",
          dep.var.labels = c("Vote for Harris", "Vote for Trump"),
          single.row = TRUE,
          covariate.labels = c(
            "Negative Partisanship",
            "Perception of Economic Trend",
            "Education",
            "Race: White",
            "Race: Hispanic",
            "Age <30",
            "Age 30-40",
            "Age 41-50",
            "Age 61-70",
            "Age >70",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News Exposure",
            "Married",
            "Female",
            "Own Home"
          ),
          keep.stat = c("n"))


stargazer(model_harris2, model_trump2,
          type = "latex",  # use "latex" for LaTeX output
          title = "Logistic Regression: Vote for Trump",
          dep.var.labels = c("Vote for Harris", "Vote for Trump"),
          single.row = TRUE,
          covariate.labels = c(
            "Negative Partisanship",
            "Perception of Economic Trend",
            "Education",
            "Race: White",
            "Race: Hispanic",
            "Age <30",
            "Age 30-40",
            "Age 41-50",
            "Age 61-70",
            "Age >70",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News Exposure",
            "Married",
            "Female",
            "Own Home"
          ),
          keep.stat = c("n"))



##########.  symmetric plot

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)
library(tidyr)

library(ggplot2)
library(dplyr)

# Example data with standard errors
df <- data.frame(
  issue = c("Negative Partisan", "Preference Diff", "Economy", "Democrat", "Republican", "White", "Latino", "Asian"),
  Harris = c(.065,  .0552,  -.22, .82, -.82,  -1.35, -1.98, -2.62 ),
  Harris_se = c(.0049, .0038, .12,  .4125, .3893, .52107,  .58999, .91278),
  Trump = c(-.071,  -.059,  .201,  -.69, .60, 1.82,  2.107,  3.0486),
  Trump_se = c(.01, .004, .12, .43, .39, .638, .697, .9787)
)

# Reshape to long format manually
df_long <- bind_rows(
  df %>% select(issue, coefficient = Harris, se = Harris_se) %>% mutate(VoteChoice = "Harris"),
  df %>% select(issue, coefficient = Trump, se = Trump_se) %>% mutate(VoteChoice = "Trump")
)

# Preserve issue order but reverse it
df_long$issue <- factor(df_long$issue, levels = rev(df$issue))

# Plot with horizontal error bars, legend title, reversed issues, and x-axis from -2 to 2
ggplot(df_long, aes(x = coefficient, y = issue, fill = VoteChoice)) +
  geom_col(width = 0.6, position = "identity") +
  geom_errorbarh(aes(xmin = coefficient - se, xmax = coefficient + se), height = 0.2, color = "black") +
  scale_fill_manual(values = c("Harris" = "steelblue", "Trump" = "firebrick")) +
  geom_vline(xintercept = 0, color = "black") +
  coord_cartesian(xlim = c(-3.7, 3.7)) +  # x-axis from -2 to 2
  theme_minimal(base_size = 14) +
  labs(
    title = "Symmetric Vote Choice in 2024",
    x = "Coefficient",
    y = NULL,
    fill = "Vote Choice"
  )



######. 2024 Harris and Trump



logit vote_harris anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if dem24==1
logit vote_harris anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if rep24==1
logit vote_harris anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if ind24==1

logit vote_trump anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if dem24==1
logit vote_trump anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if rep24==1
logit vote_trump anti_biden ib1. edu3 ib2.race ib4.age_cat ib2.income3 born_again news married female  ownhome if ind24==1



anes$edu3 <- relevel(factor(anes$edu3), ref = "1")      
anes$race <- relevel(factor(anes$race), ref = "2")  
anes$age_cat <- relevel(factor(anes$age_cat), ref = "4")
anes$income3<- relevel(factor(anes$income3), ref = "2") 

anes$race3 <- ifelse(anes$race %in% c(1, 2, 3), anes$race, NA)
anes$race3 <- relevel(factor(anes$race3), ref = "2")  


anes$white    <- ifelse(anes$race3 == 1, 1, 0)
anes$black    <- ifelse(anes$race3 == 2, 1, 0)
anes$hispanic <- ifelse(anes$race3 == 3, 1, 0)


## vote for Harris among Democrats

anes_dem <- subset(anes, dem24 == 1)

model1 <- glm(
  vote_harris ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
  data = anes_dem,
  family = binomial(link = "logit")
)

summary(model1) 



#### vote for Harris among Republicans


anes_rep <- subset(anes, rep24 == 1)

model2 <- glm(
  vote_harris ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
  data = anes_rep,
  family = binomial(link = "logit")
)

summary(model2) 



#### Vote for Harris among Independents


anes_ind <- subset(anes, ind24 ==1)

model3 <- glm(
  vote_harris ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
  data = anes_ind,
  family = binomial(link = "logit")
)

summary(model3) 





# Subset and drop unused factor levels
anes_dem <- droplevels(subset(anes, dem24 == 1))
anes_rep <- droplevels(subset(anes, rep24 == 1))
anes_ind <- droplevels(subset(anes, ind24 == 1))

# Rerun models
model1 <- glm(vote_harris ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_dem, family = binomial(link = "logit"))
model2 <- glm(vote_harris ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_rep, family = binomial(link = "logit"))
model3 <- glm(vote_harris ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_ind, family = binomial(link = "logit"))

# Stargazer table
library(stargazer)
stargazer(model1, model2, model3,
          type = "latex",
          title = "Anti-Biden Sentiment and Voting Favorability in 2024",
          dep.var.labels = "In Favor of Harris",
          column.labels = c("Democrats", "Republicans", "Independents"),
          no.space = TRUE,
          single.row = TRUE,

covariate.labels = c(
  "Anti-Biden",
  "Perceived Economic Trend",
  "College",
  "College or Higher",
  "Race: White",
  "Race: Hispanic",
  "Age $<$30",
  "Age 30-40",
  "Age 41-50",
  "Age 61-70",
  "Age $>$70",
  "Income: Low",
  "Income: High",
  "Born Again",
  "News",
  "Married",
  "Female",
  "Own Home"
),
keep.stat = c("n"))



#### vote for trump


# Subset and drop unused factor levels
anes_dem <- droplevels(subset(anes, dem24 == 1))
anes_rep <- droplevels(subset(anes, rep24 == 1))
anes_ind <- droplevels(subset(anes, ind24 == 1))

# Rerun models
model1b <- glm(vote_trump ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_dem, family = binomial(link = "logit"))
model2b <- glm(vote_trump  ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_rep, family = binomial(link = "logit"))
model3b <- glm(vote_trump ~ anti_biden + retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
              data = anes_ind, family = binomial(link = "logit"))

# Stargazer table
library(stargazer)
stargazer(model1b, model2b, model3b,
          type = "latex",
          title = "Anti-Biden and Vote Choice in 2024",
          dep.var.labels = "In Favor of Trump",
          column.labels = c("Democrats", "Republicans", "Independents"),
          no.space = TRUE,
          single.row = TRUE,
          
          covariate.labels = c(
            "Anti-Biden",
            "Perceived Economic Trend",
            "College",
            "College or Higher",
            "Race: White",
            "Race: Hispanic",
            "Age $<$30",
            "Age 30-40",
            "Age 41-50",
            "Age 61-70",
            "Age $>$70",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News",
            "Married",
            "Female",
            "Own Home"
          ),
          keep.stat = c("n"))



################
library(ggplot2)
library(tidyr)
library(dplyr)

# Data
df <- data.frame(
  Group = c("Democrat", "Republican", "Independent"),
  Set1_Coeff = c(-1.594691, -3.168553, -3.153813),
  Set1_SE = c(0.3544663, 0.3417094, 0.2742825),
  Set2_Coeff = c(2.532652, 3.135409, 3.487479),
  Set2_SE = c(0.5803656, 0.3244368, 0.3203737)
)

# Reshape
df_long <- df %>%
  pivot_longer(cols = starts_with("Set"),
               names_to = c("Set", ".value"),
               names_pattern = "(Set[12])_(.*)")

# Ensure group order
df_long$Group <- factor(df_long$Group,
                        levels = c("Independent", "Republican", "Democrat"))

# Assign colors: Blue = Harris, Red = Trump, with left/right logic
df_long <- df_long %>%
  mutate(ColorKey = case_when(
    Group == "Democrat" & Set == "Set1" ~ "Trump",
    Group == "Democrat" & Set == "Set2" ~ "Harris",
    Group %in% c("Republican", "Independent") & Set == "Set1" ~ "Harris",
    Group %in% c("Republican", "Independent") & Set == "Set2" ~ "Trump"
  ))

# Plot
ggplot(df_long, aes(x = Coeff, y = Group, color = ColorKey)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Coeff - SE, xmax = Coeff + SE), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Harris" = "blue", "Trump" = "red"),
                     name = "Vote Choice") +
  labs(x = "Coefficient",
       y = "",
       title = "Anti-Biden Sentiment in 2024") +
  xlim(-4, 4) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
  )




####### Trump and Biden 

library(ggplot2)
library(tidyr)
library(dplyr)

# Data
df <- data.frame(
  Group = c("Democrat", "Republican", "Independent"),
  Set1_Coeff = c(0.8667322, 2.358525, 1.783668),
  Set1_SE = c(0.1115629, 0.3714039, 0.1807927),
  Set2_Coeff = c(-2.848785, -2.30339, -2.900841),
  Set2_SE = c(0.2930773, 0.1776214, 0.210692)
)

# Reshape
df_long <- df %>%
  pivot_longer(
    cols = starts_with("Set"),
    names_to = c("Set", ".value"),
    names_pattern = "(Set[12])_(.*)"
  )

# Ensure group order
df_long$Group <- factor(df_long$Group, levels = c("Independent", "Republican", "Democrat"))

# Assign colors: switch Democrat colors
df_long <- df_long %>%
  mutate(ColorKey = case_when(
    Group == "Democrat" & Set == "Set1" ~ "Biden",
    Group == "Democrat" & Set == "Set2" ~ "Trump",
    Group %in% c("Republican", "Independent") & Set == "Set1" ~ "Biden",
    Group %in% c("Republican", "Independent") & Set == "Set2" ~ "Trump"
  ))

# Plot
ggplot(df_long, aes(x = Coeff, y = Group, color = ColorKey)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Coeff - SE, xmax = Coeff + SE), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Biden" = "blue", "Trump" = "red"),
                     name = "Vote Choice") +
  labs(
    x = "Coefficient",
    y = "",
    title = "Anti-Trump Sentiment in 2020"
  ) +
  coord_cartesian(xlim = c(-4, 4)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
  )



####### Trump and Clinton 


library(ggplot2)
library(tidyr)
library(dplyr)

# Data
df <- data.frame(
  Group = c("Democrat", "Republican", "Independent"),
  Set1_Coeff = c(-0.931136, -1.723293, -1.564871),
  Set1_SE = c(0.0889036, 0.3205219, 0.1290202),
  Set2_Coeff = c(2.219352, 0.736724, 1.489331),
  Set2_SE = c(0.2498372, 0.1191461, 0.1251784)
)

# Reshape
df_long <- df %>%
  pivot_longer(
    cols = starts_with("Set"),
    names_to = c("Set", ".value"),
    names_pattern = "(Set[12])_(.*)"
  )

# Ensure group order
df_long$Group <- factor(df_long$Group, levels = c("Independent", "Republican", "Democrat"))

# Assign colors: switch Democrat colors
df_long <- df_long %>%
  mutate(ColorKey = case_when(
    Group == "Democrat" & Set == "Set1" ~ "Clinton",
    Group == "Democrat" & Set == "Set2" ~ "Trump",
    Group %in% c("Republican", "Independent") & Set == "Set1" ~ "Clinton",
    Group %in% c("Republican", "Independent") & Set == "Set2" ~ "Trump"
  ))

# Plot
ggplot(df_long, aes(x = Coeff, y = Group, color = ColorKey)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Coeff - SE, xmax = Coeff + SE), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c("Clinton" = "blue", "Trump" = "red"),
    name = "Vote Choice"
  ) +
  labs(
    x = "Coefficient",
    y = "",
    title = "Anti-Obama Sentiment in 2016"
  ) +
  coord_cartesian(xlim = c(-4, 4)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray85", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
  )




##############. Determinant of Negative Partisanship

reg neg_partisan24 retrospect ib1.edu3 race ib4.age_cat ib2.income3 born_again news married female  ownhome if pid3==1



model1 <- lm(neg_partisan24 ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 1))


model2 <- lm(neg_partisan24 ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 3))


model3 <- lm(neg_partisan24 ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 2))


### diff_cand

model1b <- lm(diff_cand ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 1))


model2b <- lm(diff_cand ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 3))


model3b <- lm(diff_cand ~ retrospect +
               edu3 + race3 + age_cat + income3 +
               born_again + news + married + female + ownhome,
             data = subset(anes, pid3 == 2))



######.  2024 Turnout

anes$edu3 <- relevel(factor(anes$edu3), ref = "1")      
anes$race <- relevel(factor(anes$race), ref = "2")  
anes$age_cat <- relevel(factor(anes$age_cat), ref = "4")
anes$income3<- relevel(factor(anes$income3), ref = "2") 


V242095x
anes$turnout <- ifelse(anes$V242095x == 1, 1,
                     ifelse(anes$V242095x == 0, 0, NA))



# Subset and drop unused factor levels
anes_dem <- droplevels(subset(anes, dem24 == 1))
anes_rep <- droplevels(subset(anes, rep24 == 1))
anes_ind <- droplevels(subset(anes, ind24 == 1))

## Democrat
model1a <- glm(turnout ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_dem, family = binomial(link = "logit"))

model1b <- glm(turnout ~ np24 + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_dem, family = binomial(link = "logit"))

model1c <- glm(turnout ~retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_dem, family = binomial(link = "logit"))


# Stargazer table
library(stargazer)
stargazer(model1a, model1b, model1c,
          type = "latex",
          title = "Determinants of Turnout in 2024 (Democrat)",
          dep.var.labels = "Turnout",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          covariate.labels = c(
            "Anti-Biden",
            "Negative Partisanship",
            "Economic Trend",
            "Some College",
            "College or Higher",
            "Race: White",
            "Race: Hispanic",
            "Age $<30$",
            "Age 30–40",
            "Age 41–50",
            "Age 61–70",
            "Age $>70$",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News",
            "Married",
            "Female",
            "Own Home"
          ),
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          no.space = TRUE,
          single.row = TRUE)


### 2024 Republican 

## Republican
model1a <- glm(turnout ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_rep, family = binomial(link = "logit"))

model1b <- glm(turnout ~ np24 + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_rep, family = binomial(link = "logit"))

model1c <- glm(turnout ~retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_rep, family = binomial(link = "logit"))


# Stargazer table
library(stargazer)
stargazer(model1a, model1b, model1c,
          type = "latex",
          title = "Determinants of Turnout in 2024 (Republican)",
          dep.var.labels = "Turnout",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          covariate.labels = c(
            "Anti-Biden",
            "Negative Partisanship",
            "Economic Trend",
            "Some College",
            "College or Higher",
            "Race: White",
            "Race: Hispanic",
            "Age $<30$",
            "Age 30–40",
            "Age 41–50",
            "Age 61–70",
            "Age $>70$",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News",
            "Married",
            "Female",
            "Own Home"
          ),
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          no.space = TRUE,
          single.row = TRUE)


######. 2024 Independent 



## Independent
model1a <- glm(turnout ~ anti_biden + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_ind, family = binomial(link = "logit"))

model1b <- glm(turnout ~ np24 + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_ind, family = binomial(link = "logit"))

model1c <- glm(turnout ~retrospect + edu3 + race3 + age_cat + income3 + born_again + news + married + female + ownhome,
               data = anes_ind, family = binomial(link = "logit"))


# Stargazer table
library(stargazer)
stargazer(model1a, model1b, model1c,
          type = "latex",
          title = "Determinants of Turnout in 2024 (Independent)",
          dep.var.labels = "Turnout",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          covariate.labels = c(
            "Anti-Biden",
            "Negative Partisanship",
            "Economic Trend",
            "Some College",
            "College or Higher",
            "Race: White",
            "Race: Hispanic",
            "Age $<30$",
            "Age 30–40",
            "Age 41–50",
            "Age 61–70",
            "Age $>70$",
            "Income: Low",
            "Income: High",
            "Born Again",
            "News",
            "Married",
            "Female",
            "Own Home"
          ),
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          no.space = TRUE,
          single.row = TRUE)


#### Test for GitHub desktop