#
# Project title: LTC Focus dataset analysis 
# R script author: AF
# contact: aferra@uw.edu
# Location: UW
# 
# Version: Feb 2025

# Summary
# 1. script resources and data transformation
# 2. Creating table 1
# 3. Fit Main regression model
# 4. Calculating ICC
# 5. Fit interaction model 
# 6. Creating supplementary table S2
# 7. Creating Boxplot (Fig 2).

# loading datasets and libraries

library(readxl)
library(tidyverse)
library(lme4)
library(tableone)
library(pbkrtest)
library(lmerTest)
library(msm)

# script resources and data transformation ----
results <- list()
relevant_vars <- c("accpt_id", "state", "county", 
                   # facility
                   "totbeds", "alzunit", "profit", "hospbase", 
                   # characteristics of residents
                   "restrain", "acuindex2",
                   "avgage", 
                   "pctchf", "pcthyper","agg_female", 
                   "pctvent_mds3",
                   "pctlowcfs", "pctmidcfs", "pcthighcfs",
                   # outcomes
                   "adj_rehosprate")

numeric_vars <- c("totbeds",
                  "restrain", "acuindex2",
                  "avgage", 
                  "pctchf", "pcthyper", "agg_female", 
                  "pctvent_mds3", 
                  "pctlowcfs", "pctmidcfs", "pcthighcfs",
                  "fup_adj_rehosp"
)

binary_vars <- c("alzunit", "profit", "hospbase", "state")

directory <- "C:/Users/augus/OneDrive/r_projects/bios_528/"

ltc_merged <- read.csv(paste0(directory, 
                       "datasets/ltc_merged.csv")
)

# formatting columns
for (variable  in numeric_vars){
  ltc_merged[[variable]] <- as.numeric(ltc_merged[[variable]])
}

for (variable  in binary_vars){
  ltc_merged[[variable]] <- as.factor(ltc_merged[[variable]])
}

# creating table 1. ----
table1 <- CreateTableOne(vars = c("profit", "hospbase",
                                  numeric_vars), 
                         strata = "alzunit", 
                         data = ltc_merged)
table1
table1_df <- print(table1, quote = FALSE)

write.csv(table1_df, file = "table1.csv")

# fitting main regression model ----
main_model <- lmer(fup_adj_rehosp ~ alzunit + 
                      # adjustment vars, fixed
                      profit + hospbase + acuindex2 + avgage + 
                      pctchf + pcthyper + pctvent_mds3 + 
                      totbeds + agg_female +
                      pctlowcfs + pctmidcfs + pcthighcfs +
                      # random effects 
                      (1 | state), 
                    data = ltc_merged 
                    )

main_model_results <- summary(main_model)
summary(main_model)

# getting 95%CI
point_est <- fixef(main_model)[2]

standard_errors <- main_model_results$coefficients[, "Std. Error"]
se <- standard_errors[2]

results$main_model <- c(point_est, 
                        point_est - 1.96*se, 
                        point_est + 1.96*se)
# p value
p_values <- main_model_results$coefficients[, "Pr(>|t|)"]
p_values

# getting the ICC for the main model  ----
variance_components <- as.data.frame(VarCorr(main_model))

# Extract the variance of the random effect and the residual variance
random_effect_variance <- variance_components$vcov[1]
residual_variance <- variance_components$vcov[2]

# Calculate the ICC
results$icc_main_model <- random_effect_variance / 
                            (random_effect_variance + residual_variance)

# Print the ICC
print(results$icc_main_model)

# interaction model ----
interaction_model <- lmer(fup_adj_rehosp ~ alzunit*profit +
                     # adjustment vars, fixed
                     hospbase + acuindex2 + avgage + 
                     pctchf + pcthyper + pctvent_mds3 + 
                     totbeds + agg_female +
                     pctlowcfs + pctmidcfs + pcthighcfs +
                     # random effects 
                     (1 | state), 
                   data = ltc_merged 
)
interaction_model_results <- summary(interaction_model)
summary(interaction_model)

# no profit stratum
point_est_nop <- fixef(interaction_model)[2]

standard_errors_interaction<- interaction_model_results$coefficients[, "Std. Error"]
se_int <- standard_errors_interaction[2]

results$interaction_noprofit <- c(point_est_nop, 
                                  point_est_nop - 1.96*se_int, 
                                  point_est_nop + 1.96*se_int)

# getting results for the profit and intervention
point_estimate_profit <- fixef(interaction_model)[15] + 
                            fixef(interaction_model)[15]
se_profit <- deltamethod(g = ~x2 + x15, mean = fixef(interaction_model), 
                         cov = vcov(interaction_model))
# results of interaction, profit
results$interaction_profit <- c(point_estimate_profit, 
                                point_estimate_profit - 1.96*se_profit,
                                point_estimate_profit + 1.96*se_profit)

p_values_int <- interaction_model_results$coefficients[, "Pr(>|t|)"]
p_values_int
results$p_interaction <- p_values_int[15]

# creating supplementary table S2. ----
states <- table(ltc_merged$state, ltc_merged$alzunit, 
                deparse.level = 2)

print(states)

#write.csv(states, "states_per_arm.csv")

# plot results. Figure 2 ----
ggplot(ltc_merged, aes(x = factor(alzunit), 
                        y = fup_adj_rehosp, 
                        fill = factor(alzunit))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = factor(alzunit)),  # Add color aesthetic here
              width = 0.1, 
              size = 2, 
              alpha = 0.12) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Dementia care training", 
       y = "Monthly rate of re-admissions",
       fill = "") +
  scale_fill_discrete(labels = c("Control", "Intervention")) +
  scale_color_discrete(labels = c("Control", "Intervention")) +  # Add scale_color_discrete for legend
  scale_x_discrete(labels = c("Comparator", "Intervention")) +
  scale_y_continuous(labels = c("10%", "20%", "30%", "40%", "50%")) +  # Add this line to change y-axis labels
  theme(legend.position = "none",
        text = element_text(size = 18))  # Add this line to increase font size

# end of R script. 
