# ------------------------------------------------------------------------------
# Verification of the referential used by motor control in reaching movements
#
# Description: This code will perform the statistical analysis of this project,
# checking if there is a significant difference between the two reference
# systems through a mixed linear model, where we compare the fixed effects
# (i.e. the change of the reference) taking into account the random effects
# (caused by the presence of different individuals).
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 18/08/2022
# ------------------------------------------------------------------------------


# Necessary packages -----------------------------------------------------------

library(lme4)

# Performing the statistical analysis for W-------------------------------------

indices_MED <- read.csv("./output/indices_MED.csv")                             # Reading output table

model_W <- lme4::lmer(W_elements ~ referential_elements + (1|ind_output),       # Making the linear model with mixed effects (Fixed effect being
                    indices_MED)                                                # the referential and random effect being the difference between individuals)

null_model_W <- lme4::lmer(W_elements ~  1  + (1|ind_output), indices_MED)      # Making the linear model without the fixed effect (null-model)

summary(model_W)

anova(model_W, null_model_W)                                                    # Model selection to verify which model better explains the data

(model_W)

# For R^2 ----------------------------------------------------------------------

model_R2 <- lme4::lmer(R2_elements ~ referential_elements + (1|ind_output),     # Making the linear model with mixed effects (Fixed effect being
                      indices_MED)                                              # the referential and random effect being the difference between individuals)

null_model_R2 <- lme4::lmer(R2_elements ~  1  + (1|ind_output), indices_MED)    # Making the linear model without the fixed effect (null-model)

summary(model_R2)

anova(model_R2, null_model_R2)                                                  # Model selection to verify which model better explains the data

(model_R2)
