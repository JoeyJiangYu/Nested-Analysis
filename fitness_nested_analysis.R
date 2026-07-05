
rm(list = ls())


# Nested Analysis Simulation: workout habit formation
set.seed(849)
library(ggplot2)
library(dplyr)
library(broom)

# 1. Create simulated dataset. ----

n <- 30
fitness <- data.frame(
  case = paste0("Person_", 1:n),
  
  # Three SNA-generated rival explanations
  
  routine_integration = sample(1:5, n, replace = TRUE),
  
  self_image_change = sample(1:5, n, replace = TRUE),
  
  social_reinforcement = sample(1:5, n, replace = TRUE))


# 2. Simulate outcome: successful formation of a workout habit ----


fitness$habit_formation <- 0.5 * fitness$routine_integration +
  0.7 * fitness$self_image_change +
  0.3 * fitness$social_reinforcement +
  rnorm(n, mean = 0, sd = 0.8)

# Look at the dataset

View(fitness)  # All the variables are continuous variables. 

# 3. Preliminary LNA regression model ----

model <- lm(
  habit_formation ~ routine_integration + self_image_change 
  + social_reinforcement, data = fitness)

# 4. Show model results----

summary(model)

# Explanation of the full model --

 # Estimates: routine_integration 0.45; self_imagine_change 0.78; 
  # social_reinforcement 0.38. All the three factors have positive effects
  # on workout habit formation. Among them, self image change has the highest
  # effect, then routine integration. Social reinforcement is the least influential.

 # Model performance: How does the model fit the data?
 # R^2=0.632, meaning that this model explains 63% variations of habit formation.
 # p-value: All the three IVs are significant.

# 5. model fit assessment (satisfactory): diagnostic plot in nested analysis----
# Diagnostic plot compares the predicted Y value of each case and the observed
# values. It actually demonstrates residuals.
# residual = observed value - predicted value

# Generate predicted value.
fitness$predicted <- predict(model)

# Generate residual = observed value - predicted value
fitness$residual <- fitness$habit_formation - fitness$predicted
fitness$abs_residual <- abs(fitness$residual)  #abs() is a function to 
# calculate the absolute value.

# Rename cases as numbers.
fitness$case <- 1:nrow(fitness)
View(fitness)

plot(fitness$predicted,
     fitness$habit_formation,
     xlab = "Predicted habit formation",
     ylab = "Observed habit formation",
     main = "Diagnostic Plot")
abline(0, 1, lty = 2) # This line is the perfect prediction line. Cases on the
# line means their predicted values perfectly match their observed values.
# Case above the line means observed value is higher than predicted value.
# Case under the line means observed value is lower than predicted.
# The farther a case deviates from the line, the larger its residual.
# We use residual to identify outliers. In nested analysis, we need to identify
# on-the-line cases and off-the-line cases.

# For simplicity of model fit assessment, I use R^2 to estimate model fitness.
summary(model)
## Multiple R-squared:  0.6323,	Adjusted R-squared:  0.5899 ----
# multiple R^2 means that about 63% variations of y can be explained by this model.

# adjusted R^2 means that after correcting for the number of x variables, waht
# percentage of y variabtions can still be explained by the model.
 # If adding another x which has explanatory power, adjusted R^2 will rise.
 # If adding another useless x, adjusted R^2 will fall.

# Just for simulation, we assume that 63% and 59% indicate that this model is 
 # sufficiently fitted.


# 6. Robustness check ----
# simple robustness check in conventional statistical analysis
# I split the full model into sub-models with different x variables. The goal is to 
 # check whether the estimate and the effecting direction of some x variable remain stable 
 # across models: (1)The directions should be identical across models; (2)The estimates of
 # the same x variable can't vary too much.

## sub-model = 1 IV ----
model_1_routine <- lm(habit_formation ~ routine_integration, data = fitness)
summary(model_1_routine)

model_1_self <- lm(habit_formation ~ self_image_change, data = fitness)
summary(model_1_self)

model_1_social <- lm(habit_formation ~ social_reinforcement, data = fitness)
summary(model_1_social)

## sub-model = 2 IVs ----

model_2_routine_self <- lm(habit_formation ~ routine_integration + self_image_change,
                           data = fitness)
summary(model_2_routine_self)

model_2_routine_social <- lm(habit_formation ~ routine_integration + social_reinforcement,
                             data = fitness)
summary(model_2_routine_social)

model_2_self_social <- lm(habit_formation ~ self_image_change + social_reinforcement,
                             data = fitness)
summary(model_2_self_social)

## full model = 3 IVs ----

model_full <- model
summary(model_full)

## Summarize estimates across models. ----
library(tidyr)

# Collect model estimates

estimate_table <- bind_rows(tidy(model_1_routine) %>% mutate(model = "Routine only"),
                            tidy(model_1_self) %>% mutate(model = "Self-image only"),
                            tidy(model_1_social) %>% mutate(model = "Social only"),
                            tidy(model_2_routine_self) %>% mutate(
                              model = "Routine + Self-image"),
                            tidy(model_2_routine_social) %>% mutate(
                              model = "Routine + Social-reinforcement"),
                            tidy(model_2_self_social) %>% mutate(
                              model = "Self-image + Social-reinforcement"),
                            tidy(model_full) %>% mutate(model = "Full model")) %>%
  filter(term != "(Intercept)") %>%
  select(model, term, estimate)

# Make a wide table for PPT

estimate_table_ppt <- estimate_table %>% 
  pivot_wider(names_from = model, values_from = estimate) %>%
  mutate(across(where(is.numeric), round, 3))

View(estimate_table_ppt)

## Interpretation ----

### routine integration ----
# Routine integration's estimates across all models are similar, suggesting that it is 
  # a stable explanatory factor across specifications thus a candidate for model-testing SNA.


### self-image change ----
# Self-image change has similar estimates in the model of "Self-image only" (0.576) and 
 # "Routine + Self-image" (0.516), indicating that adding routine integration doesn't
 # substantially change the relationship between self-image change and workout habit
 # formation.

# Self-image change's estimate in the model "Self-image + Social-reinforcement" (0.796)
 # increased substantially, from 0.576 to 0.796, indicating that self-image change and 
 # social reinforcement may have associations with each other, which deserves further 
 # investigation.
 # As a candidate for model-testing SNA: To investigate the performance of y, when
  # self-image change and social reinforcement both occur.
 # As a candidate for model-building SNA: To investigate the mechanism between the two x 
  # variables.

# Self-image change's estimate in full model (0.776) doesn't vary much from it in the model 
 # of "Self-image + social-reinforcement" (0.796), suggesting that routine integration 
 # doesn't confound the relationship between self-image change and workout habit formation
 # and further supporting the independent explanatory power of routine integration.

# Overall, self-image change has positive effect on workout habit formation and is 
 # sensitive to social reinforcement.


### social reinforcement ----
# Social reinforcement appears to effect workout habit formation negatively.

# If routine integration is added to social reinforcement, the negative effect of social 
 # reinforcement would only slightly reduced, again suggesting the independent explanatory
 # power of routine integration.

# Social reinforcement's estimate reversely changes to a positive effect, after adding
 # self-image change. It indicates that social reinforcement's effect on workout habit 
 # formation is highly sensitive to self-image change. This pattern reveals an unexplored
 # puzzle that deserves further investigation -- a candidate for model-building SNA.

 # Together with the interpretation of self-image change above, a possible hypothesis could 
  # be: Between self-image change and social reinforcement, there could exist a moderating
  # mechanism through which one factor is either enhanced or reversely moderated by the 
  # other. It requires further examination to figure out which one is the moderator.

# The estimate of social reinforcement in the full model doesn't vary much away from it in
 # self-image + Social-reinforcement. The independent explanatory power of routine 
 # integration is confirmed again.




# 7. Case Selection ----
## model-testing candidates ----

# candidate 1. x = routine integration, y = workout habit formation
# candidate 2. x = self-image change + social reinforcement, y = workout habit formation，
 # to test if the combination of self-image change and social reinforcement has stable 
 # positive effect on workout habit formation.

## model-building candidates ----
 # Hypothesis: Between self-image change and social reinforcement, one of them might be 
 # the conditional variable to moderator the other's relationship with workout habit 
 # formation.


## Identify 4 on-the-line cases: smallest residuals. ----
on_the_line_cases <- fitness[order(fitness$abs_residual), ][1:2, ]

## Identify 4 off-the-line cases: largest residuals. ----
off_the_line_cases <- fitness[order(-fitness$abs_residual), ][1:2, ]

# Set default cases.
fitness$case_type <- "other"

# Mark on-the-line and off-the-line cases.
fitness$case_type[fitness$case %in% on_the_line_cases$case] <- "on-the-line"
fitness$case_type[fitness$case %in% off_the_line_cases$case] <- "off-the-line"

# Define colors.
case_colors <- ifelse(
  fitness$case_type == "on-the-line", "blue",
  ifelse(fitness$case_type == "off-the-line", "red","black"))

# Plot the diagnostic plot.
plot(fitness$predicted,
     fitness$habit_formation,
     xlab = "Predicted habit formation",
     ylab = "Observed habit formation",
     main = "Diagnostic Plot",
     pch = 19,
     col = case_colors)
abline(0, 1, lty = 2)   

text(fitness$predicted,
     fitness$habit_formation,
     labels = fitness$case,
     pos = 4,
     cex = 0.7,
     col = case_colors)
legend("topleft",
       legend = c("on-the-line cases", "off-the-line cases", "other cases"),
       col = c("blue", "red", "black"),
       pch = 19,
       bty = "n")

# Check the on-the-line cases & off-the-line cases.
on_the_line_cases[, c("case", "habit_formation", "predicted", "residual", "abs_residual")]
## on-the-line cases: case 11, case 17. ----

off_the_line_cases[, c("case", "habit_formation", "predicted", "residual", "abs_residual")]
## off-the-line cases: case 18, case 14. ----

# I don't choose another two on-the-line cases for model-building SNA for the sake of
# efficiency. 

### the principle for model-testing SNA ----
# Examine whether the causal mechanism according to the model results really can be observed
# in concrete cases. That's why the on-the-line case selection can be either deliberate or 
# random.

### the principle for model-building SNA ----
# Explain why our hypotheses fail in some cases, namely explain outliers / anomalies.
# Thus, the case selection need to be deliberate.
# Comparison between deliberately selected on-the-line cases vs. off-the-line case aims to
# find out the key differences and help further develop a more coherent model.

# 8. Model-testing SNA: routine integration ----
# To be compared cases: case 17, case 11.

# Extract the values of case 17 and case 11.
fitness %>% filter(case %in% c(17, 11))
# Suppose our hypothesis is that routine integration positive effects workout habit
# formation. 
 # From the statistic scores of case 11 and case 17, we can observe identical values of
 # self-image change in both cases and little variation in social reinforcement. But 
 # their routine integration values are obviously different and case 17 with higher
 # routine integration value has low residual, namely its model-predicted value is 
 # very close to its actual value. Then we can conclude that the comparison is consistent
 # with the statistical relationship identified by the LNA. 

 # Besides, case 17 scores higher in routine integration than case 11 (4:2), case 17 also
 # score higher than case 11 in habit_formation, their abs_residuals follow the same
 # pattern. 
 # In sum, from statistics, case 17 and case 11 fit our model.

 # When we conduct interview with the two people, person 11 and person 17, we found
  # that their differences display the same pattern as our model. The qualitative 
  # evidence therefore provides additional support for our hypothesis. The nested 
  # analysis can then be concluded.

## theoretical flaws ----
 # When we interview case 17, we find that his high routine integration frequency is
 # driven by test pressure, because he must pass the semester fitness tests (such as 
 # in China).  
 # The actual working mechanism is that tests pressure induces both routine integration
 # and workout habit formation simultaneously. On this point, routine integration
 # has no causal relationship with workout habit formation. They are just covariances.
 # Therefore, case 17 is a case of theoretical flaws.

 # In sum, theoretical flaws appear: The values of X and Y fit the theoretical expectation
  # in the LNA model. But the actual mechanism is driven by another variable. 
 
 # Then we turn to rebuild the model.

## idiosyncratic case ----
 # If we find that case 17 scores high values on routine integration and workout habit
 # because he is undergoing rehabilitation therapy after suffering a major surgery. This 
 # mechanism is quite case-specific and hard to be applied to other cases. This is 
 # an idiosyncratic case. 
 # Then we turn to repeat model testing to investigate other on-the-line cases which 
 # are not influenced by case-specific reasons or extreme circumstances.

# 9. Model-building SNA ----
View(estimate_table_ppt)
# In our model, model results of self-image change and social reinforcement are not
# robust. Since we confirmed the explanatory power of routine integration independent 
# of other two x variables, and for the purpose of demonstration of the model-building
# path, I intentionally split out another data frame only excluding routine integration.
model_build_df <- fitness %>% select(
  case, self_image_change, social_reinforcement, habit_formation) 

# Now run the model_build data frame to check the robustness of x variables.
mb_model <- lm(habit_formation ~ self_image_change + social_reinforcement,
                     data = model_build_df)
summary(mb_model)
## satisfactory or not ----
# Now we see the adjusted r^2 is 0.38, meaning that only 38% of variations in y variable
 # can be explained by this model. We are not satisfied with this result.

## robustness check ----
# Now our full model contains 2 x variables, we just need to create sub-models with 1
 # variable.
### sub-models = 1 IV ----
mb_sub_model_self <- lm(habit_formation ~ self_image_change,
                        data = model_build_df)
summary(mb_sub_model_self)

mb_sub_model_social <- lm(habit_formation ~ social_reinforcement,
                          data = model_build_df)
summary(mb_sub_model_social)

### summarize estimates across models ----
mb_estimate_table <- bind_rows(tidy(mb_sub_model_self) %>%
                                 mutate(model = "self-image only"),
                               tidy(mb_sub_model_social) %>%
                                 mutate(model = "social reinforcement only"),
                               tidy(mb_model) %>%
                                 mutate(model = "full model")) %>%
  filter(term != "(Intercept") %>%
  select(model, term, estimate)



View(mb_estimate_table)

# Create a estimates table for PPT.
mb_estimate_table_ppt <- mb_estimate_table %>%
  filter(term != "(Intercept)") %>%
  pivot_wider(names_from = model,
              values_from = estimate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

View(mb_estimate_table_ppt)

### Assessment robustness----
# The estimate of self-image change is not stable across models, from 0.576 in 1 IV model
 # to 0.796 in full model.
# The estimate of social reinforcement is also not stable across models. Its direction
 # even changed, from -0.165 in 1 IV model to 0.311 in full model.

### mb_diagnostic plot ----
View(model_build_df)

# Generate predicted habit formation, residual, abs_residual.
model_build_df$predicted <- predict(mb_model)
model_build_df$residual <- model_build_df$habit_formation - model_build_df$predicted
model_build_df$abs_residual <- abs(model_build_df$residual)

# Plot mb_diagnostic plot
plot(model_build_df$predicted,
     model_build_df$habit_formation,
     xlab = "Predicted habit foramtion",
     ylab = "observed habit formation",
     main = "Diagnostic Plot: Model-Building SNA",
     pch = 19,
     col = "black")                      
abline(0, 1, lty = 2)

text(model_build_df$predicted,
     model_build_df$habit_formation,
     labels = model_build_df$case,
     pos = 4,
     cex = 0.7)

## deliberate case selection ----
### Identify 3 on-the-line cases: according to the smallest absolute residuals ----
mb_on_the_line_cases <- model_build_df %>%
  arrange(abs_residual) %>%
  slice_head(n = 3)
mb_on_the_line_cases
# Case 11, case 24 and case 29 are the deliberately selected 3 on-the-line cases.

points(mb_on_the_line_cases$predicted,
       mb_on_the_line_cases$habit_formation,
       pch = 19,
       col = "blue",
       cex = 1.4)    # Pinpoint the three on-the-line cases.

legend("topleft",
       legend = c("on-the-line cases",
                  "off-the-line cases",
                  "others",
                  "perfect prediction"),
       col = c("blue",
               "red",
               "black",
               "black"),
       pch = c(19,19,19, NA),
       lty = c(NA, NA, NA, 2),
       bty = "n")

### Identify 3 off-the-line cases: according to the largest absolute residuals ----
mb_off_the_line_cases <- model_build_df %>%
  arrange(-abs_residual) %>%
  slice_head(n=3)
mb_off_the_line_cases
# Case 18, case 19 and case 9 are the deliberately selected 3 off-the-line cases.

points(mb_off_the_line_cases$predicted,
       mb_off_the_line_cases$habit_formation,
       pch = 19,
       col = "red",
       cex = 1.4)    # Pinpoint the three off-the-line cases.

### Compare the selected cases. ----
mb_compare <- bind_rows(
  mb_on_the_line_cases %>% mutate(case_type = "on-the-line"),
  mb_off_the_line_cases %>% mutate(case_type = "off-the-line")
  ) %>% 
  select(case, case_type, self_image_change, social_reinforcement, 
         habit_formation, abs_residual) %>%
  mutate(abs_residual = round(abs_residual, 2)) %>%
  arrange(abs_residual)

View(mb_compare)

# The next step is to conduct comparative case studies with keeping the following questions
# in mind:
 # How to explain the difference between pairs of on-the-line cases and off-the-line case?
 # Do they suggest any new thesis or hypothesis?

# If we can formulate a new hypothesis (or thesis), we return to LNA model testing.
# If we can't, end this research and try to explain why this model failed.
# But we still can conclude that this model is not sufficietnly supported by the 
# available evidence.

