# libraries !
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(janitor)
library(tidybayes)
library(broom.mixed)

# 19.2 - Incorporating two (or more!) grouping variables

# data !
data("climbers_sub")
climbers <-
  climbers_sub %>%
  select(peak_name, 
         expedition_id,
         member_id,
         success,
         year, 
         season,
         age,
         expedition_role,
         oxygen_used)

# in Chapter 18, we built a model based on grouped expeditions
# but there's another grouping var - the mountain peak!

# expeditions group
expeditions <- 
  climbers %>%
  group_by(peak_name, expedition_id) %>%
  summarise(n_climbers = n())

expeditions

# peak group
expeditions %>%
  group_by(peak_name) %>%
  summarise(n_expeditions = n(),
            n_climbers = sum(n_climbers))

# whats p_success per peak?
climbers %>%
  group_by(peak_name) %>%
  summarise(p_success = mean(success)) %>%
  ggplot(aes(x = p_success)) +
  geom_histogram(color = "white")

# original one-group model (for expedition)
climb_model_1 <-
  stan_glmer(
    success ~ age + oxygen_used + (1 | expedition_id),
    data = climbers,
    family = binomial(),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# nested group model (peak & expedition)
climb_model_2 <- 
  stan_glmer(
    success ~ age + oxygen_used + (1 | expedition_id) + (1 | peak_name),
    data = climbers,
    family = binomial(),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2, 
    seed = 84735
  )

# posterior of both fixed effects
climb_model_1_mean <- tidy(climb_model_1, effects = "fixed")
climb_model_2_mean <- tidy(climb_model_2, effects = "fixed")

climb_model_1_mean %>%
  right_join(climb_model_2_mean, 
             by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("std"))

# posterior of both variances
climb_model_1_var <- tidy(climb_model_1, effects = "ran_pars")
climb_model_2_var <- tidy(climb_model_2, effects = "ran_pars")

climb_model_1_var %>%
  right_join(climb_model_2_var, 
             by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("group"))

# group specific parameters
climb_model_2_mean %>%
  select(term, estimate)

# group-level terms
climb_model_2 %>%
  tidy(effects = "ran_vals") %>%
  select(level, group, estimate) %>%
  filter(group == "peak_name") %>%
  slice_head(n = 2)

