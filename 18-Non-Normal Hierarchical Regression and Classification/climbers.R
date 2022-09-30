# libraries !
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)

# 18.1 - Hierarchical logistic regression

# data !
data("climbers_sub")
climbers <-
  climbers_sub %>%
  select(expedition_id,
         member_id,
         success,
         year,
         season,
         age,
         expedition_role,
         oxygen_used)

climbers %>%
  tabyl(success)

# member_id is actually not a grouping var
# but expedition_id is!

# 200 expeditions
climbers_per_expedition <- 
  climbers %>%
  group_by(expedition_id) %>%
  summarise(count = n())

# lots of expeditions where no climbers made it
# a few expeditions where all climbers made it
# lots of variability in between
expedition_success <-
  climbers %>%
  group_by(expedition_id) %>%
  summarise(success_rate = mean(success))

expedition_success %>%
  ggplot(aes(x = success_rate)) +
  geom_histogram()

climbers %>%
  group_by(age, oxygen_used) %>%
  summarise(success_rate = mean(success)) %>%
  ggplot(aes(x = age,
             y = success_rate,
             color = oxygen_used)) +
  geom_point()

# simulation time!
climb_model <-
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

# prior spec
climb_model %>% prior_summary()

# mcmc diagnostics
climb_model %>% mcmc_trace()
climb_model %>% mcmc_dens_overlay()
climb_model %>% mcmc_acf()
climb_model %>% neff_ratio()
climb_model %>% rhat()

# define success rate function
success_rate <- function(x){mean(x == 1)}

pp_check(climb_model, 
         nreps = 100,
         plotfun = "stat",
         stat = "success_rate")

# posterior analysis
climb_model %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.80)

climbers %>%
  add_epred_draws(climb_model, ndraws = 100, re_formula = NA) %>%
  ggplot(aes(x = age,
             y = .epred,
             color = oxygen_used,
             group = paste(oxygen_used, .draw))) +
  geom_line(alpha = 0.1) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "probability of success")

# posterior classification of a new expedition
new_expedition <-
  tibble(
    age = c(20, 20, 60, 60),
    oxygen_used = c(FALSE, TRUE, FALSE, TRUE),
    expedition_id = rep("new", 4)
  )

new_expedition

# first let's predict the binary outcome for each
set.seed(84735)
binary_prediction <-
  climb_model %>%
  posterior_predict(newdata = new_expedition)

binary_prediction %>%
  colMeans()
