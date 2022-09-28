# libraries !
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(broom.mixed)

# data !
data("cherry_blossom_sample")
running <- 
  cherry_blossom_sample %>%
  select(runner, age, net)

# how do different runner's times compare
running %>%
  ggplot(aes(x = runner,
             y = net)) +
  geom_boxplot() +
  coord_flip()

# 15.1 - Complete pooling

# complete pooling says "all runs are independent" 
# (combine all 252 observations across 36 runners into one pool of data)
running %>%
  ggplot(aes(x = age,
             y = net)) +
  geom_point()

complete_pooled_model <-
  stan_glm(
    net ~ age,
    data = running,
    family = gaussian(),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# posterior summary stats
complete_pooled_model %>%
  tidy(conf.int = TRUE, conf.level = 0.8)

# 80% conf on age could be negative or positive !
running %>% 
  ggplot(aes(x = age, 
             y = net,
             group = runner)) +
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "gray") +
  geom_abline(aes(intercept = 75.2, 
                  slope = 0.268),
              color = "blue",
              size = 2)

# individual runners have individually positive relationships!
# but when everything is pooled, this peters out

# 15.2 - No pooling

# no pooling - consider each runner completely separately
set.seed(1)
running %>%
  nest(data = -runner) %>%
  slice_sample(n = 3) %>%
  unnest(data) %>%
  ggplot(aes(x = age,
             y = net)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE) +
  facet_wrap(~runner, ncol = 1)

# this seems great at first glance!
# but what do we expect runner 1's time to be at 65 years old?
# according to the no-pooled model, they'd keep getting faster! wack!

# 15.3 - Hierarchical data

# within-group variablity (for example, how consistent an individual's times are)
# between-group variability (for example, the degree times vary between individuals)

# 15.4 - Partial pooling with hierarchical models

