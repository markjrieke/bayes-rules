# libraries !
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)

# 18.2 - Hierarchical Poisson and Negative Binomial Regression

# data !
data("airbnb")

# 43 neighborhoods in the dataset
airbnb %>%
  summarise(nlevels(neighborhood))

# some eda
airbnb %>%
  ggplot(aes(x = reviews)) +
  geom_histogram(breaks = seq(0, 200, by = 10),
                 color = "white")

airbnb %>%
  ggplot(aes(x = as.character(rating),
             y = reviews)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.15)

airbnb %>%
  ggplot(aes(x = room_type,
             y = reviews)) +
  geom_violin()

# what about within neighborhoods
airbnb %>%
  filter(neighborhood %in% c("Albany Park", "East Garfield Park", "The Loop")) %>%
  ggplot(aes(x = rating,
             y = reviews,
             color = room_type)) +
  geom_jitter() +
  facet_wrap(~neighborhood, ncol = 1) +
  scale_color_brewer(palette = "Dark2")

# intercept-only poisson model
air_bnb_model_1 <-
  stan_glmer(
    reviews ~ rating + room_type + (1 | neighborhood),
    data = airbnb,
    family = poisson(),
    prior_intercept = normal(3, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4, 
    iter = 5000*2,
    seed = 84735
  )

# posterior check - doesn't capture overdispersion well!
air_bnb_model_1 %>%
  pp_check()

# swap to intercept-only negative binomial model
air_bnb_model_2 <-
  stan_glmer(
    reviews ~ rating + room_type + (1 | neighborhood),
    data = airbnb,
    family = neg_binomial_2(),
    prior_intercept = normal(3, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4, 
    iter = 5000*2,
    seed = 84735
  )

# much better!
air_bnb_model_2 %>%
  pp_check() +
  xlim(0, 200)

# posterior analysis
air_bnb_model_2 %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.80)

air_bnb_model_2 %>%
  tidy(effects = "ran_vals",
       conf.int = TRUE,
       conf.level = 0.80) %>%
  select(level, estimate, starts_with("conf")) %>%
  filter(level %in% c("Albany_Park", "East_Garfield_Park", "The_Loop"))

# posterior predictions of # of reviews
set.seed(84735)
predicted_reviews <-
  posterior_predict(
    air_bnb_model_2,
    newdata = tibble(
      rating = rep(5, 3),
      room_type = rep("Entire home/apt", 3),
      neighborhood = c("Albany Park", "East Garfield Park", "The Loop"))
  ) 

predicted_reviews %>%
  mcmc_areas(prob = 0.8) +
  scale_y_discrete(labels = c("Albany Park", "East Garfield Park", "The Loop")) +
  xlim(0, 150)
