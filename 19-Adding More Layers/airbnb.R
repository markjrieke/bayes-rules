# libraries !
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(janitor)
library(tidybayes)
library(broom.mixed)

# 19.1 Group-level predictors

# data!
data("airbnb")

# some info about neighborhood/price
airbnb %>%
  as_tibble() %>%
  summarise(nlevels(neighborhood),
            min(price),
            max(price))

# skewed prices! 
airbnb %>%
  ggplot(aes(x = price)) +
  geom_histogram(color = "white", breaks = seq(0, 500, by = 20))

# not when we're dealing w/logged prices
airbnb %>%
  ggplot(aes(x = log(price))) +
  geom_histogram(color = "white", binwidth = 0.5)

# some eda
airbnb %>%
  ggplot(aes(x = bedrooms,
             y = log(price))) +
  geom_jitter(alpha = 0.25)

airbnb %>%
  ggplot(aes(x = rating,
             y = log(price))) +
  geom_jitter(alpha = 0.25)

airbnb %>%
  ggplot(aes(x = room_type,
             y = log(price))) +
  geom_boxplot()

# look at the neighborhood hierarchy!
airbnb %>%
  mutate(neighborhood = fct_reorder(neighborhood, log(price))) %>%
  ggplot(aes(x = neighborhood,
             y = log(price))) +
  geom_boxplot() +
  coord_flip()

# log model, neighborhood hierarchaies
airbnb_model_1 <-
  stan_glmer(
    log(price) ~ bedrooms + rating + room_type + (1 | neighborhood),
    data = airbnb,
    family = gaussian(),
    prior_intercept = normal(4.6, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

prior_summary(airbnb_model_1)

pp_check(airbnb_model_1)

# there's also group-level predictors to consider
# for example, all listings in Albany Park have a walk score of 87 and a transit score of 62
airbnb %>%
  select(price, neighborhood, walk_score, transit_score) %>%
  slice_head(n = 3)

# ignoring these group-level predictors means we're missing out on add'l info:
airbnb %>%
  group_by(neighborhood, walk_score) %>%
  summarise(mean_log_price = mean(log(price)),
            n_listings = n()) %>%
  ggplot(aes(x = walk_score,
             y = mean_log_price,
             size = n_listings)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", 
              se = FALSE)

# stan will recognize that all the listings in the neighborhood share the same 
# walk_score and list it as a group-level predictor, so the code is only changed
# a little
airbnb_model_2 <-
  stan_glmer(
    log(price) ~ walk_score + bedrooms + rating + room_type + (1 | neighborhood),
    data = airbnb,
    family = gaussian(),
    prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

prior_summary(airbnb_model_2)

# comparison between global median models
model_1_mean <- tidy(airbnb_model_1, effects = "fixed")
model_2_mean <- tidy(airbnb_model_2, effects = "fixed")

combined_summaries <- 
  model_2_mean %>%
  left_join(model_1_mean, 
            by = "term",
            suffix = c("_model_2", "_model_1")) %>%
  select(-starts_with("std"))

# all predictors (except intercept) are the same --- this makes sense!
# walkability essentially replaces the original global intercept (b0) without tweaking
# the individual listing parameters
combined_summaries

model_1_var <- tidy(airbnb_model_1, effects = "ran_pars")
model_2_var <- tidy(airbnb_model_2, effects = "ran_pars")

# the variance around the individual listings is unchanged but lower var abt neighborhood
# this also makes sense! we added information about the neighborhoods
# but not individual listings within neighborhoods 
model_2_var %>%
  left_join(model_1_var, 
            by = "term",
            suffix = c("_model_2", "_model_1")) %>%
  select(-starts_with("group"))

            