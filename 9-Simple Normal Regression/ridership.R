library(tidyverse)
library(rstan)
library(tidybayes)
library(broom.mixed)

# prep data
bike_rides <- 
  bayesrules::bikes %>%
  as_tibble()

bike_rides <- 
  bike_rides %>%
  select(temp_feel, rides)

# setup data for stan
bike_data <- 
  list(
    n = nrow(bike_rides),
    Y = bike_rides$rides,
    X = bike_rides$temp_feel
  )

# model
stan_bike_sim <-
  stan(
    file = "9-Simple Normal Regression/ridership.stan",
    model_name = "shindig_model",
    data = bike_data,
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# posterior prediction (manual)
stan_bike_sim %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(mu = beta0 + beta1*75,
         y_new = rnorm(20000, mean = mu, sd = sigma)) %>%
  summarise(lower_mu = quantile(mu, 0.025),
            upper_mu = quantile(mu, 0.975),
            lower_new = quantile(y_new, 0.025),
            upper_new = quantile(y_new, 0.975))

# posterior prediction (rstanarm)
set.seed(84735)
stan_bike_sim %>%
  rstanarm::posterior_predict(newdata = data.frame(temp_feel = 75))
