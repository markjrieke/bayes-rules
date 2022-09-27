# libraries !
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)

# 13.1

# (no code)

# 13.2 

# load data
data("weather_perth")
weather <- 
  weather_perth %>%
  select(day_of_year,
         raintomorrow,
         humidity9am,
         humidity3pm,
         raintoday)

# run a prior simulation
rain_model_prior <-
  stan_glm(
    raintomorrow ~ humidity9am,
    data = weather,
    family = binomial(),
    
    # on an average day, expect a ~ 20% chance of rain
    # but reasonably could range between 6% & 50%
    # pi ~ 0.2, so log(pi/(1-pi)) ~ -1.4
    # logit(0.06) ~ -2.8, logit(0.5) ~ 0
    # -1.4 +/- 2*0.7
    prior_intercept = normal(-1.4, 0.7), 
    
    # on odds scale, we're assuming there's a positive relationship
    # between humidity & rain, but not outside the realm of possibility 
    # that it includes 0
    # 0.7 +/- 2*0.035
    prior = normal(0.07, 0.035),
    chains = 4,
    iter = 5000*2,
    seed = 84735,
    prior_PD = TRUE
  )

# prior models with humidity
set.seed(84735)
weather %>%
  add_epred_draws(rain_model_prior, ndraws = 100) %>%
  ggplot(aes(x = humidity9am,
             y = raintomrrow)) +
  geom_line(aes(y = .epred, 
                group = .draw),
            alpha = 0.25)

# observed proportion of rain in 100 prior datasets
weather %>%
  add_predicted_draws(rain_model_prior, ndraws = 100) %>%
  group_by(.draw) %>%
  summarise(proportion_rain = mean(.prediction == 1)) %>%
  ggplot(aes(x = proportion_rain)) +
  geom_histogram(color = "white")

# 13.3 simulating the posterior

# more weather plots
weather %>%
  ggplot(aes(x = humidity9am,
             y = raintomorrow)) +
  geom_jitter(alpha = 0.25)

weather %>%
  mutate(humidity_bracket = cut(humidity9am, 
                                breaks = seq(10, 100, by = 10))) %>%
  group_by(humidity_bracket) %>%
  summarise(rain_rate = mean(raintomorrow == "Yes")) %>%
  ggplot(aes(x = humidity_bracket,
             y = rain_rate)) +
  geom_point(size = 3)

# simulate the model
rain_model_1 <- update(rain_model_prior, prior_PD = FALSE)

# diagnostics!
rain_model_1 %>% mcmc_trace()
rain_model_1 %>% mcmc_dens_overlay()
rain_model_1 %>% mcmc_acf()

# posterior check
weather %>%
  add_epred_draws(rain_model_1, ndraws = 100) %>%
  ggplot(aes(x = humidity9am,
             y = raintomrrow)) +
  geom_line(aes(y = .epred,
                group = .draw),
            alpha = 0.25)

# posterior summarise on the log(odds) scale
posterior_interval(rain_model_1, prob = 0.8)

# posterior summaries on the odds scale
posterior_interval(rain_model_1, prob = 0.8) %>% exp()

# 13.4 prediction and classification

# posterior prediction of binary outcome
set.seed(84735)
binary_prediction <-
  posterior_predict(rain_model_1, newdata = data.frame(humidity9am = 99))

rain_model_1 %>%
  as_tibble() %>%
  mutate(log_odds = `(Intercept)` + humidity9am*99,
         odds = exp(log_odds),
         prob = odds/(1+odds),
         y = rbinom(20000, size = 1, prob = prob)) %>%
  ggplot(aes(x = y)) +
  geom_histogram()

# 13.5 model evaluation

# custom function for pp_check
proportion_rain <- function(x) {mean(x == 1)}

# pp_check
rain_model_1 %>%
  pp_check(nreps = 100,
           plotfun = "stat",
           stat = "proportion_rain")

# posterior predictive models for each day in the dataset
set.seed(84735)
rain_pred_1 <-
  rain_model_1 %>%
  posterior_predict(newdata = weather)

weather_classification <-
  weather %>%
  mutate(rain_prob = colMeans(rain_pred_1),
         rain_class_1 = as.numeric(rain_prob >= 0.5)) %>%
  select(humidity9am,
         rain_prob,
         rain_class_1, 
         raintomorrow)

# confusion matrix
weather_classification %>%
  mutate(rain_class_1 = if_else(rain_class_1 == 1, "Yes", "No"),
         rain_class_1 = as.factor(rain_class_1)) %>%
  yardstick::conf_mat(truth = raintomorrow,
                      estimate = rain_class_1)

# basically just classifies everything as "no" when the cutoff is 0.5
set.seed(84735) 
classification_summary(model = rain_model_1, data = weather)

# can change cutoff to 0.2
set.seed(84735)
classification_summary(model = rain_model_1, data = weather, cutoff = 0.2)

# 13.6 extending the model

# include more predictors
rain_model_2 <-
  stan_glm(
    raintomorrow ~ humidity9am + humidity3pm + raintoday,
    data = weather,
    family = binomial(),
    prior_intercept = normal(-1.4, 0.7),
    prior = normal(0, 2.5, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

prior_summary(rain_model_2)

# posterior summary
rain_model_2 %>%
  tidy(effects = "fixed", conf.int = TRUE, conf.level = 0.8)

# which is the better model?
# (using cutoff of 0.2)
set.seed(84735)
cv_accuracy_2 <- 
  rain_model_2 %>%
  classification_summary_cv(data = weather, cutoff = 0.2, k = 10)

cv_accuracy_2$cv
