# libraries
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(broom.mixed)
library(tidybayes)

# load data
data("weather_WU")

weather_WU <- 
  weather_WU %>%
  select(location,
         windspeed9am,
         humidity9am,
         pressure9am,
         temp9am,
         temp3pm)

weather_WU %>%
  ggplot(aes(x = temp9am, y = temp3pm)) +
  geom_point()

weather_model_1 <- 
  stan_glm(
    temp3pm ~ temp9am,
    data = weather_WU,
    family = gaussian(),
    prior_intercept = normal(25, 5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# prior specification
weather_model_1 %>%
  prior_summary()

# interval around each term
weather_model_1 %>%
  posterior_interval(prob = 0.8)

# posterior predictive check - not great!
weather_model_1 %>%
  pp_check()

# categorical-only model
weather_model_2 <- 
  stan_glm(
    temp3pm ~ location,
    data = weather_WU,
    family = gaussian(),
    prior_intercept = normal(25, 5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# diagnostics!
mcmc_trace(weather_model_2)
mcmc_dens_overlay(weather_model_2)
mcmc_acf(weather_model_2)

# posterior summary stats
weather_model_2 %>%
  tidy(effects = c("fixed", "aux"),
       conf.int = TRUE,
       conf.level = 0.8)

weather_model_2 %>%
  as_tibble() %>%
  mutate(uluru = `(Intercept)`,
         wollongong = `(Intercept)` + locationWollongong) %>%
  mcmc_areas(pars = c("uluru", "wollongong"))

# temp + location model --- prior check
weather_model_3_prior <-
  stan_glm(
    temp3pm ~ temp9am + location,
    data = weather_WU,
    family = gaussian(),
    prior_intercept = normal(25, 5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, 
    iter = 5000*2,
    seed = 84735,
    prior_PD = TRUE
  )

# prior pred checks

# prior distribution
weather_WU %>%
  add_predicted_draws(weather_model_3_prior, ndraws = 100) %>%
  ggplot(aes(x = .prediction, 
             group = .draw)) + 
  geom_density() +
  labs(x = "temp3pm")

# prior linear
weather_WU %>%
  add_epred_draws(weather_model_3_prior, ndraws = 100) %>%
  ggplot(aes(x = temp9am,
             y = .epred,
             color = location,
             group = paste(location, .draw))) +
  geom_line(alpha = 0.25)

# update from prior -> posterior
weather_model_3 <-
  weather_model_3_prior %>%
  update(prior_PD = FALSE)

# epred draws (i.e., credible interval around expected model output)
weather_WU %>%
  add_epred_draws(weather_model_3, ndraws = 100) %>%
  ggplot(aes(x = temp9am,
             y = temp3pm,
             color = location)) + 
  geom_line(aes(y = .epred, group = paste(location, .draw)),
            alpha = 0.1)  +
  geom_point(data = weather_WU)

weather_WU %>%
  add_predicted_draws(weather_model_3, ndraws = 10000) %>%
  select(-c(.row, .chain, .iteration)) %>%
  nest(predictions = c(.draw, .prediction)) %>%
  mutate(.pred = map_dbl(predictions, ~quantile(.x$.prediction, probs = 0.5)),
         .pred_lower = map_dbl(predictions, ~quantile(.x$.prediction, probs = 0.025)),
         .pred_upper = map_dbl(predictions, ~quantile(.x$.prediction, probs = 0.975))) %>%
  ggplot(aes(x = temp9am,
             y = .pred,
             fill = location,
             color = location)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
                  alpha = 0.25,
              color = NA) +
  geom_point(data = weather_WU,
             aes(y = temp3pm),
             size = 2.5,
             alpha = 0.5)

# posterior predictions!
set.seed(84735)
temp3pm_prediction <-
  weather_model_3 %>%
  posterior_predict(
    newdata = data.frame(temp9am = c(10, 10),
                         location = c("Uluru", "Wollongong"))
  )

# posterior prediction areas
temp3pm_prediction %>%
  mcmc_areas() +
  scale_y_discrete(labels = c("Uluru", "Wollongong"))

# utilizing interaction terms

# interaction between humidity & location
weather_WU %>%
  ggplot(aes(x = humidity9am,
             y = temp3pm,
             color = location)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              se = FALSE)

interaction_model <-
  stan_glm(
    temp3pm ~ location + humidity9am + location:humidity9am,
    data = weather_WU,
    family = gaussian(),
    prior_intercept = normal(25, 5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

interaction_model %>%
  tidy(effects = c("fixed", "aux"))

interaction_model %>%
  posterior_interval(prob = 0.8)

weather_WU %>%
  add_epred_draws(interaction_model, ndraws = 200) %>%
  ggplot(aes(x = humidity9am,
             y = temp3pm,
             color = location)) +
  geom_line(aes(y = .epred, 
                group = paste(location, .draw)),
            alpha = 0.1)

# interactions not always needed lol
data("bike_users")

bike_users %>%
  as_tibble() %>%
  count(user)

bike_casual <- bike_users %>% filter(user == "casual")
bike_registered <- bike_users %>% filter(user == "registered")

bike_users %>%
  as_tibble() %>%
  ggplot(aes(x = temp_actual,
             y = rides,
             color = weekend)) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~user, scales = "free_y") +
  theme(legend.position = "none")

# more than two predictors!
weather_WU %>%
  names()

weather_model_4 <-
  stan_glm(
    temp3pm ~ .,
    data = weather_WU,
    family = gaussian(),
    prior_intercept = normal(25, 5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

weather_model_4 %>% prior_summary()

# diagnostics
weather_model_4 %>% mcmc_trace()
weather_model_4 %>% mcmc_dens_overlay()
weather_model_4 %>% mcmc_acf()

# parameter intervals
weather_model_4 %>%
  posterior_interval(prob = 0.95)

# are these on a normalized or response scale??

# pp checks
weather_model_1 %>% pp_check()
weather_model_2 %>% pp_check()
weather_model_3 %>% pp_check()
weather_model_4 %>% pp_check()

# evaluating predictive accuracy

# viz
predictions_1 <-
  weather_model_1 %>%
  posterior_predict(newdata = weather_WU)

predictions_2 <-
  weather_model_2 %>%
  posterior_predict(newdata = weather_WU)

predictions_3 <-
  weather_model_3 %>%
  posterior_predict(newdata = weather_WU)

predictions_4 <-
  weather_model_4 %>%
  posterior_predict(newdata = weather_WU)

predictions_4 %>%
  t() %>%
  as_tibble() %>%
  rowid_to_column() %>%
  pivot_longer(starts_with("V"),
               values_to = ".pred") %>%
  select(-name) %>%
  nest(.preds = .pred) %>%
  mutate(.pred = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.5)),
         .pred_lower = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.025)),
         .pred_upper = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.975))) %>%
  bind_cols(weather_WU) %>%
  ggplot(aes(x = temp3pm,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper,
             color = location)) +
  geom_errorbar(size = 1,
                alpha = 0.5) +
  geom_point(size = 2,
             alpha = 0.5) +
  geom_abline(linetype = "dashed")
  
  
  # ggplot(aes(x = temp9am,
  #            color = location)) +
  # geom_point(aes(y = .pred),
  #            size = 2) +
  # geom_errorbar(aes(ymin = .pred_lower,
  #                   ymax = .pred_upper),
  #               alpha = 0.25,
  #               size = 2) +
  # geom_point(aes(y = temp3pm),
  #            size = 2,
  #            color = "midnightblue") 
  
# use cv
set.seed(84735)
prediction_summary_cv(model = weather_model_1,
                      data = weather_WU,
                      k = 10)

# can extend to other models...

# elpd (expected log-predictive densities)
set.seed(84735)
loo_1 <- loo(weather_model_1)
loo_2 <- loo(weather_model_2)
loo_3 <- loo(weather_model_3)
loo_4 <- loo(weather_model_4)

loo_compare(loo_1, loo_2, loo_3, loo_4)

# bias variance tradeoff
set.seed(84735)
weather_shuffle <- 
  weather_australia %>%
  filter(temp3pm < 30, 
         location == "Wollongong") %>%
  sample_n(nrow(.))

sample_1 <- weather_shuffle %>% head(40)
sample_2 <- weather_shuffle %>% tail(40)

sample_2 %>%
  ggplot(aes(x = day_of_year, 
             y = temp3pm)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "blue") +
  geom_smooth(method = "lm",
              se = FALSE,
              formula = y ~ poly(x, 2),
              color = "orange") +
  geom_smooth(method = "lm",
              se = FALSE,
              formula = y ~ poly(x, 12),
              color = "red")




