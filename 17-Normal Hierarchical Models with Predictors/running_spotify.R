# libraries !
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)

# data !
data("cherry_blossom_sample")
running <- cherry_blossom_sample

running <- 
  running %>%
  select(runner, age, net) %>%
  drop_na()

# 17.1 - First steps: Complete pooling

# see 15.1 for completely pooled model

# 17.2 - Hierarchical model with varying intercepts

# typical runner in our age group sample has somewhere between 80/120 min time B0c ~ N(100, 10^2)
# think this will increase with age, maybe something between 0.5/4.5 min/year B1 ~ N(2.5, 1^2)
# not super clear understanding of variability between runners (sigma0) or within runners (sigmay), so use weak priors

running_model_1_prior <-
  stan_glmer(
    net ~ age + (1 | runner),
    data = running,
    family = gaussian(),
    prior_intercept = normal(100, 10),
    prior = normal(2.5, 1),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735,
    prior_PD = TRUE
  )

# prior predictive checks
set.seed(84735)
running %>%
  add_epred_draws(running_model_1_prior, ndraws = 4) %>%
  ggplot(aes(x = age,
             y = net)) +
  geom_line(aes(y = .epred,
                group = paste(runner, .draw)),
            size = 1, 
            alpha = 0.25) +
  facet_wrap(~.draw)

running %>%
  add_predicted_draws(running_model_1_prior, ndraws = 100) %>%
  ggplot(aes(x = .prediction,
             group = .draw)) +
  geom_density(color = NA,
               fill = "midnightblue",
               alpha = 0.0625) +
  labs(x = "net")

# posterior simulation & analysis!

running_model_1 <- 
  running_model_1_prior %>%
  update(prior_PD = FALSE)

# (Intercept) = B0 global intercept
# age = B1 global age parameter
# b[(Intercept) runner:j] = b0j = B0j - B0 difference between runner j's intercept & global intercept
# sigma = sigmay global within-runner variability
# Sigma[runner:(Intercept),(Intercept)] = sigma0^2 between runner intercept variability (variability of B0j)

tidy_summary_1 <- 
  running_model_1 %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.8)

B0 <- tidy_summary_1$estimate[1]
B1 <- tidy_summary_1$estimate[2]

# global running time model
running %>%
  add_epred_draws(running_model_1, ndraws = 200, re_formula = NA) %>%
  ggplot(aes(x = age, 
             y = .epred,
             group = .draw)) +
  geom_line(alpha = 0.25) +
  geom_abline(intercept = B0,
              slope = B1,
              size = 1,
              color = "blue") +
  labs(y = "net")

# posterior summaries of runner-specific intercepts
runner_summaries_1 <- 
  running_model_1 %>%
  spread_draws(`(Intercept)`, b[,runner]) %>%
  mutate(runner_intercept = `(Intercept)` + b) %>%
  select(-`(Intercept)`, -b) %>%
  median_qi(.width = 0.8) %>%
  select(runner, runner_intercept, .lower, .upper)

# let's look at 4/5 specifically
runner_summaries_1 %>%
  filter(runner %in% c("runner:4", "runner:5"))

running %>%
  filter(runner %in% c("4", "5")) %>%
  add_epred_draws(running_model_1, ndraws = 100) %>%
  ggplot(aes(x = age,
             y = .epred,
             group = paste(runner, .draw),
             color = runner)) +
  geom_line(alpha = 0.25) +
  scale_color_brewer(palette = "Dark2") +
  geom_point(aes(y = net),
             size = 3)

# now let's look at the global model & each runner's model
running %>%
  ggplot(aes(x = age,
             y = net, group = runner)) +
  geom_abline(data = runner_summaries_1,
              aes(intercept = runner_intercept, slope = B1),
              color = "gray",
              size = 1) +
  geom_abline(intercept = B0, 
              slope = B1, 
              color = "blue",
              size = 2) +
  expand_limits(x = c(50, 61),
                y = c(50, 135))

# how does the variance differ within and between groups?
tidy_sigma <- 
  running_model_1 %>%
  tidy(effects = "ran_pars")

# greater variability between runners than within!
tidy_sigma

# 17.3 - Hierarchical model with varying intercepts and slopes

# same slope for each runner isn't necessarily good!
set.seed(666)
running %>%
  filter(runner %in% c("4", "5", "20", "29")) %>%
  add_epred_draws(running_model_1, ndraws = 100) %>%
  ggplot(aes(x = age,
             y = .epred,
             group = paste(runner, .draw),
             color = runner)) +
  geom_line(alpha = 0.25) +
  geom_point(aes(y = net),
             size = 3) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~runner) +
  theme(legend.position = "none")

# look at 17.3 for more info on the decov() priors

running_model_2 <- 
  stan_glmer(
    net ~ age + (age | runner),
    data = running,
    family = gaussian(),
    prior_intercept = normal(100, 10),
    prior = normal(2.5, 1),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735,
    adapt_delta = 0.99999
  )

# quick summmary of global regression parameters
running_model_2 %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.80)

# get mcmc chains for runner-specific intercepts & slopes
runner_chains_2 <- 
  running_model_2 %>%
  spread_draws(`(Intercept)`, b[term, runner], `age`) %>%
  pivot_wider(names_from = term,
              names_glue = "b_{term}",
              values_from = b) %>%
  mutate(runner_intercept = `(Intercept)` + `b_(Intercept)`,
         runner_age = age + b_age)

# get posterior medians of each runner-specific intercept and age coefficient
runner_summaries_2 <- 
  runner_chains_2 %>%
  group_by(runner) %>%
  summarise(runner_intercept = median(runner_intercept),
            runner_intercept_lower = quantile(runner_intercept, probs = 0.1),
            runner_intercept_upper = quantile(runner_intercept, probs = 0.9),
            runner_age = median(runner_age),
            runner_age_lower = quantile(runner_age, probs = 0.1),
            runner_age_upper = quantile(runner_age, probs = 0.9))

# posterior median models for all 36 runners
running %>%
  ggplot(aes(x = age,
             y = net,
             group = runner)) +
  geom_abline(data = runner_summaries_2, 
              color = "gray",
              size = 1,
              alpha = 0.75,
              aes(intercept = runner_intercept, 
                  slope = runner_age)) +
  expand_limits(x = c(50, 61),
                y = c(50, 135))

# 17.4 - Model evaluation and selection

# hmmm these look similar - how to discern?
pp_check(running_model_1)
pp_check(running_model_2)

# prediction summaries
set.seed(84735)
prediction_summary(running_model_1, data = running)
prediction_summary(running_model_2, data = running)

# cv prediction summaries
prediction_summary_cv(running_model_1, data = running, k = 10, group = "runner")

# expected log predictive densities
elpd_1 <- loo(running_model_1)
elpd_2 <- loo(running_model_2)

# comparison shows 2 is better - but is this difference significant? (it's within 2 std. dev)
loo_compare(elpd_1, elpd_2)

# 17.5 - Posterior prediction
running %>%
  filter(runner %in% c("1", "10")) %>%
  ggplot(aes(x = age,
             y = net)) +
  geom_point(size = 3) +
  facet_wrap(~runner)

set.seed(84735)
model_1_pred <-
  running_model_1 %>%
  posterior_predict(newdata = data.frame(runner = c("1", "Miles", "10"),
                                         age = c(61, 61, 61)))

mcmc_areas(model_1_pred) +
  scale_y_discrete(labels = c("runner 1", "Miles", "runner 10"))

set.seed(84735)
model_2_pred <-
  running_model_2 %>%
  posterior_predict(newdata = data.frame(runner = c("1", "Miles", "10"),
                                         age = c(61, 61, 61)))

mcmc_areas(model_2_pred) +
  scale_y_discrete(labels = c("runner 1", "Miles", "runner 10"))

# 17.7 - Example: Danceability (!)

# more data !
data("spotify")
spotify <-
  spotify %>%
  select(artist, title, danceability, valence, genre)

# some lite eda
spotify %>%
  mutate(genre = fct_reorder(genre, danceability)) %>%
  ggplot(aes(x = genre, 
             y = danceability)) +
  geom_boxplot() +
  coord_flip()

spotify %>%
  ggplot(aes(x = valence,
             y = danceability)) +
  geom_point(size = 2.5,
             alpha = 0.25)

spotify %>%
  ggplot(aes(x = valence,
             y = danceability,
             group = artist)) +
  geom_smooth(method = "lm", se = FALSE)

# model 1 - intercept only
spotify_model_1 <- 
  stan_glmer(
    danceability ~ valence + genre + (1 | artist),
    data = spotify, 
    family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE), 
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4, 
    iter = 5000*2, 
    seed = 84735
  )

# model 2 - slope intercept
spotify_model_2 <-
  stan_glmer(
    danceability ~ valence + genre + (valence | artist),
    data = spotify,
    family = gaussian(),
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# at first glance, they look similar
spotify_model_1 %>% pp_check()
spotify_model_2 %>% pp_check()

elpd_1 <- loo(spotify_model_1)
elpd_2 <- loo(spotify_model_2)

# comparing loos says model 2 is better,
# but within 2 sd of model 1
loo_compare(elpd_1, elpd_2)

# model 1 is far simpler, so let's look at that
spotify_model_1 %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.80)

# posterior models of genre coefficients
spotify_model_1 %>%
  mcmc_areas(pars = vars(starts_with("genre")),
             prob = 0.8) +
  geom_vline(xintercept = 0)

spotify_model_1 %>%
  tidy(effects = "ran_vals",
       conf.int = TRUE,
       conf.level = 0.80) %>%
  filter(level %in% c("Camilo", "Missy_Elliott")) %>%
  select(level, estimate, starts_with("conf"))

# how about predicting new song danceability?
set.seed(84735)
spotify_model_1 %>%
  posterior_predict(newdata = tibble(artist = c("Camilo", "Mohsen Beats", "Missy Elliott"),
                                     valence = c(80, 60, 90),
                                     genre = c("latin", "rock", "rap"))) %>%
  mcmc_areas(prob = 0.8) +
  scale_y_discrete(labels = c("Camilo", "Mohsen Beats", "Missy Elliott"))

# this model is a bit flawed in that the range is 0-100 but the normal model 
# produces responses outside of that range!



