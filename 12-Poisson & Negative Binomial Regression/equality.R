# libraries!
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)

# data!
data("equality_index")
equality <- equality_index

equality %>%
  ggplot(aes(x = laws)) +
  geom_histogram()

# california is a hefty outlier:
equality %>%
  filter(state != "california") %>%
  ggplot(aes(x = laws)) +
  geom_histogram()

equality <-
  equality %>%
  filter(state != "california")

# urbanicity vs # laws, colored by historical
equality %>%
  ggplot(aes(x = percent_urban,
             y = laws,
             color = historical)) +
  geom_point(size = 3, 
             alpha = 0.75) +
  scale_color_manual(values = c("blue", "red", "purple"))

# bayesian normal model (bad)
equality_normal_sim <-
  stan_glm(
    laws ~ percent_urban + historical,
    data = equality,
    family = gaussian(),
    prior_intercept = normal(7, 1.5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, 
    iter = 5000*2,
    seed = 84735
  )

# posterior predictive check
equality_normal_sim %>%
  pp_check(plotfun = "hist", 
           nreps = 5) +
  geom_vline(xintercept = 0,
             linetype = "dashed")

# bayesian poisson model
equality_model_prior <-
  stan_glm(
    laws ~ percent_urban + historical,
    data = equality,
    family = poisson(),
    # typical # of laws is 7, but could be 3-20
    # 2 +/- 2*0.5 = 1, 3
    # (e^1, e^3) ~= (3, 20)
    # priors are on log scale because of poisson mod 
    prior_intercept = normal(2, 0.5), 
    prior = normal(0, 2.5, autoscale = TRUE),
    chains = 4, 
    iter = 5000*2,
    seed = 84735,
    prior_PD = TRUE
  )

# prior predictive checks!
prior_summary(equality_model_prior)

equality %>%
  add_epred_draws(equality_model_prior, ndraws = 100) %>%
  ggplot(aes(x = percent_urban,
             y = laws,
             color = historical)) +
  geom_line(aes(y = .epred,
                group = paste(historical, .draw)),
            size = 1,
            alpha = 0.25) +
  ylim(0, 100) +
  facet_wrap(~historical, ncol = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("blue", "red", "purple"))

# simulate the posterior
equality_model <-
  equality_model_prior %>%
  update(prior_PD = FALSE)

# diagnostics
mcmc_trace(equality_model)
mcmc_dens_overlay(equality_model)
mcmc_acf(equality_model)

# posterior predictive check
set.seed(1)
equality_model %>% pp_check(plotfun = "hist", nreps = 5)
equality_model %>% pp_check()

equality %>%
  add_epred_draws(equality_model, ndraws = 50) %>%
  ggplot(aes(x = percent_urban,
             y = .epred,
             color = historical)) +
  geom_line(aes(group = paste(historical, .draw)),
            size = 1,
            alpha = 0.25) +
  geom_point(data = equality,
             aes(x = percent_urban,
                 y = laws),
             size = 3,
             alpha = 0.75) +
  scale_color_manual(values = c("blue", "red", "purple"))

# interpreting the posterior
equality_model %>%
  tidy(conf.int = TRUE, 
       conf.level = 0.80)

# posterior prediction
set.seed(84735) 
equality %>%
  filter(state == "minnesota") %>%
  posterior_predict(equality_model, newdata = .) %>%
  as_tibble() %>%
  rename(minnesota = `1`) %>%
  ggplot(aes(x = minnesota)) +
  geom_histogram(binwidth = 1,
                 color = "white")

# model evaluation
set.seed(84735)
equality_model %>%
  posterior_predict(newdata = equality) %>%
  t() %>%
  as_tibble() %>%
  rowid_to_column() %>%
  pivot_longer(starts_with("V"),
               values_to = ".pred") %>%
  select(-name) %>%
  nest(.preds = .pred) %>%
  mutate(.pred = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.5)),
         .pred_lower = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.1)),
         .pred_upper = map_dbl(.preds, ~quantile(.x$.pred, probs = 0.9))) %>%
  bind_cols(equality) %>%
  ggplot(aes(x = percent_urban)) +
  geom_point(aes(y = laws,
                 color = historical)) +
  geom_line(aes(y = .pred,
                color = historical),
            size = 1) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper,
                  fill = historical),
              alpha = 0.25) +
  facet_wrap(~historical, 
             ncol = 1) +
  scale_color_manual(values = c("blue", "red", "purple")) +
  scale_fill_manual(values = c("blue", "red", "purple"))

