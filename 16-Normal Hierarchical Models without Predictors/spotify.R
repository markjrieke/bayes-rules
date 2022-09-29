# libraries !
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)

# data !
data("spotify")
spotify <- 
  spotify %>%
  select(artist, title, popularity) %>%
  mutate(artist = fct_reorder(artist, popularity, .fun = "mean"))

# hierarchicy !!!
nlevels(spotify$artist)

# if we only look at means, we lose some information
artist_means <-
  spotify %>%
  group_by(artist) %>%
  summarise(count = n(),
            popularity = mean(popularity))

artist_means %>%
  slice(1:2, 43:44)

# 16.1 - Complete pooled model

# average of abt 50
spotify %>%
  ggplot(aes(x = popularity)) +
  geom_density()

# intercept-only model (full pool)
spotify_complete_pooled <-
  stan_glm(
    popularity ~ 1,
    data = spotify,
    family = gaussian(),
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# prior specifications
prior_summary(spotify_complete_pooled)

# posterior summary
spotify_complete_pooled %>%
  tidy(effects = c("fixed", "aux"),
       conf.int = TRUE,
       conf.level = 0.8)

# posterior predictions!
set.seed(84735)
predictions_complete <- 
  spotify_complete_pooled %>%
  posterior_predict(newdata = artist_means)

# complete pooled model ignores artist-specific data!!!
ppc_intervals(artist_means$popularity, 
              yrep = predictions_complete,
              prob_outer = 0.8) +
  scale_x_continuous(labels = artist_means$artist,
                     breaks = 1:nrow(artist_means)) +
  coord_flip()

# 16.2 - No pooled model

# look in at these within-artist differences
spotify %>%
  ggplot(aes(x = popularity,
             group = artist)) +
  geom_density()

# let's generate a no pooled model 
# (really it's a no-pooled model for mean, variation is set globally here for simplicity)
# using popularity ~ artist - 1 removes a global intercept
spotify_no_pooled <- 
  stan_glm(
    popularity ~ artist - 1,
    data = spotify,
    family = gaussian(),
    prior = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# posterior predictive models
set.seed(84735)
predictions_no <-
  spotify_no_pooled %>%
  posterior_predict(newdata = artist_means)

# correctly capturing the artist-specific data
# BUT, ignores data on one artist when learning about popularity of another!
ppc_intervals(artist_means$popularity,
              yrep = predictions_no,
              prob_outer = 0.8) +
  scale_x_continuous(labels = artist_means$artist,
                     breaks = 1:nrow(artist_means)) +
  coord_flip()

# for example, Mia X's low score is based on only 3 data points 
# also cannot be extended to artists outside the sample

# 16.3 - Building the hierarchical model

# Yij | uj, sy -> individual artist/song popularity given by individual mean, global within-group variation
# uj | u, su -> individual mean given by global mean, global variance

# Yij | uj, xy ~ N(uj, sy^2) model of individual observations within group i
# uj | u, su ~ N(u, su^2) model of how parameters vary between groups
# u ~ N(m, s^2) prior models on global parameters
# sy ~ exp(ly)
# su ~ exp(lu)

# Var (Yij) = sy^2 + su^2

# 16.4 - Posterior analysis

spotify_hierarchical <-
  stan_glmer(
    popularity ~ (1 | artist), # varying intercepts by artist
    data = spotify,
    family = gaussian(),
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1), # more to come here in ch 17
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# diagnostics
spotify_hierarchical %>% mcmc_trace()
spotify_hierarchical %>% mcmc_dens_overlay()
spotify_hierarchical %>% mcmc_acf()
spotify_hierarchical %>% neff_ratio() 
spotify_hierarchical %>% rhat()
spotify_hierarchical %>% pp_check()

# dig into the posterior results 
spotify_hierarchical_df <-
  spotify_hierarchical %>% 
  as_tibble()

# 16.4.2 - Posterior analysis of global parameters

# u = (Intercept)
# sy = sigma
# su^2 = Sigma[artist:(Intercept),(Intercept)]

# only one fixed effect
spotify_hierarchical %>%
  tidy(effects = "fixed",
       conf.int = TRUE,
       conf.level = 0.80)

# posterior info for random parameters, ran_pars
# within any artist, popularity ratings tend to vary 14 pts from song to song
# the standard deviation of artist means is about 15.2
spotify_hierarchical %>%
  tidy(effects = "ran_pars",
       conf.int = TRUE,
       conf.level = 0.80)

# 54% of the variability in song population is explained by differences between artists
# 46% is explained by differences among songs within each artist
15.1^2 / (15.1^2 + 14.0^2)

# 16.4.3 - Posterior analysis of group-specific parameters

# tidy with all the random (artist-specific) values
artist_summary <-
  spotify_hierarchical %>%
  tidy(effects = "ran_vals",
       conf.int = TRUE,
       conf.level = 0.80)

artist_summary %>%
  select(level, conf.low, conf.high) %>%
  slice(1:2, 43:44)

# uj = u + bj = (Intercept) + b[(Intercept) artist:j]
artist_chains <- 
  spotify_hierarchical %>%
  spread_draws(`(Intercept)`, b[,artist]) %>%
  mutate(mu_j = `(Intercept)` + b)

artist_chains %>%
  select(artist, `(Intercept)`, b, mu_j) %>% 
  head(4)

# mean_qi() produces posterior summary for each artist's mean popularity muj
artist_summary_scaled <-
  artist_chains %>%
  select(-`(Intercept)`, -b) %>%
  mean_qi(.width = 0.80) %>%
  mutate(artist = fct_reorder(artist, mu_j))

# look at that! some artists have drastically different posterior intervals
# even if they have similar posterior means
artist_summary_scaled %>%
  select(artist, mu_j, .lower, .upper)

artist_summary_scaled %>%
  ggplot(aes(x = artist,
             y = mu_j,
             ymin = .lower,
             ymax = .upper)) +
  geom_pointrange() +
  coord_flip()

# 16.5 - Posterior Prediction

# frank ocean's prior predictive model
set.seed(84735)
ocean_chains <- 
  spotify_hierarchical_df %>%
  rename(b = `b[(Intercept) artist:Frank_Ocean]`) %>%
  select(`(Intercept)`, b, sigma) %>%
  mutate(mu_ocean = `(Intercept)` + b,
         y_ocean = rnorm(20000, mean = mu_ocean, sd = sigma))

head(ocean_chains, 3)

# posterior summary of frank ocean - any song (not mean!)
# 80% chance frank ocean's next song is between 51.3 & 57.6
ocean_chains %>%
  mean_qi(y_ocean, .width = 0.80)

# posterior summary of franke ocean mu parameter
# 80% chance frank ocean's average song pop is between 66.6 & 72.2
artist_summary_scaled %>%
  filter(artist == "artist:Frank_Ocean")

# let's create an estimate of a new artist, mohsen beats
set.seed(84735)
mohsen_chains <-
  spotify_hierarchical_df %>%
  mutate(sigma_mu = sqrt(`Sigma[artist:(Intercept),(Intercept)]`), # get the global variance from our chains
         mu_mohsen = rnorm(20000, `(Intercept)`, sigma_mu), # generate 20k possible mu values for mohsen
         y_mohsen = rnorm(20000, mu_mohsen, sigma)) # generate 20k possible mohsen popularities

# posterior predictive summaries
mohsen_chains %>%
  mean_qi(y_mohsen, .width = 0.80)

# now let's do this using posterior predictions
set.seed(84735)
prediction_shortcut <-
  spotify_hierarchical %>%
  posterior_predict(
    newdata = data.frame(artist = c("Frank Ocean", "Mohsen Beats"))
  )

prediction_shortcut %>%
  mcmc_areas(prob = 0.8) +
  scale_y_discrete(labels = c("Frank Ocean", "Mohsen Beats"))

# 16.6 - Shrinkage & the bias-variance trade-off

# let's construct posterior predictive models for all 44 artists
set.seed(84735)
predictions_hierarchical <-
  spotify_hierarchical %>% 
  posterior_predict(newdata = artist_means)

# posterior predictive plots
ppc_intervals(artist_means$popularity,
              yrep = predictions_hierarchical,
              prob_outer = 0.80) +
  
  scale_x_continuous(labels = artist_means$artist,
                     breaks = 1:nrow(artist_means)) +
  coord_flip()

# 16.7 - Not everything is hierarchical

# when all categories of interest are covered, it's likely that the 
# variable is categorical rather than grouping

# for example, with ridership data, day-of-week covers all possible days, 
# so can use as a categorical predictor, but each rider gives us information about
# that particular rider + riders as a whole!

