# libraries!
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)

# data!
data("pulse_of_the_nation")
pulse <-
  pulse_of_the_nation %>%
  filter(books < 100)

# eda!
pulse %>%
  ggplot(aes(x = books)) +
  geom_histogram(binwidth = 10) 

pulse %>%
  ggplot(aes(x = age,
             y = books)) +
  geom_point()

pulse %>%
  ggplot(aes(x = wise_unwise,
             y = books)) +
  geom_boxplot()

# poisson model (not great)
books_poisson_sim <-
  stan_glm(
    books ~ age + wise_unwise,
    data = pulse,
    family = poisson(),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# hmmmm doesn't capture it super well
books_poisson_sim %>%
  pp_check()

# poisson model assumes equal mean/variance
# data is unequal variance!!
pulse %>%
  summarise(mean = mean(books),
            var = var(books))

# negative binomial model allows for modeling overdispersion
books_negbin_sim <-
  stan_glm(
    books ~ age + wise_unwise,
    data = pulse,
    family = neg_binomial_2(),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 84735
  )

# much better!
books_negbin_sim %>%
  pp_check() +
  xlim(0, 75)

books_negbin_sim %>%
  tidy(conf.int = TRUE,
       conf.level = 0.8)
