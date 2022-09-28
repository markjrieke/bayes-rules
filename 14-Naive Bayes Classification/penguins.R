# libraries !
library(bayesrules)
library(tidyverse)
library(e1071)
library(janitor)

# data !
data("penguins_bayes")
penguins <- penguins_bayes

# prop of species
penguins %>%
  tabyl(species)

# 14.1 - Classifiying one penguine

# 14.1.1 - One categorical predictor

# prop of above average weight by species
penguins %>%
  drop_na(above_average_weight) %>%
  ggplot(aes(fill = above_average_weight,
             x = species)) +
  geom_bar(position = "fill")

# gentoo is mostly above average weight
# chinstrap is lowest by above avg weight, but also the rarest species!
penguins %>%
  select(species, above_average_weight) %>%
  drop_na() %>%
  tabyl(species, above_average_weight) %>%
  adorn_totals(c("row", "col"))

# p(penguin == Adelie | above_average_weight == 0) = 126/193 = 0.6528

# 14.1.2 - One quantitative predictor

# which species is a 50mm-long bill the most common?
penguins %>%
  ggplot(aes(x = bill_length_mm,
             fill = species)) +
  geom_density(alpha = 0.25) +
  geom_vline(xintercept = 50,
             linetype = "dashed")

# naive:
# assume that any quantitative predictor is continuous and conditionally normal
# i.e., within each species, bill lengths are normally distributed
#       possibly with different means (mu) & std deviations (sigma)

# what are the sample means/sds
penguins %>%
  group_by(species) %>%
  summarise(mean = mean(bill_length_mm, na.rm = TRUE),
            sd = sd(bill_length_mm, na.rm = TRUE))

# this is a bit more idealistic but still works for our purposes!
penguins %>%
  ggplot(aes(x = bill_length_mm,
             color = species)) +
  stat_function(fun = dnorm,
                args = list(mean = 38.8, sd = 2.66),
                aes(color = "Adelie")) +
  stat_function(fun = dnorm,
                args = list(mean = 48.8, sd = 3.34),
                aes(color = "Chinstrap")) +
  stat_function(fun = dnorm,
                args = list(mean = 47.5, sd = 3.08),
                aes(color = "Gentoo")) +
  geom_vline(xintercept = 50, 
             linetype = "dashed")

# now we can calculate the likelihood of each penguin w/a 50mm bill
# L(y = A | x = 50)
dnorm(50, mean = 38.8, sd = 2.66)

# L(y = C | x = 50)
dnorm(50, mean = 48.8, sd = 3.34)

# L(y = G | x = 50)
dnorm(50, mean = 47.5, sd = 3.08)

# with all this, what is the *marginal pdf* of a 50mm bill penguin?
marginal_pdf <- 
  (151/342) * 0.0000212 + (68/342) * 0.112 + (123/342) * 0.09317

marginal_pdf

# now, what's the posterior probability of each penguin given a bill of 50mm?
# (y = A | x = 50) = 
((151/342)*0.0000212)/marginal_pdf

# (y = C | x = 50) =
((68/342)*0.112)/marginal_pdf

# (y = G | x = 50) =
((123/342)*0.09317)/marginal_pdf

# posterior probability is highest for a gentoo !
# even though the likelihood is less than that of a chinstrap
# because gentoo are more common than chinstrap

# 14.1.3 - Two predictors

# what about flipper & bill length?
penguins %>%
  select(species, 
         bill_length_mm,
         flipper_length_mm) %>%
  pivot_longer(ends_with("mm"),
               names_to = "measurement") %>%
  drop_na() %>%
  ggplot(aes(x = value,
             fill = species)) +
  geom_density(alpha = 0.25) +
  facet_wrap(~measurement, 
             scales = "free",
             ncol = 1)

# overlap is much smaller when we consider both distributions
penguins %>%
  ggplot(aes(x = flipper_length_mm,
             y = bill_length_mm,
             color = species)) +
  geom_point()

# another naive assumption: predictors are conditionally independent
# i.e., length of a penguin bill has no relationship to the length of the flipper
# this may make the model *wrong* (in a sense)
# e.g., within each species, there appears to be a positive correlation
#       between bill/flipper length!

# sample mean & sd for each 
penguins %>%
  group_by(species) %>%
  summarise(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm = TRUE))

# what are the likelihoods based on a 195mm flipper length?
# L(y = A | x = 195)
dnorm(195, mean = 190, sd = 6.54)

# L(y = C | x = 195)
dnorm(195, mean = 196, sd = 7.13)

# L(y = G | x = 195)
dnorm(195, mean = 217, sd = 6.48)

# we can combine the likelihoods based on a 195mm flipper/50mm bill length
# L(y = A | bill = 50, flipper = 195) =
151/342 * 0.0000212 * 0.04554

# L(y = C | bill = 50, flipper = 195) = 
68/342 * 0.112 * 0.05541

# L(y = G | bill = 50, flipper = 195) = 
123/342 * 0.09317 * 0.0001934

# adding together gives the marginal probability of 
marginal_pdf2 <- 151/342 * 0.0000212 * 0.04554 + 68/342 * 0.112 * 0.05541 + 123/342 * 0.09317 * 0.0001934

# plugging into bayes rule gives posterior probabilities:
# p(y = A | data) = 
(151/342 * 0.0000212 * 0.04554)/marginal_pdf2

# p(y = C | data) =
(68/342 * 0.112 * 0.05541)/marginal_pdf2 # ~ 99.4%

# p(y = G | data) = 
(123/342 * 0.09317 * 0.0001934)/marginal_pdf2

# 14.2 - Implementing & evaluating naive Bayes classification

# let's not do this by hand...
naive_model_1 <- 
  naiveBayes(
    species ~ bill_length_mm, 
    data = penguins
  )

naive_model_2 <-
  naiveBayes(
    species ~ bill_length_mm + flipper_length_mm,
    data = penguins
  )

# double check work from above
our_penguin <- tibble(bill_length_mm = 50, flipper_length_mm = 195)

naive_model_1 %>%
  predict(newdata = our_penguin, type = "raw")

naive_model_2 %>%
  predict(newdata = our_penguin, type = "raw")

# let's apply this to all the pengoons
penguins <- 
  penguins %>%
  mutate(class_1 = predict(naive_model_1, newdata = .),
         class_2 = predict(naive_model_2, newdata = .)) 

# conf matrix for naive_model_1
# misclassifies a lot of chinstrap as gentoo
penguins %>%
  yardstick::conf_mat(truth = species, estimate = class_1)

# conf matrix for naive_model_2
penguins %>%
  yardstick::conf_mat(truth = species, estimate = class_2)

# let's test this using cross validation
set.seed(84735)
cv_model_2 <-
  naive_classification_summary_cv(
    model = naive_model_2, 
    data = penguins,
    y = "species",
    k = 10
  )

cv_model_2$cv

